

library(fitdistrplus)
fit1 <- fitdist(fi_peak %>% filter(age1 >0) %>% pull(age1),"lnorm")
fit2 <- fitdist(se_peak %>% filter(age1 >0) %>% pull(age1),"lnorm")

x_vals <- seq(0, 7, le = 512)


fit_1p <- data.frame(peak = "1st",age = x_vals, density = dlnorm(x_vals,
                                                       meanlog = fit1$estimate[1],
                                                       sdlog = fit1$estimate[2]))

fit_2p <- data.frame(peak = "2nd",age = x_vals, density = dlnorm(x_vals,
                                                                 meanlog = fit2$estimate[1],
                                                                 sdlog = fit2$estimate[2]))

data %>%
  ggplot(aes(age))+
  geom_histogram(binwidth = 0.5,
                 color = "white",
                 fill = "black",
                 alpha = .5)+
  geom_line(data = rbind(fit_1p,fit_2p),aes(x = age,y = density*10000))+
  facet_wrap(~peak)+
  scale_x_continuous(breaks = seq(0,7,by=.5),limits = c(0,7))+
  theme_minimal()+
  scale_y_continuous(
    name = "Cases count",
    sec.axis = sec_axis( trans=~./10000, name="Density")
  )

library(nortest)
fit1$loglik
ks.test(dlnorm(x_vals,
               meanlog = fit1$estimate[1],
               sdlog = fit1$estimate[2]),
        dlnorm(x_vals,
               meanlog = fit2$estimate[1],
               sdlog = fit2$estimate[2]),
        altervative = "two.sided")


df_fit_logn <- df1 %>% filter(year(adm_date) == "2023") %>%
  filter(!is.na(adm_date) & !is.na(age1)) %>%
  select(adm_date,age1) %>%
  mutate(adm_date2 = as.numeric(adm_date)) %>%
  arrange(adm_date2)

fit.lognormal(data$age, k=2,normal=FALSE) %>% str()

cut_candidates <- df_fit_logn %>%
  pull(adm_date) %>%
  unique()

fit_lognorm <- function(x) {
  x <- x[x > 0]
  if (length(x) < 30) return(NULL)
  fit <- tryCatch(
    fitdist(x, "lnorm"),
    error = function(e) NULL
  )
  fit
}

compute_diff <- function(cut_date) {
  group1 <- df_fit_logn$age1[df_fit_logn$adm_date <= cut_date]
  group2 <- df_fit_logn$age1[df_fit_logn$adm_date >  cut_date]

  if (length(group1) < 30 || length(group2) < 30) return(NA)

  # KS statistic (measure of difference)
  ks <- suppressWarnings(ks.test(group1, group2))
  ks$statistic
}

# Scan all possible cutoffs
ks_scores <- map_dbl(cut_candidates, compute_diff)

# Find best cutoff
best_cut_date <- cut_candidates[which.max(ks_scores %>% na.omit())]
best_cut_date
library(fitdistrplus)
fit_lognorm(fi_peak$age1)
fit1 <- fit_lognorm(df_fit_logn$age1[df_fit_logn$adm_date <= best_cut_date])
fit2 <- fit_lognorm(df_fit_logn$age1[df_fit_logn$adm_date > best_cut_date])

