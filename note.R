library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(magrittr)
library(mgcv)
library(tidyverse)
library(patchwork)
Sys.setlocale("LC_TIME", "English")
### data
apr_2023 <- read_excel("D:/OUCRU/hfmd/data/4_2023.xlsx")
aug_2023 <- read_excel("D:/OUCRU/hfmd/data/08_2023.xlsx")
dec_2022 <- read_excel("D:/OUCRU/hfmd/data/12_2022.xls")
dec_2023 <- read_excel("D:/OUCRU/hfmd/data/12_2023.xlsx")

t423 <- data.frame(apr_2023[-c(1,2),10:14])
t423$pos <- replace(t423$...14,is.na(t423$...14),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t423) <- c("age","col_day","col_month","col_year","neutralization","pos")
t423$age <- as.numeric(t423$age)


t823 <- data.frame(aug_2023[-c(1,2),c(9,14:17)])
t823$pos <- str_detect(t823$...17,regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t823) <- c("age","col_day","col_month","col_year","neutralization","pos")
t823$age <- as.numeric(t823$age)


t1222 <- data.frame(dec_2022[-c(1,2),10:14])
t1222$pos <- replace(t1222$...14,is.na(t1222$...14),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t1222) <- c("age","col_day","col_month","col_year","neutralization","pos")
t1222$age <- as.numeric(t1222$age)


t1223 <- data.frame(dec_2023[-c(1,2),c(9,14:17)])
t1223$pos <- replace(t1223$...17,is.na(t1223$...17),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t1223) <- c("age","col_day","col_month","col_year","neutralization","pos")
t1223$age <- as.numeric(t1223$age)




polygon2 <- function(x, y1, y2, ...) {
  polygon(c(x, rev(x)), c(y1, rev(y2)), ...)
}

lrt <- function(...) anova(..., test = "LRT")


points2 <- function(...) points(..., pch = "|", cex = .5)


gam2 <- function(formula, family = gaussian(), data = list(), ...){
  out <- mgcv::gam(formula, family, data, ...)
  out$data <- data
  out
}


quality <- function(x) {
  tibble(deviance = deviance(x),
         AIC      = AIC(x),
         GCV      = x$gcv.ubre)
}



add_smooth_col <- function(x, s) {
  bind_cols(tibble(smooth = s), x)
}


p_values <- function(x) {
  tibble(year     = x$p.table[2, "Pr(>|z|)"],
         "s(age)" = x$s.table[1, "p-value"])
}

predict2 <- function(x, ci = .95, le = 512, m = 100) {
  p <- (1 - ci) / 2

  link_inv <- x$family$linkinv
  dataset <- x$data
  n <- nrow(dataset) - length(x$coefficients)
  age_range <- range(dataset$age)

  ages <- seq(age_range[1], age_range[2], le = le)

  x |>
    predict(data.frame(age = ages), se.fit = TRUE) |>
    extract(c("fit", "se.fit")) %>%
    c(age = list(ages), .) |>
    as_tibble() |>
    mutate(lwr = m * link_inv(fit + qt(    p, n) * se.fit),
           upr = m * link_inv(fit + qt(1 - p, n) * se.fit),
           fit = m * link_inv(fit)) |>
    select(- se.fit)
}

plot_predictions <- function(x, add = FALSE, col = 4, alpha = .2, lwd = 2,
                             m = 100) {
  with(x, {
    if (! add) {
      plot(NA, xlim = c(0, max(age)), ylim = c(0, m),
           xlab = "age (year)", ylab = "seroprevalence (%)")
    }
    polygon2(age, lwr, upr, border = NA, col = adjustcolor(col, alpha))
    lines(age, fit, col = col, lwd = lwd)
  })
}



make_formula <- function(degree) {
  2:degree |>
    map_chr(~ paste("I(age ^", .x, ")")) |>
    paste(collapse = " + ") %>%
    paste("pos ~ age +", .) |>
    as.formula()
}

test_degrees <- function(data, degree) {
  degree |>
    make_formula() |>
    glm(binomial, data = data) |>
    lrt()
}

test_degrees(t1222,5)
test_degrees(t1223,5)
test_degrees(t423,5)
test_degrees(t823,5)

m <- 100
eps <- 1

make_formula(5)

glm(pos ~ age, binomial, data = t1222) |>
  predict2() |>
  plot_predictions(col = 1)

glm(pos ~ age + I(age ^2), binomial, data = t423) |>
  predict2() |>
  plot_predictions(add = T, col = 2)

glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t823) |>
  predict2() |>
  plot_predictions(add = TRUE, col = 3)

glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t1223) |>
  predict2() |>
  plot_predictions(add = F, col = 4)

data = t1222 |>
  with(points2(age, m * pos + eps, col = 1))

data = t423 |>
  with(points2(age, m * pos + 2*eps, col = 2))

data = t823 |>
  with(points2(age, m * pos - eps, col = 3))

data = t1223 |>
  with(points2(age, m * pos - 2*eps, col = 4))

legend("topleft", legend = c("Dec 2022",
                             "Apr 2023",
                             "Aug 2023",
                             "Dec 2023"),
       lty = 1, lwd = 2,
       col = c(1,2,3,4), bty = "n",
       y.intersp = .25)


legend("topleft", legend = c(
  "Dec 2022"),
  lty = 1, lwd = 2,
  col = 4, bty = "n",
  y.intersp = .25)

data = t1222


plot1222 <-  glm(pos ~ age, binomial, data = t1222) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2022"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5)+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2022" = "#0808cf"))+
  labs(y = "Seroprevalence(%)")+
  geom_point(data= t1222, aes(x = age,y = m * pos + eps),shape = "|",size = 3,
             col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title= element_blank(),
    legend.position = "inside",
    legend.position.inside =  c(0.15,0.80),
    legend.text = element_text(size = 15))


plot0423 <-  glm(pos ~ age + I(age ^2), binomial, data = t423) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Apr 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = 2)+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Apr 2023" = 2))+
  labs(y = "Seroprevalence(%)")+
  geom_point(data= t423, aes(x = age, m * pos + eps),
             shape = "|",size = 3,
             col = 2)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title= element_blank(),
    legend.position = "inside",
    legend.position.inside =  c(0.15,0.80),
    legend.text = element_text(size = 15))

plot0823 <-  glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t823) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Aug 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = 3)+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Aug 2023" = 3))+
  labs(y = "Seroprevalence(%)")+
  geom_point(data= t823, aes(x = age, m * pos + eps),
             shape = "|",size = 3,
             col = 3)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title= element_blank(),
    legend.position = "inside",
    legend.position.inside =  c(0.15,0.80),
    legend.text = element_text(size = 15))

plot1223 <-  glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t1223) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = 4)+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2023" = 4))+
  labs(y = "Seroprevalence(%)")+
  geom_point(data= t1223, aes(x = age, m * pos + eps),
             shape = "|",size = 3,
             col = 4)+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title= element_blank(),
    legend.position = "inside",
    legend.position.inside =  c(0.15,0.80),
    legend.text = element_text(size = 15))

result_sero <- (plot1222 + plot0423) /
  (plot0823 + plot1223)

result_sero


###### ts in attackrate.R
## ts in another file :))

# ts2 <- ts +
#   geom_vline(xintercept = as.Date("2022-12-01"),
#              alpha = 0.4,col = "#0808cf")+
#   geom_vline(xintercept = as.Date("2022-12-30"),
#              alpha = 0.4,col = "#0808cf")+
#   geom_vline(xintercept = as.Date("2023-04-01"),
#              alpha = 0.4,col = "#ed097b")+
#   geom_vline(xintercept = as.Date("2023-04-30"),
#              alpha = 0.4,col = "#ed097b")+
#   geom_vline(xintercept = as.Date("2023-08-01"),
#              alpha = 0.4,col = "#ed6b00")+
#   geom_vline(xintercept = as.Date("2023-08-30"),
#              alpha = 0.4,col = "#ed6b00")+
#   geom_vline(xintercept = as.Date("2023-12-01"),
#              alpha = 0.4,col = "#33516b")+
#   geom_vline(xintercept = as.Date("2023-12-30"),
#              alpha = 0.4,col = "#33516b")+
#   xlim(as.Date("2022-11-24"),as.Date("2024-01-01"))


plot1222 <-  glm(pos ~ age, binomial, data = t1222) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2022"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#0808cf")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2022" = "#0808cf"))+
  labs(y = "Seroprevalence (%)")+
  geom_point(data= t1222, aes(x = age,y = m * pos + eps),shape = "|",
             col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Dec 2022"),size = 6)

plot0423 <-  glm(pos ~ age + I(age ^2), binomial, data = t423) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Apr 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#ed097b")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Apr 2023" = "#ed097b"))+
  labs(y = "Seroprevalence (%)")+
  geom_point(data= t423, aes(x = age, m * pos + eps),
             shape = "|",
             col = "#ed097b")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Apr 2023"),size = 6)

plot0823 <-  glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t823) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Aug 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#ed6b00")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Aug 2023" = "#ed6b00"))+
  labs(y = "Seroprevalence (%)")+
  geom_point(data= t823, aes(x = age, m * pos + eps),
             shape = "|",
             col = "#ed6b00")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Aug 2023"),size = 6)

plot1223 <-  glm(pos ~ age + I(age^2) + I(age^3), binomial, data = t1223) |>
  predict2() %>% as.data.frame() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#33516b")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2023" = "#33516b"))+
  labs(y = "Seroprevalence (%)")+
  geom_point(data= t1223, aes(x = age, m * pos + eps),
             shape = "|",
             col = "#33516b")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Dec 2023"),size = 6)

result_sero <- plot1222 | plot0423 | plot0823 | plot1223

result_sero/
  ts2


ggsave("D:/OUCRU/hfmd/figure/journal club presentation/sero.svg",
       width = 17,height = 7)

ggsave("D:/OUCRU/hfmd/figure/journal club presentation/atk19.svg",
       width = 4,height = 10)


## model age and time at the same time

library(scam)
library(mgcv)
library(plotly)

atdf <- rbind(t1222,t423,t823,t1223) %>%
  mutate(col_date = make_date(year = col_year,
                              month = col_month,
                              day = col_day)) %>%
  select(age,col_date,pos)

colnames(atdf)


g1 <- gam(pos~s(age)+col_date,
          family=binomial,method = "REML",atdf)

age_val <- c(.1, 1:14)
collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date), le = 20)

new_data <- expand.grid(age = age_val,
                        col_date = collection_date_val)


gamf <- cbind(new_data, fit = 100 * predict.gam(g1, new_data,"response"))
colnames(gamf)

plot_ly(gamf, x = ~sort(unique(col_date)),
        y = ~sort(unique(age)),
        z = ~matrix(fit, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

s1 <- scam(pos~s(age)+col_date,family=binomial,atdf)

atdf2 <- atdf
atdf2$col_date <- as.numeric(atdf2$col_date)

s1 <- scam(pos~s(age,bs = "mpi")+s(col_date),family=binomial,
           mutate(atdf2, across(col_date, as.numeric)))


age_val <- c(.1, 1:14)
collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date), le = 20)

new_data <- expand.grid(age = age_val,
                        col_date = as.numeric(collection_date_val))


scamf <- cbind(new_data, fit = 100 * predict(s1, new_data,"response"))

plot_ly(scamf, x = ~sort(unique(as.Date(col_date))),
        y = ~sort(unique(age)),
        z = ~matrix(fit, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

## glm

mod1 <- glm(pos ~ age * col_date +
              I(age ^ 2) * col_date, binomial,
            mutate(atdf, across(col_date, as.numeric)))

dataset <- mod1$data

collection_date_val <- seq(min(dataset$col_date),
                           max(dataset$col_date),le=20)

new_data <- expand.grid(age = age_val, col_date = collection_date_val)


prdcts <- cbind(new_data, fit = 100 * predict(mod1, new_data, "response")) |>
  as_tibble() |>
  arrange(col_date) |>
  mutate(across(col_date, as_date))

plot_ly(prdcts, x = ~sort(unique(col_date)),
        y = ~sort(unique(age)),
        z = ~matrix(fit, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
    ))

###

scamf$col_date <- as.Date(scamf$col_date)

scamf$age2 <- factor(scamf$age,
                     labels=c('(0,1)','[1,2)','[2,3)','[3,4)','[4,5)',
                              '[5,6)','[6,7)','[7,8)','[8,9)','[9,10)',
                              '[10,11)','[11,12)','[12,13)','[13,14)','14+'))

scamf %>% ggplot(aes(x = col_date,y = fit,colour = age2)) +
  geom_line()+
  ylim(0,100)+
  theme_classic()+
  labs(x = "Collection date", y = "Seroprevalence (%)",colour = "Age group")+
  guides(fill=guide_legend(ncol=2))

s1 <- scam(pos~s(age)+s(col_date,bs = "mpi"),family=binomial,
           mutate(atdf, across(col_date, as.numeric)))

age_val <- c(.1,1:14)

collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date), le = 512)

new_data <- expand.grid(age = age_val,
                        col_date = as.numeric(collection_date_val))

scamf <- cbind(new_data,
               fit = 100 * predict(s1, new_data,"response",se.fit = "TRUE")$fit,
               se = 100 * predict(s1, new_data,"response",se.fit = "TRUE")$se.fit)
scamf <- cbind(new_data,
               fit = 100 * predict(s1, new_data,"response"))

scamf$col_date <- as.Date(scamf$col_date)


plot_ly(scamf, x = ~sort(unique(as.Date(col_date))),
        y = ~sort(unique(age)),
        z = ~matrix(fit, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

##

predict2 <- function(x, ci = .95, le = 512, m = 100){
  p <- (1 - ci) / 2
  link_inv <- x$family$linkinv
  dataset <- x$model
  n <- nrow(dataset) - length(x$coefficients)
  age_range <- range(dataset$age)
  ages <- seq(age_range[1], age_range[2], le = le)
  date_range <- range(dataset$col_date)
  dates <- seq(date_range[1], date_range[2], le = le)

out <- x |>
  predict(expand.grid(age = ages,
                        col_date = dates), se.fit = TRUE) |>
  extract(c("fit", "se.fit")) %>%
  c(expand.grid(age = ages,
               date = dates),
    .) |>
  as_tibble() |>
  mutate(lwr = m * link_inv(fit + qt(    p, n) * se.fit),
           upr = m * link_inv(fit + qt(1 - p, n) * se.fit),
           fit = m * link_inv(fit)) |>
  select(-se.fit)

return(out)
}

out <- predict2(s2)

out$date <- as.Date(out$date)

s1222 <- out %>% filter(month(date) == 12 & year(date) == 2022) %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2022"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#0808cf")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2022" = "#0808cf"))+
  labs(y = "Seroprevalence (%)")+
  # geom_point(data= t1222, aes(x = age, m * pos + 1),
  #            shape = "|",
  #            col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Dec 2022"),size = 6)

s423 <- out %>% filter(month(date) == 4 & year(date) == 2023) %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Apr 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#ed097b")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Apr 2023" = "#ed097b"))+
  labs(y = "Seroprevalence (%)")+
  # geom_point(data= t1222, aes(x = age, m * pos + 1),
  #            shape = "|",
  #            col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Apr 2023"),size = 6)

s823 <- out %>% filter(month(date) == 8 & year(date) == 2023) %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Aug 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#ed6b00")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Aug 2023" = "#ed6b00"))+
  labs(y = "Seroprevalence (%)")+
  # geom_point(data= t1222, aes(x = age, m * pos + 1),
  #            shape = "|",
  #            col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Aug 2023"),size = 6)

s1223 <- out %>% filter(month(date) == 12 & year(date) == 2023) %>%
  ggplot(aes(x = age,y = fit))+
  geom_line(aes(col = "Dec 2023"))+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5,fill = "#33516b")+
  ylim(0,101)+
  theme_minimal()+
  scale_color_manual(name = "Y series",
                     values = c("Dec 2023" = "#33516b"))+
  labs(y = "Seroprevalence (%)")+
  # geom_point(data= t1222, aes(x = age, m * pos + 1),
  #            shape = "|",
  #            col = "#0808cf")+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.position = "hide",
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))+
  annotate("text", x = 3, y = 90, label = c("Dec 2023"),size = 6)

s1222 + s423 + s823 + s1223

##

df1 <- read_excel("D:/OUCRU/hfmd/data/TCM_full.xlsx",
                  col_types = c("date", "numeric", "text",
                                "text", "text", "date", "date", "date",
                                "text", "text", "text"))
colnames(df1) <- c("dob", "age", "gender", "commune", "district",
                   "reported_date", "onset_date","adm_date",
                   "medi_cen","inout","severity")
df1$dob <- df1$dob %>% as_date()
df1$adm_date <- df1$adm_date %>% as_date()

df1$age1 <- interval(df1$dob, df1$adm_date) / years(1)
df1$adm_week <- as.Date(floor_date(df1$adm_date, "week"))
df1$district <- df1$district %>% str_replace_all(
  c( "Quận Gò vấp"  = "Quận Gò Vấp"))
df1$district <- df1$district %>%
  str_remove("Quận|Huyện|Thành phố") %>%
  trimws(which = "both")

df_plot <- df1 %>% filter(year(adm_week) == "2023") %>%
  filter(!is.na(adm_week) ) %>%
  count(adm_week) %>% as.data.frame()

ts <- ggplot()+
  geom_bar(data = df_plot, aes(x = as.Date(adm_week), y = n),stat = "identity",
           alpha = 0.5) +
  labs(x = "Admission week","Cases")+
  geom_vline(xintercept = as.Date("2022-12-01"),
             alpha = 0.4,col = "#0808cf")+
  geom_vline(xintercept = as.Date("2022-12-30"),
             alpha = 0.4,col = "#0808cf")+
  geom_vline(xintercept = as.Date("2023-04-01"),
             alpha = 0.4,col = "#ed097b")+
  geom_vline(xintercept = as.Date("2023-04-30"),
             alpha = 0.4,col = "#ed097b")+
  geom_vline(xintercept = as.Date("2023-08-01"),
             alpha = 0.4,col = "#ed6b00")+
  geom_vline(xintercept = as.Date("2023-08-30"),
             alpha = 0.4,col = "#ed6b00")+
  geom_vline(xintercept = as.Date("2023-12-01"),
             alpha = 0.4,col = "#33516b")+
  geom_vline(xintercept = as.Date("2023-12-30"),
             alpha = 0.4,col = "#33516b")+
  xlim(as.Date("2022-11-24"),as.Date("2024-01-01"))+theme_classic()

result_sero <- s1222 | s423 | s823 | s1223

result_sero/
  ts


plot_ly(data.frame(out),
        x = ~date,
        y = ~age,
        z = ~fit,
        type = "mesh3d") %>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

### test another hypothesis


t1223a <- t1223 %>%
  mutate(col_date = make_date(year = col_year,
                                          month = col_month,
                                          day = col_day))
t1222a <-  t1222 %>%
  mutate(col_date = make_date(year = col_year,
                              month = col_month,
                              day = col_day))
t423a <- t423 %>%
  mutate(col_date = make_date(year = col_year,
                              month = col_month,
                              day = col_day))
t823a <- t823 %>%
  mutate(col_date = make_date(year = col_year,
                              month = col_month,
                              day = col_day))


sw <- t1223a$col_date
t1223a$col_date <- t1222a$col_date
t1222a$col_date <- sw



atdfa <- rbind(t1223a,t423a,t823a,t1222a)

library(scam)
library(plotly)
library(mgcv)

sa <- scam(pos~s(age)+s(col_date,bs= "mpi"),family=binomial,
           mutate(atdf, across(col_date, as.numeric)))

age_val <- c(.1,1:14)

collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date), le = 512)

new_data <- expand.grid(age = age_val,
                        col_date = as.numeric(collection_date_val))

scamf <- cbind(new_data,
               fit = 100 * predict(sa, new_data,"response"))

scamf$col_date <- as.Date(scamf$col_date)

plot_ly(scamf, x = ~sort(unique(as.Date(col_date))),
        y = ~sort(unique(age)),
        z = ~matrix(fit, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

# %>% layout(autosize = F, width = 900, height = 1000, margin = m)

adf <- additive.mpspline.fitter(response=atdf$pos,x.var=atdf$age,z.var=as.numeric(atdf$col_date),ps.intervals=20,degree=3,order=2,link="logit",
                         family="binomial",alpha=2,kappa=1e8)

str(adf)

adf$yhat %>% View()
adf$x






