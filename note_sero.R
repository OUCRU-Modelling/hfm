lapply(df.list, function(x)
  gam(pos~s(age,bs = "bs"),method = "REML",
      family = "binomial",data = x) |>
    predictg() %>% as.data.frame()
) %>%
  bind_rows(.id = "label")


gam(pos~s(col_date,bs = "bs"),method = "REML",family = "binomial",
    data = sero %>%
      filter(age_gr2 == "<0 & ≤3 years") %>%
      mutate(across(col_date, as.numeric)))$fitted.values


ggplot_build(sp_agr)$data[[2]] %>% mutate(across(x, as.Date)) %>%
  filter(PANEL == 1) %>% select(x,y,ymin,ymax)  %>%
  mutate(y = y*100 ,
         ymin = ymin*100,
         ymax= ymax*100)

df.list %>% bind_rows(.id = "label") %>%
  ggplot(aes(x = age, y = pos)) +
  # geom_jitter(height = 0.05)+
  geom_point(aes(x = age, pos),
             shape = "|")+
  facet_wrap(~factor(col_time,levels = c("Dec 2022","Apr 2023",
                                         "Aug 2023","Dec 2023")),
             ncol = 4)+
  geom_smooth(fill = "blue",alpha = 1/10,
              method = mgcv::gam,formula = y ~ s(x, bs = "bs"),
              method.args = list(method = "REML",link = "logit",
                                 family = "binomial"))+
  labs(y = "Seroprevalence (%)",x = "Age",tag = "A")+
  scale_x_continuous(breaks = seq(0,15,by = 3), minor_breaks = NULL)+
  scale_y_continuous(labels = scales::label_percent(), minor_breaks = NULL)+
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))


df23 %>%
  select(adm_week,age_gr2)%>%
  count(age_gr2,adm_week) %>%
  left_join(pop_agegr,by = join_by(age_gr2 == age3)) %>%
  group_by(age_gr2) %>%
  mutate(pop = n.y - cumsum(lag(n.x, default = 0)),
         atr = cumsum(n.x)/pop)  %>%
  select(-n.y) %>%
  mutate(
    attack_rate_ma = slide_dbl(
      .x      = atr,
      .f      = mean,         # function to compute
      .before = 4,            # 4 weeks before = 5-week window total
      .after  = 0,
      .step = 1,
      .complete = TRUE        # only compute for full windows
    )
  ) %>%
  ungroup() %>% na.omit() %>% filter(age_gr2 != "16+") %>%
  ggplot(aes(x = adm_week,y=log(attack_rate_ma*100000))) +
  # geom_bar(stat = "identity")+
  geom_line()+
  facet_wrap(vars(age_gr2),ncol = 5)+
  scale_x_date(breaks = seq(as.Date("2022-12-01"),as.Date("2023-12-31"), le = 4),
               date_labels = "%b %Y", minor_breaks = NULL)+
  scale_y_continuous(minor_breaks = NULL)+
  # scale_x_date(date_breaks = "4 months",date_labels = "%b %Y")+
  coord_cartesian(ylim = c(-2.5,9.5),xlim = as.Date(c("2022-12-01","2023-12-31")))+
  theme_bw()+
  labs(x = "Admission week",y= "Log attack rate",tag = "A")+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))

lapply(df.list, function(x)
  gam(pos~s(age,bs = "bs", k = 40),method = "REML",
      family = "binomial",data = x) |>
    predictg() %>% as.data.frame()
) %>%
  bind_rows(.id = "label") %>%
  mutate(col_date = case_when(
    label == 1 ~ "Dec 2022",
    label == 2 ~ "Apr 2023",
    label == 3 ~ "Aug 2023",
    label == 4 ~ "Dec 2023"
  ))%>%
  ggplot(aes(x = age,y = fit,
             fill = factor(col_date,levels = c("Dec 2022","Apr 2023",
                                               "Aug 2023","Dec 2023"))))+
  geom_line()+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5)+
  scale_fill_paletteer_d("nbapalettes::thunder")+
  ylim(0,101)+
  labs(y = "Seroprevalence (%)",x = "Age",tag = "Manually")+
  scale_x_continuous(breaks = seq(0,15,by = 3))+
  theme_bw()+
  facet_wrap(~factor(col_date,levels = c("Dec 2022","Apr 2023",
                                         "Aug 2023","Dec 2023")),
             ncol = 4)+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))


deg <- 3

xs <- c(3,6, 9)

xd <- c(0, xs, 14.96438)

## clamped auxiliary boundary knots
left.aux <- rep(0, deg)

right.aux <- rep(14.96438, deg)

my.knots <- c(left.aux, xd, right.aux)

my.k <- length(xs) + deg + 1

# ttt <- lapply(df.list, function(x)
#   gam(pos~s(age,bs = "bs", k = my.k),method = "REML",
#       family = "binomial",data = x,
#       knots = list(x = my.knots))
# )

ttt <- gam(pos~s(age,bs = 'bs', k = my.k),method = "REML",
    family = "binomial",data = t1222,
    knots = list(x = my.knots))

# t1222$pos <- as.numeric(t1222$pos)
# class(t1222$pos)
ttt$smooth[[1]]$knots

library(gps.mgcv)

x <- t823$age
y <- t823$pos

deg <- 3

a <- min(x)
b <- max(x)

xs <- c(2,4,6,8,10,12)
xd <- c(a, xs, b)
my.k <- length(xs) + deg + 1
left.aux <- rep(a, deg)
right.aux <- rep(b, deg)

my.knots <- c(left.aux, xd, right.aux)



gpsfit <- gam(y ~ s(x, bs = 'bs', k = my.k),
              method = "REML",
              family = "binomial",
              knots = list(x = my.knots))

data.frame(x = x, y = gpsfit$fitted.values) %>%
ggplot(aes(x = x,y = y))+
  geom_line()+
  ylim(0,1)

gpsfit$smooth[[1]]$knots
min(t1222$age)
t1222$age

lapply(df.list, function(x)
  # gam(pos~s(age,bs = "bs", k = 40),method = "REML",
  #     family = "binomial",data = x)
  gam(pos ~ s(age, bs = 'gps', k = my.k),method = "REML",
      family = "binomial",data = x,
      knots = list(x = xs)) |>
    predictg() %>% as.data.frame()
) %>%
  bind_rows(.id = "label") %>%
  mutate(col_date = case_when(
    label == 1 ~ "Dec 2022",
    label == 2 ~ "Apr 2023",
    label == 3 ~ "Aug 2023",
    label == 4 ~ "Dec 2023"
  ))%>%
  ggplot(aes(x = age,y = fit,
             fill = factor(col_date,levels = c("Dec 2022","Apr 2023",
                                               "Aug 2023","Dec 2023"))))+
  geom_line()+
  geom_ribbon(aes(x = age,y = fit,
                  ymin=lwr, ymax=upr),alpha = 0.5)+
  scale_fill_paletteer_d("nbapalettes::thunder")+
  ylim(0,101)+
  labs(y = "Seroprevalence (%)",x = "Age",tag = "Manually")+
  scale_x_continuous(breaks = seq(0,15,by = 3))+
  theme_bw()+
  facet_wrap(~factor(col_date,levels = c("Dec 2022","Apr 2023",
                                         "Aug 2023","Dec 2023")),
             ncol = 4)+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))




knots = list(x = xs)  ## provide interior knots

x <- c(20.70, 20.44, 20.58, 21.02, 19.90,  6.20,  8.20,  6.92,  5.86,  6.44,  6.34,  8.48,  8.46,  9.00,  9.06,  9.00,  9.06, 17.98, 18.42, 19.18, 22.88, 24.16,20.20, 23.50)

y <- c(rep(c(0,1),12))


data = data.frame(x,y)
## degree of spline
deg <- 3

## domain
a <- min(x)
#[1] 5.86
b <- max(x)
#[1] 24.16

## interior knots (must be between a and b)
xs <- c(6.5, 20.5)
#[1]  6 20

## domain knots
xd <- c(a, xs, b)
#[1]  5.86  6.00 20.00 24.16

## clamped auxiliary boundary knots
left.aux <- rep(a, deg)
#[1] 5.86 5.86 5.86
right.aux <- rep(b, deg)
#[1] 24.16 24.16 24.16

## complete B-spline knots
my.knots <- c(left.aux, xd, right.aux)
my.k <- length(xs) + deg + 1

myfit <- gam(y ~ s(x, bs = 'bs', k = my.k),
             method = "REML",family = "binomial",
             knots = list(x = my.knots))

myfit$smooth[[1]]$knots

#### fig 2

ggplot(sero,
       aes(x = col_date, y = pos)) +
  # geom_jitter(height = 0.05)+
  geom_point(aes(x = col_date, pos),
             shape = "|")+
  facet_wrap(~age_gr2,
             ncol = 5)+
  geom_smooth(fill = "blue",alpha = 1/10,
              method = mgcv::gam,formula = y ~ s(x, bs = "bs"),
              method.args = list(method = "REML",link = "logit",
                                 family = "binomial"))+
  labs(x = "Collection date", y  = "Seroprevalence (%)",tag = "B")+
  scale_x_date(breaks = seq(as.Date("2022-12-01"),as.Date("2023-12-31"), le = 4),
               date_labels = "%b %y", minor_breaks = NULL)+
  scale_y_continuous(labels = scales::label_percent(), minor_breaks = NULL)+
  # scale_x_date(date_breaks = "4 months",date_labels = "%b %Y")+
  coord_cartesian(ylim = c(0, 1),xlim = as.Date(c("2022-12-01","2023-12-31")))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 12,vjust = 0.5,
                                   hjust = 0.5,angle = 45),
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))


te_sp <- sero %>%
  group_by(age_gr2,col_time) %>%
  count(pos) %>%
  pivot_wider(names_from = pos, values_from = n) %>%
  ungroup() %>%
  mutate(tot = `0`+`1`,
         sp = `1`/(`0`+`1`)) %>%
  as.data.frame()

for (i in 1:nrow(te_sp)) {
  te_sp$lwr[i] <- prop.test(te_sp$`1`[i],te_sp$tot[i],correct=TRUE)$conf.int[1]
  te_sp$upr[i] <- prop.test(te_sp$`1`[i],te_sp$tot[i],correct=TRUE)$conf.int[2]
}

prop.test(mm$sp$pos[i],mm$sp$tot[i],correct=TRUE)$conf.int[1]*100

col_ddd<- sero %>%
  group_by(age_gr2,col_time) %>%
  summarise(mean = mean(col_date)) %>%
  ungroup()

te_sp$col_date <- col_ddd$mean


class(te_sp$col_date)


atk_rate/
  sp_agr2

ggplot(sero,
       aes(x = col_date, y = pos)) +
  # geom_jitter(height = 0.05)+
  geom_point(aes(x = col_date, pos),
             shape = "|")+
  geom_smooth(fill = "blue",alpha = 1/10,
              method = mgcv::gam,formula = y ~ s(x, bs = "bs"),
              method.args = list(method = "REML",link = "logit",
                                 family = "binomial"))+
  geom_point(data = te_sp,
             aes(x  = col_date,y = sp))+
  geom_errorbar(data = te_sp,
                aes(x  = col_date,y = sp,ymin = lwr, ymax = upr),alpha = .5)+
  facet_wrap(~age_gr2,
             ncol = 5)+
  labs(x = "Collection date", y  = "Seroprevalence (%)",tag = "B")+
  scale_x_date(breaks = seq(as.Date("2022-12-01"),as.Date("2023-12-31"), le = 4),
               date_labels = "%b %y", minor_breaks = NULL)+
  scale_y_continuous(labels = scales::label_percent(), minor_breaks = NULL)+
  # scale_x_date(date_breaks = "4 months",date_labels = "%b %Y")+
  coord_cartesian(ylim = c(0, 1),xlim = as.Date(c("2022-12-01","2023-12-31")))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 12,vjust = 0.5,
                                   hjust = 0.5,angle = 45),
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))

##

atdf <- rbind(t1222,t423,t823,t1223) %>%
  mutate(col_date = make_date(year = col_year,
                              month = col_month,
                              day = col_day))

library(Iso)

atdf %>%
  group_by(age_gr, col_date) %>%
  summarise(
    n = n(),
    pos = sum(pos),
    seroprev = pos / n,
    .groups = "drop"
  ) %>%
  mutate(time_numeric = as.numeric(difftime(col_date, min(col_date), units = "days"))) %>%
  group_by(age_gr) %>%
  arrange(time_numeric) %>%
  mutate(
    seroprev_monotonic = pava(seroprev, time_numeric, decreasing = FALSE)
  ) %>% ggplot(aes(x = col_date)) +
  geom_point(aes(y = seroprev), color = "gray") +
  geom_line(aes(y = seroprev_monotonic), color = "blue") +
  facet_wrap(~age_gr) +
  labs(y = "Seroprevalence", x = "Date") +
  theme_minimal()


## scam

s1 <- scam(pos~s(age)+s(col_date,bs = "mpi"),family=binomial,
           mutate(atdf, across(col_date, as.numeric)))

age_val <- c(.1,1:14)

collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date), le = 512)

new_data <- expand.grid(age = age_val,
                        col_date = as.numeric(collection_date_val))

scamf <- cbind(new_data,
               fit = 100 * predict(s1, new_data,"response"))

scamf$col_date <- as.Date(scamf$col_date)

## scar package
library(scar)
atdf$col_date2 <- as.numeric(atdf$col_date)
x <- atdf[,c("age","col_date2")] %>% as.matrix()

out <- scar(x, atdf$pos, shape = rep("in",2),
     family = binomial())

pred <- predict(out, as.matrix(new_data), type = c("response"), rule=2)

scar <-cbind(new_data,
      pred)

scar$col_date <- as.Date(scar$col_date)

plot_ly(scar, x = ~sort(unique(as.Date(col_date))),
        y = ~sort(unique(age)),
        z = ~matrix(pred, 15),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,1))
  ))

plot(out$x,out$y)

mod1 <- glm(pos ~ age * col_date +
              I(age ^ 2) * col_date, binomial,
            mutate(atdf, across(col_date, as.numeric)))


age_val <- c(.1, seq(1,15,0.5))
collection_date_val <- seq(min(atdf$col_date),
                           max(atdf$col_date))

new_data <- expand.grid(age = age_val, col_date = as.numeric(collection_date_val))

prdcts <- cbind(new_data, fit = 100 * predict(mod1, new_data, "response")) |>
  as_tibble() |>
  arrange(col_date) |>
  mutate(across(col_date, as_date))

plot_ly(prdcts, x = ~sort(unique(as.Date(col_date))),
        y = ~sort(unique(age)),
        z = ~matrix(fit, length(age_val)),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

out <- prdcts %>%
  mutate(time_numeric = as.numeric(col_date)) %>%
  group_by(age) %>%
  arrange(time_numeric) %>%
  mutate(
    seroprev_monotonic = pava(fit, time_numeric, decreasing = FALSE)
  )

plot_ly(out,x = ~sort(unique(as.Date(col_date))),
          y = ~sort(unique(age)),
          z = ~matrix(seroprev_monotonic, length(age_val)),
          showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))


sp1222 <- out %>% filter(month(col_date) == 12 & year(col_date) == 2022) %>%
  group_by(age) %>%
  summarise(
    sp = mean(seroprev_monotonic),
  ) %>% ggplot(aes(x = age,y = sp))+
  geom_line()+
  ylim(c(0,100))+
  labs(tag = "Dec 2022",y = "Seroprevalence (%)")+
  theme_minimal()



sp0423 <-out %>% filter(month(col_date) == 04 & year(col_date) == 2023) %>%
  group_by(age) %>%
  summarise(
    sp = mean(seroprev_monotonic),
  ) %>% ggplot(aes(x = age,y = sp))+
  geom_line()+
  ylim(c(0,100))+
  labs(tag = "Apr 2023",y = "Seroprevalence (%)")+
  theme_minimal()

sp0823 <- out %>% filter(month(col_date) == 08 & year(col_date) == 2023) %>%
  group_by(age) %>%
  summarise(
    sp = mean(seroprev_monotonic),
  ) %>% ggplot(aes(x = age,y = sp))+
  geom_line()+
  ylim(c(0,100))+
  labs(tag = "Aug 2023",y = "Seroprevalence (%)")+
  theme_minimal()

sp1223 <- out %>% filter(month(col_date) == 12 & year(col_date) == 2023) %>%
  group_by(age) %>%
  summarise(
    sp = mean(seroprev_monotonic),
  ) %>% ggplot(aes(x = age,y = sp))+
  geom_line()+
  ylim(c(0,100))+
  labs(tag = "Dec 2023",y = "Seroprevalence (%)")+
  theme_minimal()

sp1222 | sp0423 | sp0823 | sp1223

out %>% filter(month(col_date) %in% c(4,8,12)) %>%
  mutate(col_time = ymd(year(col_date),month(col_date)))
  group_by(age)



