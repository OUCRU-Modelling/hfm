library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)
library(patchwork)
## import data vaccine registry

count_dangky_week <- readRDS("/Users/nguyenpnt/Library/CloudStorage/OneDrive-OxfordUniversityClinicalResearchUnit/OUCRU Modelling - HFMD HCMC 2023/data/cleaned/count_dangky_week.rds")
child <- count_dangky_week %>% filter(birth_year >= 2017) %>% group_by(birth_week, birth_year) %>%
  summarise(n=sum(n)) %>% arrange(birth_year)
colnames(child) <- c("week","year","birth")
## combine week 52 and 53

child$week <- ifelse(child$week == 53,52,child$week)

child <- child %>% group_by(year) %>%
  mutate(newn = ifelse(week == 52, sum(birth[week==52]), birth)) %>%
  {data.frame(.$week, .$year, .$newn )} %>% unique() %>%
  magrittr::set_colnames(c("week","year","birth"))

child$week2 <- seq(1:length(child$week))

time <- data.frame()

for (i in 2017:2022){
  range <- child$week[child$year == i]
  if (length(range) == 52){
    time_add <- seq.int(i + 7/365 ,(i+1) - 7/365,
                        length.out = length(range)) %>% data.frame()
  } else {
    time_add <- seq.int(i + 7/365 ,(i+1) - 7/365,
                        length.out = 52)[1:length(range)] %>% data.frame()
  }
  time <- rbind(time,time_add)
}


child[,5] <- time %>%
  magrittr::set_colnames(c("time"))

## model fitting
fit <- mgcv::gam(birth ~ s(week) + s(week2),method = "REML",data = child)

cutpoint <- function(point){
  fitt <- mgcv::gam(birth ~ s(week) + s(week2),
                    method = "REML",data = child[-c(point:nrow(child)),])

  new_data2 <- data.frame(week = rep(1:52,7))

  new_data2$week2 <- seq(1,nrow(new_data2))
  new_data2$year <- rep(2017:2023,each = 52)

  time <- data.frame()
  for (i in 2017:2023){
    range <- new_data2$week[new_data2$year == i]
    time_add <- data.frame(seq.int(i + 7/365 ,(i+1) - 7/365,
                                   length.out = length(range)))
    time <- rbind(time,time_add)
  }

  new_data2[4] <- time %>%
    magrittr::set_colnames(c("time"))

  est2 <- predict.gam(fitt,newdata = new_data2,
                      type="response",se.fit = TRUE)

  new_data2 <- new_data2 %>% mutate(
    fit = est2$fit,
    lwr = est2$fit - qt(0.95,nrow(new_data2))*est2$se.fit,
    upr = est2$fit + qt(0.95,nrow(new_data2))*est2$se.fit,
  )
  out <- list()
  out$point <- point
  out$df <- new_data2
  return(out)
}

plot_cp <- function(model){
  dta <- model$df
  ggplot(data = dta) +
    geom_line(aes(x = time,y = fit),col = "blue")+
    geom_ribbon(aes(x = time,ymin = lwr, ymax = upr), fill = "royalblue",alpha = 0.4)+
    geom_vline(xintercept = dta$time[dta$week2 == model$point])+
    ylab("births")+
    theme_minimal()
}


## The function of GAM model with cutpoint

model <- cutpoint(270)

plot_cp(model)+
  geom_point(data = child, aes(x = time, y = birth))+
  annotate("text", x= 2017, y=3500, label= "Fitting") +
  annotate("text", x = 2023.5, y=3500, label = "Estimation")+
  plot_annotation(
    title = "Fitting data until week 10/2022"
  )
