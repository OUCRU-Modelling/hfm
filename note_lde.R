library(readxl)
library(tidyverse)
library(gtsummary)
library(patchwork)
library(lubridate)
library(stringr)
library(mgcv)
library(janitor)
library(predtools)
library(magrittr)
library(slider)
library(scam)
library(epitools)
library(sf)
library(stringi)
invisible(Sys.setlocale("LC_TIME", "English"))

## case notification
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



hfmd_ts <- df1 %>%
  group_by(adm_week) %>%
  count()

hfmd_ts %>%
  ggplot(aes(x = adm_week,y = n))+
  geom_line()


hfmd_ts %>%
  filter(year(adm_week) == 2023)%>%
  ggplot(aes(x = adm_week,y = n))+
  geom_line()

hfmd_ts %>%
  filter(year(adm_week) == 2023) %>% pull(n) %>% cumsum()


hfmd_ts %>%
  filter(year(adm_week) == 2023) %>%
  ungroup() %>%
  mutate(week = 1:53) %>%
  filter(week <= 36) %>%
  ggplot(aes(x = adm_week,y = n))+
  geom_line()

input_dt <- hfmd_ts %>%
  filter(year(adm_week) == 2023) %>%
  ungroup() %>%
  mutate(week = 1:53)


input_dt %>%
  mutate(cum_n = cumsum(n)) %>%
  ggplot(aes(x = adm_week,y = cum_n))+
  geom_line()


lde_deriv <- function(t, n, parms) {
  with(as.list(parms), {
    dn <- k * n * (1 - n / N)
    list(dn)
  })
}

# Analytical solution of LDE
lde_solution <- function(t, k,c, N) {
  n <- N / (1 + exp(-k*t - c))
  return(n)
}

# Example: simulate 30 weeks of epidemic

## parameter need to find
k <- 0.4
c <- 5

N <- 43617

t <- seq(0, 36, by = 1)

# Compute cumulative cases over time
n_t <- lde_solution(t, k,c, N)

# Combine into data frame
lde_df <- data.frame(week = t, cum_cases = n_t)

# Plot
ggplot(lde_df, aes(x = week, y = cum_cases)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = "Logistic Differential Equation (LDE) model",
    x = "Week",
    y = "Cumulative reported cases"
  ) +
  theme_minimal()


df_fit <- input_dt %>%
  mutate(cum_n = cumsum(n))


N <- 23843

fit <- optim(
  par = start_par,
  fn = nll,
  t = df_fit$week,
  obs = df_fit$cum_n,
  N = N,
  method = "L-BFGS-B",
  lower = c(0, -Inf),
  control = list(maxit = 1000)
)

k_f <- fit$par[1]
c_f <- fit$par[2]


n_t <- lde_solution(t = df_fit$week, k = k_f,c=c_f, N)

# Combine into data frame
lde_df <- data.frame(week = df_fit$week, cum_cases = n_t)

# Plot
ggplot() +
  geom_line(data = lde_df, aes(x = week, y = cum_cases),
            color = "red", size = 1) +
  geom_line(data = df_fit, aes(x = week, y = cum_n),
            color = "blue", size = 1)+
  theme_minimal()

t1 = (-c_f-1.317)/k_f
t2 = (-c_f+1.317)/k_f

data.frame(n = diff(lde_df$cum_cases),
           week = 1:length(diff(lde_df$cum_cases))) %>%
  ggplot()+
  geom_line(aes(x = week,y = n))+
  geom_point(data = df_fit,aes(x = week,y = n))+
  geom_vline(xintercept = t1-2)+
  geom_vline(xintercept = t2)

##

lde_model <- function(data){

  df_fit <- data %>%
    mutate(cum_n = cumsum(n),
           week2 = 1:nrow(.)) %>%
    select(week2,n,cum_n)

  N <- df_fit %>% pull(cum_n) %>% tail(1)

  lde_solution <- function(t, k,c, N) {
    n <- N / (1 + exp(-k*t - c))
    return(n)
  }

  nll <- function(par, t, obs, N) {
    k <- par[1]
    c <- par[2]

    # Predicted values
    pred <- lde_solution(t, k, c, N)

    # Least squares error
    SSE <- sum((obs - pred)^2)
    return(SSE)
  }

  start_par <- c(k = 0, c = 0)

  fit <- optim(
    par = start_par,
    fn = nll,
    t = df_fit$week2,
    obs = df_fit$cum_n,
    N = N,
    method = "L-BFGS-B",
    lower = c(0, -Inf),
    control = list(maxit = 1000)
  )

  k_f <- fit$par[1]
  c_f <- fit$par[2]

  n_t <- lde_solution(t = df_fit$week2, k = k_f,c=c_f, N)

  # Combine into data frame
  lde_df <- data.frame(week = df_fit$week2, cum_cases = n_t)

  t1 = (-c_f-1.317)/k_f
  t2 = (-c_f+1.317)/k_f

  re <- list()
  re$df <- lde_df
  re$dt_f <- df_fit
  re$t1 <- t1
  re$t2 <- t2
  re
}

plot_lde <- function(model){
  data.frame(n = diff(model$df$cum_cases),
             week = 1:length(diff(model$df$cum_cases))) %>%
    ggplot()+
    geom_line(aes(x = week,y = n))+
    geom_point(data = model$dt_f,
               aes(x = week2,y = n))+
    geom_vline(xintercept = model$t1-2)+
    geom_vline(xintercept = model$t2)
}

output1 <- lde_model(data = input_dt %>% filter(week <= 36))

f23 <- plot_lde(output1)

output2 <- lde_model(data = input_dt %>% filter(week > 36))

s23 <-plot_lde(output2)


input_dt2 <- hfmd_ts %>%
  filter(year(adm_week) == 2024) %>%
  ungroup() %>%
  mutate(week = 1:nrow(.))

input_dt2 %>%
  filter(week >= 7) %>%
  ggplot(aes(x=week,y=n))+
  geom_col()

output3 <- lde_model(data = input_dt2 %>%
                       filter(week >= 7))

plot_lde(output3)


f23|s23

data.frame(n = diff(rbind(output1$df,output2$df)$cum_cases),
           week = 1:length(diff(rbind(output1$df,output2$df)$cum_cases))) %>%
  filter(n >0) %>%
  ggplot()+
  geom_line(aes(x = week,y = n))+
  geom_point(data = input_dt,
             aes(x = week,y = n))+
  # geom_vline(xintercept = 16)+
  # geom_vline(xintercept = output1$t2)
  {}

##


df1 %>%
  group_by(district,adm_week) %>%
  count() %>%
  ggplot() +
  # geom_tile(aes(x = adm_week,y = district,fill=n))+
  geom_line(aes(x = adm_week,y = n))+
  facet_wrap(~district)

