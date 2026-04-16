library(BART)
library(tidyverse)
library(forecast)
library(MASS)
### bart for hfmd

path2data <- paste0("/Users/nguyenpnt/Library/CloudStorage/OneDrive-OxfordUniversityClinicalResearchUnit/Data/HFMD/cleaned/")

# Name of cleaned HFMD data file
cases_file <- "hfmd_hcdc.rds"

df_cases <- paste0(path2data, cases_file) |>
  readRDS()

hfmd_1324 %<>%
  mutate(
    y_transformed = ((n^lambda - 1) / lambda),
    lag_t.1 = lag(y_transformed,1),
    lag_t.2 = lag(y_transformed,2),
    lag_t.3 = lag(y_transformed,3)
  )

cores <- detectCores() - 1

run_bart_month <- function(w, hfmd_data,y) {

  # hfmd_data <- hfmd_1324
  # y = 2023
  # w = 39

  train_data <- hfmd_data |>
    filter(year < y | (year == y & week == w)) |>
    data.frame() |>
    na.omit()

  test_data <- hfmd_data |>
    filter(year == y & week %in% c(w+1,w+2,w+3)) |>
    data.frame()

  if (nrow(test_data) == 0)
    return(NULL)

  model <- wbart(
    x.train = train_data[, c("adm_week", "lag_t.1","lag_t.2","lag_t.3")],
    y.train = train_data$y_transformed,
    # x.test = test_data[, c("adm_week", "lag_t.1", "lag_t.2")],
    ntree   = 1000,
    ndpost  = 10000,
    nskip   = 1000,
    a       = 0.95,
    b       = 2
  )

  pred_draws <- predict(model,test_data[, c("adm_week", "lag_t.1", "lag_t.2", "lag_t.3")])
  pred_ori <- (pred_draws * lambda + 1)^(1 / lambda)

  result <- test_data |>
    mutate(
      pred_t = colMeans(pred_ori),
      y_lower95  = apply(pred_ori, 2, quantile, 0.025),
      y_upper95  = apply(pred_ori, 2, quantile, 0.975),
      y_lower80  = apply(pred_ori, 2, quantile, 0.10),
      y_upper80  = apply(pred_ori, 2, quantile, 0.90)
    )

  return(result)
}

year_week_grid <- expand.grid(y = c(2023), w = 1:52)

results_list <- mclapply(
  X        = 1:nrow(year_week_grid),
  FUN      = function(i) {
    run_bart_month(
      w         = year_week_grid$w[i],
      y         = year_week_grid$y[i],
      hfmd_data = hfmd_1324
    )
  },
  mc.cores = cores
)


## prediction for 1 week
pred_1w <- bind_rows(results_list)
save(pred_1w,file = "pred_1w.RData")

load("pred_1w.RData")

pred_1w |>
  filter(week >= 20) |>
  ggplot(aes(x = adm_week)) +
  geom_point(aes(y = n)) +
  geom_point(aes(y = pred_t), color = "red") +
  geom_errorbar(
    aes(ymin = y_lower95, ymax = y_upper95),
    fill = "#4A90D9",
    alpha = 0.15,
    na.rm = TRUE
  ) +
  # # Shaded 80% credible interval
  # geom_ribbon(
  #   aes(ymin = y_lower80, ymax = y_upper80),
  #   fill = "#4A90D9",
  #   alpha = 0.25,
  #   na.rm = TRUE
  # ) +
  scale_x_date(name = "Admission week") +
  scale_y_continuous(name = "Total cases") +
  theme_bw() +
  facet_wrap(~ week, ncol = 4)


## prediction for 2 weeks

pred_2w <- bind_rows(results_list,.id = "id")
save(pred_2w,file = "pred_2w.RData")

load("pred_2w.RData")

vlines <- pred_2w |>
  group_by(id) |>
  summarise(
    vline1 = min(adm_week),   # replace with your actual breakpoint logic
    vline2 = max(adm_week)    # replace with your actual breakpoint logic
  )

pred_2w |>
  # filter(week >= 20) |>
  ggplot(aes(x = adm_week)) +
  geom_point(aes(y = n)) +
  geom_line(aes(y = pred_t)) +
  geom_ribbon(
    aes(ymin = y_lower95, ymax = y_upper95),
    fill = "#4A90D9",
    alpha = 0.15,
    na.rm = TRUE
  ) +
  # Shaded 80% credible interval
  geom_ribbon(
    aes(ymin = y_lower80, ymax = y_upper80),
    fill = "#4A90D9",
    alpha = 0.25,
    na.rm = TRUE
  ) +
  scale_x_date(name = "Admission week")+
  scale_y_continuous(name = "Total cases")+
  theme_bw()+
  # Vertical lines per facet/id
  # geom_vline(
  #   data     = vlines,
  #   aes(xintercept = vline1),
  #   color    = "red",
  #   linetype = "dashed",
  #   linewidth = 0.8,
  #   alpha = 0.4
  # ) +
  # geom_vline(
  #   data     = vlines,
  #   aes(xintercept = vline2),
  #   color    = "blue",
  #   linetype = "dashed",
  #   linewidth = 0.8,
  #   alpha = 0.4
  # ) +
  # facet_wrap( ~ factor(id,levels = pred_2w$id |> unique()),ncol = 4)
  {}

### 2 week ahead


pred_3w <- bind_rows(results_list,.id = "id")
save(pred_3w,file = "pred_3w.RData")

load("pred_3w.RData")

bart_plot_w(pred_2w)

bart_plot_w <- function(data){
  data |>
    ggplot(aes(x = adm_week)) +
    geom_point(
      data = data |> select(-id),
      aes(y = n),
      size = .2,
      alpha = .1
    ) +
    geom_line(aes(y = pred_t)) +
    geom_ribbon(
      aes(ymin = y_lower95, ymax = y_upper95),
      fill = "#4A90D9",
      alpha = 0.15,
      na.rm = TRUE
    ) +
    # Shaded 80% credible interval
    geom_ribbon(
      aes(ymin = y_lower80, ymax = y_upper80),
      fill = "#4A90D9",
      alpha = 0.25,
      na.rm = TRUE
    ) +
    scale_x_date(name = "Admission week") +
    scale_y_continuous(name = "Total cases") +
    theme_bw() +
    facet_wrap(~ factor(id, levels = data$id |> unique()), ncol = 6)
}

pred_3w |>
  ggplot(aes(x = adm_week)) +
  geom_point(
    data = pred_3w |> select(-id),
    aes(y = n),
    size = .2,
    alpha = .1
  ) +
  geom_line(aes(y = pred_t)) +
  geom_ribbon(
    aes(ymin = y_lower95, ymax = y_upper95),
    fill = "#4A90D9",
    alpha = 0.15,
    na.rm = TRUE
  ) +
  # Shaded 80% credible interval
  geom_ribbon(
    aes(ymin = y_lower80, ymax = y_upper80),
    fill = "#4A90D9",
    alpha = 0.25,
    na.rm = TRUE
  ) +
  scale_x_date(name = "Admission week") +
  scale_y_continuous(name = "Total cases") +
  theme_bw() +
  facet_wrap(~ factor(id, levels = pred_3w$id |> unique()), ncol = 6)


## rmse of prediction
pred_3w |>
  group_by(id) |>
  summarise(rmse = sqrt(mean((n - pred_t)^2))) |>
  mutate(id = as.numeric(id)) |>
  ggplot(aes(x = id, y = rmse))+
  geom_line()

## ARIMA
library(forecast)
library(parallel)
library(dplyr)
library(tseries)

## ARIMA assume data is stationary
## A time series is considered stationary if its statistical properties,
## such as mean and variance, remain constant over time.

ts_train <- ts(train_data$n, frequency = 52)

## adf test
## null hypothesis: The time series has a unit root, meaning it is non-stationary.
adf.test(ts_train) ## can reject the null hypothesis
## kpss test
## null hypothesis: The series is stationary
kpss.test(ts_train) ## can reject the null hypothesis

plot(ts_train)

## different transformation

diff_data <- diff(ts_train)
## conduct a re-test
adf.test(diff_data) ## can reject the null hypothesis
kpss.test(diff_data) ## not have enough evidence to reject null hypothesis
plot(diff_data)

run_arima_week <- function(w, y, hfmd_data, lambda) {

  # i        = 1
  # w         = year_week_grid$w[i]
  # y         = year_week_grid$y[i]
  # hfmd_data = hfmd_1324
  # lambda    = lambda


  # Expanding window training data
  train_data <- hfmd_data |>
    filter(year < y | (year == y & week < w)) |>
    data.frame() |>
    na.omit()

  # Test = 3 weeks ahead from week w
  test_data <- hfmd_data |>
    filter(year == y & week >= w & week <= w + 2) |>
    data.frame()

  if (nrow(test_data) == 0 | nrow(train_data) < 10) return(NULL)

  # Convert training to time series object
  ts_train <- ts(train_data$y_transformed, frequency = 52)

  # Fit auto ARIMA with tryCatch
  model <- auto.arima(ts_train)


  if (is.null(model)) return(NULL)

  # Fix 1: forecast exactly nrow(test_data) steps, not always 3
  h  <- nrow(test_data)
  fc <- forecast(model, h = h, level = c(80, 95))

  # Fix 2: safely back-transform with InvBoxCox instead of manual formula
  # handles edge cases like lambda = 0 (log transform)
  result <- test_data |>
    mutate(
      pred_t    = as.numeric(InvBoxCox(fc$mean,          lambda)),
      y_lower95 = as.numeric(InvBoxCox(fc$lower[, "95%"], lambda)),
      y_upper95 = as.numeric(InvBoxCox(fc$upper[, "95%"], lambda)),
      y_lower80 = as.numeric(InvBoxCox(fc$lower[, "80%"], lambda)),
      y_upper80 = as.numeric(InvBoxCox(fc$upper[, "80%"], lambda)),
      horizon   = row_number(),
      week_run  = w,
      year_run  = y
    )

  return(result)
}


sarima_results_list <- mclapply(
  X        = 1:2,
  FUN      = function(i) {
    run_arima_week(
      w         = year_week_grid$w[i],
      y         = year_week_grid$y[i],
      hfmd_data = hfmd_1324,
      lambda    = lambda
    )
  },
  mc.cores = 10
)

results_arima <- bind_rows(sarima_results_list)

result |>
  ggplot(aes(x = adm_week))+
  geom_point(aes(y = n))+
  geom_line(aes(y = pred_t)) +
  geom_ribbon(
    aes(ymin = y_lower95, ymax = y_upper95),
    fill = "#4A90D9",
    alpha = 0.15,
    na.rm = TRUE
  ) +
  # Shaded 80% credible interval
  geom_ribbon(
    aes(ymin = y_lower80, ymax = y_upper80),
    fill = "#4A90D9",
    alpha = 0.25,
    na.rm = TRUE
  )

## fit by month, and use lag of 12 months

district_month_df <- df_cases %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month"))) %>%
  group_by(adm_month, district) %>%
  dplyr::count() %>%
  ungroup() %>%
  mutate(month = month(adm_month), year = year(adm_month))

## epicurve districts
district_month_df |>
  ggplot(aes(x = adm_month)) +
  geom_line(aes(y = n)) +
  facet_wrap(~ district)

##

district_month_df |>
  ggplot(aes(n))+
  geom_histogram()+
  facet_wrap(~ district)

bc <- boxcox(n ~ adm_month,data = district_month_df)
lambda <- bc$x[which.max(bc$y)]

district_month_df %<>%
  filter(year != 2024) %>%
  mutate(
    y_transformed = ((n^lambda - 1) / lambda),
    lag = lag(y_transformed, 6),
  )

district_month_df |>
  ggplot(aes(x = y_transformed,y = lag))+
  geom_point()+
  facet_wrap(~ district)
  # ggplot(aes(y_transformed))+
  # geom_histogram()+
  # facet_wrap(~ district)

district_month_df |>
  group_by(district) |>
  group_split()

training_set <- district_month_df |>
  filter(year < 2023)

testing_set <- district_month_df |>
  filter(year == 2023)

dis = unique(district_month_df$district)

district_month_df |>
  filter(year < 2023 | (year == 2023 & month <= 6)) |>
  tail(12)
year == 2023 & month <= 6

run_bart_dis <- function(num, hfmd_data, lambda) {  # Fix 1: swap arg order, add lambda

  hfmd_split <- hfmd_data |>
    group_by(district) |>
    group_split()

  train_data <- hfmd_split[[num]] |>
    filter(year < 2023 | (year == 2023 & month <= 6)) |>
    data.frame() |>
    na.omit()

  test_data <- hfmd_split[[num]] |>
    filter(year == 2023 & month > 6) |>        # Fix 2: remove meaningless district == district
    data.frame()

  if (nrow(test_data) == 0) return(NULL)
  if (nrow(train_data) == 0) return(NULL)  # extra safety check

  model <- wbart(
    x.train = train_data[, c("adm_month", "lag"), drop = FALSE],
    y.train = train_data$y_transformed,
    ntree   = 1000,
    ndpost  = 10000,
    nskip   = 1000,
    a       = 0.95,
    b       = 2
  )

  pred_draws <- predict(model, test_data[, c("adm_month", "lag"), drop = FALSE])
  pred_ori   <- (pred_draws * lambda + 1)^(1 / lambda)  # Fix 3: uses passed lambda

  result <- test_data |>
    mutate(
      pred_t    = colMeans(pred_ori),
      y_lower95 = apply(pred_ori, 2, quantile, 0.025),
      y_upper95 = apply(pred_ori, 2, quantile, 0.975),
      y_lower80 = apply(pred_ori, 2, quantile, 0.10),
      y_upper80 = apply(pred_ori, 2, quantile, 0.90)
    )

  return(result)
}


results_list <- mclapply(
  X         = 1:22,
  FUN       = run_bart_dis,
  hfmd_data = district_month_df,   # Fix 1: pass dataset explicitly
  lambda    = lambda,       # Fix 3: pass lambda explicitly
  mc.cores  = cores
)


results_list |>
  bind_rows() |>
  ggplot(aes(x = adm_month))+
  geom_point(aes(y=n))+
  geom_line(aes(y = pred_t)) +
  geom_ribbon(
    aes(ymin = y_lower95, ymax = y_upper95),
    fill = "#4A90D9",
    alpha = 0.15,
    na.rm = TRUE
  ) +
  # Shaded 80% credible interval
  geom_ribbon(
    aes(ymin = y_lower80, ymax = y_upper80),
    fill = "#4A90D9",
    alpha = 0.25,
    na.rm = TRUE
  ) +
  facet_wrap(~district)

####
