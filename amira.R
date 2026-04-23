## ARIMA
library(parallel)
library(dplyr)
library(forecast)
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
    dplyr::filter(year < y | (year == y & week <= w)) |>
    data.frame() |>
    na.omit()

  # Test = 3 weeks ahead from week w
  test_data <- hfmd_data |>
    dplyr::filter(year == y & week >= w & week <= w + 2) |>
    data.frame()

  # Convert training to time series object
  ts_train <- ts(train_data$y_transformed, frequency = 52)

  # Fit auto ARIMA with tryCatch
  model <- auto.arima(ts_train)

  # Fix 1: forecast exactly nrow(test_data) steps, not always 3
  h  <- nrow(test_data)
  fc <- forecast(model, h = h, level = c(80, 95))

  # Fix 2: safely back-transform with InvBoxCox instead of manual formula
  # handles edge cases like lambda = 0 (log transform)
  future <- test_data |>
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

  r.t <- estimate_R(
    incid = train_data$n,
    # dt = 7,
    # recon_opt = "match",
    method = "parametric_si",
    config = make_config(list(mean_si = 3.7,
                              std_si = 2.6))
  )

  past <- train_data %>%
    mutate(r_t.time = 1:nrow(.),
           week_run = w,
           year_run = y) %>%
    left_join(clean_names(r.t$R), by = join_by(r_t.time == t_end)) |>
    filter(year >= 2022)

  result <- list()
  result$past <- past
  result$future <- future

  return(result)
}

# Explicit cluster setup
cl <- makeCluster(cores)

# Export everything the workers need
clusterExport(cl, varlist = c("run_arima_week", "hfmd_1324",
                              "lambda", "year_week_grid"))

# Load packages on each worker
clusterEvalQ(cl, {
  library(forecast)
  library(dplyr)
  library(EpiEstim)
  library(janitor)
})

# Run
sarima_results_list <- parLapply(
  cl  = cl,
  X   = 1:nrow(year_week_grid),
  fun = function(i) {
    tryCatch(
      run_arima_week(
        w         = year_week_grid$w[i],
        y         = year_week_grid$y[i],
        hfmd_data = hfmd_1324,
        lambda    = lambda
      ),
      error = function(e) return(NULL)
    )
  }
)

# Always stop cluster when done
stopCluster(cl)

results_arima_24 <- bind_rows(sarima_results_list)

save(sarima_results_list,file = "results_arima_24.RData")

load("results_arima_24.RData")

sarima_3w_23[[28]]

plot_arima <- function(data, week_min,week_max){

  results_past   <- data |> lapply(\(x) x$past)   |> bind_rows()
  results_future <- data |> lapply(\(x) x$future) |> bind_rows()

  scale_rt <- 1000

  results_future_p <- results_future |>
    filter(week_run >= week_min & week_run <= week_max)

  results_past_p  <- results_past |>
    filter(week_run >= week_min & week_run <= week_max)


  results_past_p |>
    ggplot(aes(x = adm_week)) +
    ## rt
    geom_col(aes(y = n), alpha = .2) +
    geom_line(aes(y = mean_r * scale_rt)) +
    geom_ribbon(aes(y = mean_r * scale_rt,
                    ymin = quantile_0_025_r  * scale_rt,
                    ymax = quantile_0_975_r * scale_rt),
                fill = "red", alpha = 0.2) +
    scale_y_continuous(
      name      = "Number of cases",
      sec.axis  = sec_axis(trans = ~ . / scale_rt, name = "R(t)"),
      limits = c(0,4000)
    ) +
    scale_x_date(
      limits = c(max(results_future_p$adm_week) - weeks(60),
                 max(results_future_p$adm_week))
    ) +
    ## training fit
    # geom_line(aes(y = train),linetype = "dashed") +
    ## future predictions
    geom_point(
      data = results_future_p ,
      aes(x = adm_week, y = n),
      size = .3
    ) +
    geom_line(
      data = results_future_p,
      aes(x = adm_week, y = pred_t)
    ) +
    geom_ribbon(
      data = results_future_p,
      aes(ymin = y_lower95, ymax = y_upper95),
      fill = "#4A90D9", alpha = 0.15, na.rm = TRUE
    ) +
    geom_ribbon(
      data = results_future_p,
      aes(ymin = y_lower80, ymax = y_upper80),
      fill = "#4A90D9", alpha = 0.25, na.rm = TRUE
    ) +
    ## vertical line at forecast start per facet
    geom_vline(
      data = results_future_p |>
        group_by(year_run, week_run) |>
        slice(1),                          # first row per facet = forecast start
      aes(xintercept = adm_week),
      linetype = "solid"
    ) +
    geom_hline(yintercept = 1000, linetype = "dashed") +
    labs(x = "Admission week") +
    theme_bw() +
    facet_wrap(~ week_run, scale = "free_x")
}

plot_arima(sarima_results_list,week_min = 20,week_max = 52)

results_arima_24 |>
  filter(week_run <= 24) |>
  model_plot_w()

results_arima |>
  ggplot(aes(x = adm_week)) +
  geom_point(
    data = results_arima |> dplyr::select(-week_run),
    aes(y = n),
    alpha = 0.4,
    size = .2
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
  facet_wrap( ~ week_run)


perform_arima <- results_arima |>
  group_by(week_run) |>
  summarise(
    rmse = sqrt(mean((n - pred_t)^2, na.rm = TRUE)),
    mae  = mean(abs(n - pred_t), na.rm = TRUE),
    mape = mean(abs((n - pred_t) / n), na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  # rename()
  mutate(model = "ARIMA")

perform_model <- pred_3w |>
  group_by(id) |>
  summarise(
    rmse = sqrt(mean((n - pred_t)^2, na.rm = TRUE)),
    mae  = mean(abs(n - pred_t), na.rm = TRUE),
    mape = mean(abs((n - pred_t) / n), na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  rename(week_run = id) |>
  mutate(model = "BART") |>
  rbind(perform_arima)

perform_model |>
  pivot_longer(cols = - c(week_run,model)) |>
  ggplot(aes(x = as.numeric(week_run)))+
  geom_line(aes(y = value,group = model,color = model))+
  facet_wrap(~name)

###


run_arima_week <- function(w, y, hfmd_data, lambda, week_ahead) {

  # i        = 27
  # w         = year_week_grid$w[i]
  # y         = year_week_grid$y[i]
  # hfmd_data = hfmd_1324
  # lambda    = lambda
  # week_ahead = 4

  # Expanding window training data
  train_data <- hfmd_data |>
    dplyr::filter(year < y | (year == y & week <= w)) |>
    data.frame() |>
    na.omit()

  test_data <- hfmd_data |>
    dplyr::filter(year == y & week %in% (w + 1):(w + week_ahead)) |>
    data.frame()

  # Convert training to time series object
  ts_train <- ts(train_data$y_transformed, frequency = 52)

  # Fit auto ARIMA with tryCatch
  model <- auto.arima(ts_train)

  # Fix 1: forecast exactly nrow(test_data) steps, not always 3
  h  <- nrow(test_data)
  fc <- forecast(model, h = h, level = c(80, 95))

  # Fix 2: safely back-transform with InvBoxCox instead of manual formula
  # handles edge cases like lambda = 0 (log transform)
  future <- test_data |>
    mutate(
      pred_t    = as.numeric(InvBoxCox(fc$mean,          lambda)),
      y_lower95 = as.numeric(InvBoxCox(fc$lower[, "95%"], lambda)),
      y_upper95 = as.numeric(InvBoxCox(fc$upper[, "95%"], lambda)),
      y_lower80 = as.numeric(InvBoxCox(fc$lower[, "80%"], lambda)),
      y_upper80 = as.numeric(InvBoxCox(fc$upper[, "80%"], lambda)),
      week_run  = w,
      year_run  = y
    )

  r.t <- estimate_R(
    incid = train_data$n,
    # dt = 7,
    # recon_opt = "match",
    method = "parametric_si",
    config = make_config(list(mean_si = 3.7,
                              std_si = 2.6))
  )

  past <- train_data %>%
    mutate(r_t.time = 1:nrow(.),
           week_run = w,
           year_run = y) %>%
    left_join(clean_names(r.t$R), by = join_by(r_t.time == t_end)) |>
    filter(year >= 2022)

  result <- list()
  result$past <- past
  result$future <- future

  return(result)
}

year_week_grid <- expand.grid(y = 2023, w = 1:52, week_ahead = 4)

# Explicit cluster setup
cl <- makeCluster(cores)

# Export everything the workers need
clusterExport(cl, varlist = c("run_arima_week", "hfmd_1324",
                              "lambda", "year_week_grid"))

# Load packages on each worker
clusterEvalQ(cl, {
  library(forecast)
  library(dplyr)
  library(EpiEstim)
  library(janitor)
})

# Run
sarima_results_list <- parLapply(
  cl  = cl,
  X   = 1:nrow(year_week_grid),
  fun = function(i) {
    tryCatch(
      run_arima_week(
        w         = year_week_grid$w[i],
        y         = year_week_grid$y[i],
        hfmd_data = hfmd_1324,
        lambda    = lambda,
        week_ahead = year_week_grid$week_ahead[i]
      ),
      error = function(e) return(NULL)
    )
  }
)

# Always stop cluster when done
stopCluster(cl)

save(sarima_results_list,file = "./data/results_arima_23.RData")

load("./data/results_arima_23.RData")
# sarima_3w_23 <- sarima_results_list


sarima_results_list[[27]]$future

sarima_results_list|>
  plot_bart4w(week_interval = c(1,13))

sarima_results_list|>
  plot_bart4w(week_interval = c(14,26))

sarima_results_list|>
  plot_bart4w(week_interval = c(27,40))

sarima_results_list |>
  plot_bart4w(week_interval = c(41,52))


##

sarima_results_list

bart_4w_rt


pred_bart <- bart_4w_rt |> lapply(\(x) x$future) |> bind_rows()
pred_sarima <- sarima_results_list |> lapply(\(x) x$future) |> bind_rows()

performance_2models <- bind_rows(pred_bart, pred_sarima, .id = "model") |>
  mutate(
    model = case_when(model == 1 ~ "bart", .default = "sarima"),
    cover = case_when(n <= y_upper95 &
                        n >= y_lower95 ~ T, .default = F)
  ) |>
  group_by(model, week_run) |>
  summarise(
    rmse = sqrt(mean((n - pred_t)^2, na.rm = TRUE)),
    mae  = mean(abs(n - pred_t), na.rm = TRUE),
    mape = mean(abs((n - pred_t) / n), na.rm = TRUE) * 100,
    picp = mean(cover),
    pinaw = mean(y_upper95 - y_lower95) / (max(n) - min(n)),
    penalty = ifelse(picp < .95, exp(-1 * (picp - .95)), 1),
    cwc = pinaw * penalty,
    .groups = "drop"
  ) |>
  dplyr::select(-c(penalty, picp)) |>
  pivot_longer(cols = -c(week_run,model))

performance_2models |>
  ggplot(aes(
    x = week_run,
    y = value,
    group = model,
    color = model
  )) +
  geom_line() +
  facet_wrap( ~ factor(name, levels = c("rmse", "mae", "mape", "cwc", "pinaw")), scale = "free_y") +
  theme_bw() +
  labs(x = "Week forecasting") +
  theme(legend.position = "bottom")


performance_2models |>
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_boxplot() +
  facet_wrap( ~ factor(name, levels = c("rmse", "mae", "mape", "cwc", "pinaw")), scale = "free_y") +
  theme_bw() +
  labs(x = "Model") +
  theme(legend.position = "bottom")
