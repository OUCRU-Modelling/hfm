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
    dplyr::filter(year < y | (year == y & week < w)) |>
    data.frame() |>
    na.omit()

  # Test = 3 weeks ahead from week w
  test_data <- hfmd_data |>
    dplyr::filter(year == y & week >= w & week <= w + 2) |>
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

# Explicit cluster setup
cl <- makeCluster(cores)

# Export everything the workers need
clusterExport(cl, varlist = c("run_arima_week", "hfmd_1324",
                              "lambda", "year_week_grid"))

# Load packages on each worker
clusterEvalQ(cl, {
  library(forecast)
  library(dplyr)
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

save(results_arima_24,file = "results_arima_24.RData")

load("results_arima_24.RData")

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

