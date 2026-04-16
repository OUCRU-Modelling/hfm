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

run_bart_month <- function(w, hfmd_data,y,lambda) {

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
  pred_ori <- InvBoxCox(pred_draws,lambda)

  result <- test_data |>
    mutate(
      pred_t = colMeans(pred_ori),
      y_lower95  = apply(pred_ori, 2, quantile, 0.025),
      y_upper95  = apply(pred_ori, 2, quantile, 0.975),
      y_lower80  = apply(pred_ori, 2, quantile, 0.10),
      y_upper80  = apply(pred_ori, 2, quantile, 0.90),
      week_run = w,
      year_run = y
    )

  return(result)
}

year_week_grid <- expand.grid(y = 2023, w = 1:52)

results_list <- mclapply(
  X        = 1:nrow(year_week_grid),
  FUN      = function(i) {
    run_bart_month(
      w         = year_week_grid$w[i],
      y         = year_week_grid$y[i],
      hfmd_data = hfmd_1324,
      lambda = lambda
    )
  },
  mc.cores = cores
)

### 3 week ahead


pred_3w <- bind_rows(results_list)
save(pred_3w,file = "pred_3w.RData")

load("pred_3w.RData")

bart_plot_w(pred_3w)

## for 2024

year_week_grid <- expand.grid(y = 2024, w = 1:26)

results_list <- mclapply(
  X        = 1:nrow(year_week_grid),
  FUN      = function(i) {
    run_bart_month(
      w         = year_week_grid$w[i],
      y         = year_week_grid$y[i],
      hfmd_data = hfmd_1324,
      lambda = lambda
    )
  },
  mc.cores = cores
)


pred_3w_24 <- bind_rows(results_list)
save(pred_3w_24,file = "pred_3w_24.RData")

load("pred_3w_24.RData")

model_plot_w(pred_3w_24)

## rmse of prediction
pred_3w |>
  group_by(id) |>
  summarise(rmse = sqrt(mean((n - pred_t)^2))) |>
  mutate(id = as.numeric(id)) |>
  ggplot(aes(x = id, y = rmse))+
  geom_line()

