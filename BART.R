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
    lag_t.3 = lag(y_transformed,3),
    lag_t.4 = lag(y_transformed,4)
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

## combine with R(t)
# install.packages("EpiEstim")
library(EpiEstim)

run_bart_month <- function(w, hfmd_data,y,lambda) {

  # hfmd_data <- hfmd_1324
  # y = 2023
  # w = 23

  train_data <- hfmd_data |>
    filter(year < y | (year == y & week <= w)) |>
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

  ## prediction

  pred_draws <- predict(model,test_data[, c("adm_week", "lag_t.1", "lag_t.2", "lag_t.3")])
  pred_ori <- InvBoxCox(pred_draws,lambda)

  future <- test_data |>
    mutate(
      pred_t = colMeans(pred_ori),
      y_lower95  = apply(pred_ori, 2, quantile, 0.025),
      y_upper95  = apply(pred_ori, 2, quantile, 0.975),
      y_lower80  = apply(pred_ori, 2, quantile, 0.10),
      y_upper80  = apply(pred_ori, 2, quantile, 0.90),
      week_run = w,
      year_run = y
    )

  ## r(t) estimation

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
           train = InvBoxCox(model$yhat.train.mean,lambda),
           week_run = w,     # add these
           year_run = y) %>%
    left_join(clean_names(r.t$R), by = join_by(r_t.time == t_end)) |>
    filter(year >= 2022)

  result <- list()
  result$past <- past
  result$future <- future

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

hfmd_1324 |>
  filter(year == 2024)

# save(results_list,file = "./data/pred_3w_23.RData")
load("./data/pred_3w_23.RData")

# save(results_list,file = "./data/pred_3w_24.RData")
# load("./data/pred_3w_24.RData")

plot_bart <- function(data, week_min,week_max){

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


plot_bart(results_list,week_min = 20,week_max = 52)

hfmd_1324 |>
  filter(year > 2020) |>
  ggplot(aes(x = adm_week, y =n))+
  geom_line()+
  scale_x_date(date_breaks = "3 month")


