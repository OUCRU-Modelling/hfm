library(BART)
### bart for hfmd

path2data <- paste0("/Users/nguyenpnt/Library/CloudStorage/OneDrive-OxfordUniversityClinicalResearchUnit/Data/HFMD/cleaned/")

# Name of cleaned HFMD data file
cases_file <- "hfmd_hcdc.rds"

df_cases <- paste0(path2data, cases_file) |>
  readRDS()


hfmd_1324 <- df_cases  %>%
  mutate(
    adm_week = as.Date(floor_date(admission_date, "week"))
  ) %>%
  group_by(adm_week) %>%
  dplyr::count() |>
  ungroup() %>%
  mutate(
    x = 1:nrow(.)
  )

hfmd_1324 <- hfmd_1324 |>
  mutate(t = x,
         week = week(adm_week),
         year = year(adm_week)) |>
  select(-x)

hfmd_1324 |>
  ggplot(aes(x = adm_week,y = n)) +
  geom_line()

train_df <-  hfmd_1324 |>
  filter(
    year(adm_week) < 2023
  )

test_df <-  hfmd_1324 |>
  filter(
    year(adm_week) >= 2023
  )


X_train_hfmd <- train_df |>
  mutate(t = x,
         week = week(adm_week)) |>
  select(t,week) |>
  as.data.frame()

y_train_hfmd <- train_df |>
  mutate(t = x,
         week = week(adm_week)) |>
  pull(n)

X_test_hfmd  <- test_df |>
  mutate(t = x,
         week = week(adm_week)) |>
  select(t,week)|>
  as.data.frame()

# X_test_hfmd  <- rbind.data.frame(X_test_hfmd,data.frame(t = seq(598,597+26),week = seq(27,52)))

y_test_hfmd  <- test_df |>
  mutate(t = x,
         week = week(adm_week)) |>
  pull(n)


bart_model_hfmd <- wbart(
  x.train = X_train_hfmd,
  y.train = y_train_hfmd,
  x.test  = X_test_hfmd,
  ntree   = 1000,        # number of weak learners
  ndpost  = 10000,       # posterior draws (kept)
  nskip   = 1000,        # burn-in draws (discarded)
)

post_draws_hfmd <- bart_model_hfmd$yhat.test          # shape: [1000, horizon]

# Point forecast (posterior mean) and 95% credible interval
y_pred     <- colMeans(post_draws_hfmd)
y_lower95  <- apply(post_draws_hfmd, 2, quantile, 0.025)
y_upper95  <- apply(post_draws_hfmd, 2, quantile, 0.975)
y_lower80  <- apply(post_draws_hfmd, 2, quantile, 0.10)
y_upper80  <- apply(post_draws_hfmd, 2, quantile, 0.90)

# prediction <- cbind.data.frame(
#   y_lower95,y_lower80,y_pred,y_upper80,y_upper95,X_test_hfmd,y_test_hfmd
# )



plot_df <- data.frame(
  rbind(X_train_hfmd,X_test_hfmd),
  y_actual  = c(y_train_hfmd,y_test_hfmd),
  y_pred    = c(rep(NA, nrow(X_train_hfmd)), y_pred),
  y_lower95 = c(rep(NA, nrow(X_train_hfmd)), y_lower95),
  y_upper95 = c(rep(NA, nrow(X_train_hfmd)), y_upper95),
  y_lower80 = c(rep(NA, nrow(X_train_hfmd)), y_lower80),
  y_upper80 = c(rep(NA, nrow(X_train_hfmd)), y_upper80)
) |>
  mutate(
    split = case_when(
      is.na(y_pred) ~ "train",
      .default = "test"
    )
  )

ggplot(plot_df, aes(x = t)) +
  # Shaded 95% credible interval
  geom_ribbon(aes(ymin = y_lower95, ymax = y_upper95),
              fill = "#4A90D9", alpha = 0.15, na.rm = TRUE) +
  # Shaded 80% credible interval
  geom_ribbon(aes(ymin = y_lower80, ymax = y_upper80),
              fill = "#4A90D9", alpha = 0.25, na.rm = TRUE) +
  # Actual values
  geom_line(aes(y = y_actual, colour = "Actual"),
            linewidth = 0.8) +
  # Forecast line
  geom_line(aes(y = y_pred, colour = "BART forecast"),
            linewidth = 1.1, linetype = "solid", na.rm = TRUE)

prediction |>
  ggplot(aes(x = t))+
  geom_point(aes(y = y_test_hfmd))+
  geom_line(aes(y = y_pred))+
  geom_ribbon(aes(y = y_pred,ymin = y_lower80, ymax = y_upper80),alpha = .5,fill = "blue")+
  geom_ribbon(aes(y = y_pred,ymin = y_lower95, ymax = y_upper95),alpha = .2,fill = "blue")


plot_df |>
  filter()


plot_df |>
  filter(t > 518) |>
  mutate(new_y = case_when(
    split == "train" ~ y_actual,
    .default = y_pred
  ),
  year = case_when(
    split == "train" ~ "2023",
    .default = "2024"
  )
  ) |>
  ggplot(aes(x = week,y = new_y))+
  geom_line(aes(group = year,color = year))

###

# data_past <- cbind.data.frame(X_train_hfmd,y = y_train_hfmd)

hfmd_forecast_hcmc <- function(data_past,week_current,new_cases,num_week_pred){

  ## setting training data
  train_hcmc_df <- data_past |>
    select(t,week,y = n) |>
    as.data.frame()
  t_current <- nrow(train_hcmc_df)
  train_add <- data.frame(t = t_current+1,
             week = week_current,
             y = new_cases)

  train_hcmc_df %<>%
    rbind.data.frame(train_add)

  ## setting testing data
  X_test_hfmd <- data.frame(t = seq(train_hcmc_df[nrow(train_hcmc_df),1]+1,
                                    train_hcmc_df[nrow(train_hcmc_df),1] + num_week_pred),
                            week = seq(train_hcmc_df[nrow(train_hcmc_df),2]+1,
                                       train_hcmc_df[nrow(train_hcmc_df),2] + num_week_pred)
                            )

  ## model

  bart_model_hfmd <- wbart(
    x.train = train_hcmc_df[,1:2],
    y.train = train_hcmc_df[,3],
    x.test  = X_test_hfmd,
    ntree   = 1000,        # number of weak learners
    ndpost  = 10000,       # posterior draws (kept)
    nskip   = 1000,        # burn-in draws (discarded)
  )

  post_draws_hfmd <- bart_model_hfmd$yhat.test          # shape: [1000, horizon]

  # Point forecast (posterior mean) and 95% credible interval
  y_pred     <- colMeans(post_draws_hfmd)
  y_lower95  <- apply(post_draws_hfmd, 2, quantile, 0.025)
  y_upper95  <- apply(post_draws_hfmd, 2, quantile, 0.975)
  y_lower80  <- apply(post_draws_hfmd, 2, quantile, 0.10)
  y_upper80  <- apply(post_draws_hfmd, 2, quantile, 0.90)

  out <- data.frame(
    rbind.data.frame(train_hcmc_df[,1:2],X_test_hfmd),
    y_train = c(train_hcmc_df[,3],rep(NA,nrow(X_test_hfmd))),
    y_train_mean = c(bart_model_hfmd$yhat.train.mean,rep(NA,nrow(X_test_hfmd))),
    y_pred = c(rep(NA,nrow(train_hcmc_df)),y_pred),
    y_lower95 = c(rep(NA,nrow(train_hcmc_df)),y_lower95),
    y_upper95 = c(rep(NA,nrow(train_hcmc_df)),y_upper95),
    y_lower80 = c(rep(NA,nrow(train_hcmc_df)),y_lower80),
    y_upper80 = c(rep(NA,nrow(train_hcmc_df)),y_upper80)
  )
  out
}

plot_pred <- function(obj){

  model_df <- obj |>
    mutate(type = case_when(
      !is.na(y_train) ~ "train",
      .default = "predict"
    )) |>
    filter(type == "predict")

  actual_df <- hfmd_1324 |>
    filter(t %in% model_df$t) |>
    pull(n)

  cbind.data.frame(model_df,y_actual = actual_df) |>
    ggplot(aes(x = t))+
    # Shaded 95% credible interval
    geom_ribbon(aes(ymin = y_lower95, ymax = y_upper95),
                fill = "#4A90D9", alpha = 0.15, na.rm = TRUE) +
    # Shaded 80% credible interval
    geom_ribbon(aes(ymin = y_lower80, ymax = y_upper80),
                fill = "#4A90D9", alpha = 0.25, na.rm = TRUE) +
    # Forecast line
    geom_line(aes(y = y_pred),
              linewidth = 1.1, linetype = "solid", na.rm = TRUE)+
    geom_point(aes(y = y_actual))
}


# week_current = 1
# new_cases = 59

# hfmd_1324 <- hfmd_1324 |>
#   mutate(t = x,
#          week = week(adm_week),
#          year = year(adm_week)) |>
#   select(-x)

hfmd_1324 |>
  filter(year == 2023) |>
  ggplot(aes(x = t,y = n))+
  geom_line()

currnttt <- hfmd_1324 |>
  filter(t == 545)

## using
num_week_pred = 12
week_until = currnttt$week-1
year_until = 2023

t_train = hfmd_1324 |>
  filter(week == week_until & year == year_until) |>
  pull(t)

data_past <- hfmd_1324 |>
  filter(t <= t_train)

# hfmd_1324 |>
#   filter(t > t_train)

outputt <- hfmd_forecast_hcmc(data_past = data_past,
                              week_current = currnttt$week,
                              new_cases=currnttt$n,
                              num_week_pred = num_week_pred)

plot_pred(outputt)

bart_model_hfmd |> str()

model_df <- obj |>
  mutate(type = case_when(
    !is.na(y_train) ~ "train",
    .default = "predict"
  )) |>
  filter(type == "predict")

actual_df <- hfmd_1324 |>
  filter(t %in% model_df$t) |>
  pull(n)

outputt

cbind.data.frame(model_df,y_actual = actual_df)

outputt |>
  filter(!is.na(y_train) & t>400) |>
  # mutate(y_fit = bart_model_hfmd$yhat.train.mean) |>
  ggplot(aes(x = t))+
  geom_point(aes(y = y_train)) +
  geom_line(aes(y = y_train_mean))

bart_model_hfmd$yhat.train.mean

