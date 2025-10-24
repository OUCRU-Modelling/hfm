source("./constraint_func.R")

# ------- Part 1: Simulate data, replace this part with your own data if available ---------
set.seed(12345)

n_rows <- 300
survey_months <- c("2022-12-01", "2023-04-01", "2023-08-01", "2023-12-01")
survey_months <- as.Date(survey_months)

# Function to sample random date within a month
random_date_in_month <- function(start_date, n) {
  start <- as.Date(format(start_date, "%Y-%m-01"))
  end <- seq(start, by = "1 month", length.out = 2)[2] - 1
  as.Date(runif(n, as.numeric(start), as.numeric(end)), origin = "1970-01-01")
}

sigmoid <- function(x, beta0 = -3, beta1 = 0.5) {
  1 / (1 + exp(-(beta0 + beta1 * x)))
}

simulated_data <- data.frame(
  time = unlist(lapply(survey_months, function(m) random_date_in_month(m, n_rows / length(survey_months)))),
  age = runif(n_rows, min = 0, max = 15) # random ages
)

# Probability of being seropositive follows a sigmoid in age
simulated_data$prob_sero <- sigmoid(simulated_data$age, beta0 = -3, beta1 = 0.5)

# Simulate binary outcome
simulated_data$seropositive <- rbinom(n_rows, size = 1, prob = simulated_data$prob_sero)

simulated_data <- simulated_data |> mutate(date = as.Date(time),
                                           col_time = rep(c(1:4),each = 75)) |> select(-c(time,prob_sero))

# ----- Part 2: Sample function usage ------------
# Use case 1: without age correct
time_age_out1 <- simulated_data %>%
  rename(status = seropositive) %>%
  time_age_model(grouping_col = "col_time", time_col = "date", age_correct = F)

# facet by group
plot.time_age_model(time_age_out1, modtype = "monotonized", cex = 1, facet = TRUE, le=512)
# no faceting
plot.time_age_model(time_age_out1, modtype = "monotonized", cex = 1, facet = FALSE, le=512)

# Use case 2: with age correct
time_age_out2 <- simulated_data %>%
  rename(status = seropositive) %>%
  time_age_model(grouping_col = "col_time", time_col = "date", age_correct = T)

# facet by group
plot.time_age_model(time_age_out2, modtype = "monotonized", cex = 1, facet = TRUE, le=512)
# facet by group
plot.time_age_model(time_age_out2, modtype = "monotonized", cex = 1, facet = FALSE, le=512)


