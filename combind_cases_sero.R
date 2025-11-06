###

dataaaa <- hfmd %>% select(age,collection,pos) %>%
  mutate(pos = as.numeric(pos),
         collection = case_when(
           collection %in% c(6,7) ~ "1",
           !collection %in% c(6,7) ~ "2"
         ),
         age2 = cut(age,seq(0,15,by=0.25),right = FALSE))  %>%
  group_by(age2,collection) %>%
  count(pos) %>%
  pivot_wider(names_from = pos,values_from = n,names_prefix = "pos") %>%
  ungroup() %>%
  replace_na(list(pos1 = 0,pos0 = 0)) %>%
  arrange(collection) %>%
  mutate(total = pos0+pos1,
         ob_sp = pos1/total)

dataaaa$age3 <- dataaaa %>% pull(age2) %>%
  levels() %>% sub("^\\[?(.+),.*", "\\1",.) %>% as.numeric() %>% rep(2)


dataaaa %>%
  ggplot(aes(x = age2,y = ob_sp,color = collection,size = total))+
  geom_point()

# Simple Force of Infection (FOI) model with maternal immunity and waning
library(dplyr)
library(ggplot2)

# Parameters
omega <- 2       # maternal immunity waning rate (per year)
delta <- 0.25  # age step (in years)

# Define stepwise FOI for each year (example values)
lambda_t <- c("2019"=0.3, "2020"=0.05, "2021"=0.15, "2022"=0.25)

# Simulate ages 0–10
ages <- seq(0, 10, by = delta)
n_age <- length(ages)

# Initialize states at birth (t=2022)
M <- numeric(n_age)
X <- numeric(n_age)


# Initial conditions: all newborns maternally immune
M[1] <- 1
X[1] <- 0


# Define a function for yearly FOI by age
foi_by_time <- function(age) {
  # Assign each age to calendar year cohort, example logic
  if (age < 3) lambda_t["2022"]
  else if (age < 5) lambda_t["2021"]
  else if (age < 7) lambda_t["2020"]
  else lambda_t["2019"]
}

library(extraDistr)

lambda_t = rhnorm(n=1, sigma = 1)
omega = rnorm(n = 1,mean = 2,sd = 1)
M[1] = runif(n = 1,0,1)

# Recursive simulation
for (i in 2:n_age) {

  M[i] <- M[i-1] - (1 - exp(-omega * delta)) * M[i-1]
  X[i] <- X[i-1] + (1 - exp(-omega * delta)) * M[i-1] - (1 - exp(-lambda_t * delta)) * X[i-1]
}

# Combine
df <- data.frame(
  age = ages,
  seropositive = 1 - X
)

# Plot seroprevalence curve
ggplot(df, aes(x = age, y = seropositive)) +
  geom_line(size = 1.2, color = "blue") +
  labs(y = "Seroprevalence", x = "Age (years)",
       title = "Estimated Age-Seroprevalence Curve (Example FOI Model)") +
  theme_minimal()



# In R
library(rstan)

# Model file (simplified)
stan_code <- "
data {
  int<lower=1> N;           // number of age groups
  vector[N] age;
  int y[N];                 // number seropositive
  int n[N];                 // total tested
}
parameters {
  real<lower=0> lambda[4];  // FOI for each year period
  real<lower=0> rho;
  real<lower=0> nu;
}
model {
  vector[N] p;              // predicted seroprevalence
  for (i in 1:N) {
    // call same recursion as above, embedded in Stan loop
    // p[i] = predicted seroprevalence at age[i]
  }
  y ~ binomial(n, p);
}
"

fit <- stan(model_code = stan_code, data = list(...))


### SIR model

# --- Simulate model ---
simulate_SIR <- function(params, tmax = 572) {
  times <- seq(0, tmax, by = 1)
  out <- ode(y = c(S = 0.85, I = 1e-4, R = 0),
             times = times, func = sir_model,
             parms = params)
  as.data.frame(out)
}

sir_model <- function(t, state, params) {
  with(as.list(c(state, params)), {
    beta_t <- r0 * (1 + theta * cos(2 * pi * (t - phi) / 52)) * (1 - delta)
    dS <- mu - beta_t * S * (I + omega) - mu * S
    dI <- beta_t * S * (I + omega) - (gamma + mu) * I
    dR <- gamma * I - mu * R
    list(c(dS, dI, dR))
  })
}


params <- list(
  r0 = 2,   # baseline transmission rate
  theta = 0.2,     # seasonal amplitude
  phi = 1,     # seasonal offset
  delta = 0.5, # NPI effect
  mu = 1/(80*52),
  gamma = 1,
  phi = 0.0001,
  omega = 2e-6
)



sim <- simulate_SIR(params)

sim %>%
  ggplot(aes(x = time,y = I))+
  geom_line()+
  scale_x_continuous(breaks = seq(1,600,by=52))+
  ylim(c(0,1))



library(deSolve)
library(dplyr)
library(tidyr)
library(ggplot2)


### data ND1

ch1_adm <- df1 %>%
  filter(year(adm_date) == 2023 &
           medi_cen %in% c("Bệnh viện Nhi đồng 1",
                           "Bênh viện Nhi Đồng 1",
                           "Bệnh viện Nhi Đồng 1")) %>%
  mutate(district2 = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower())

wwww2 <- slide_age(time1 = ch1_adm$adm_date,
          age1 =  ch1_adm$age1,
          w1 = 7, s1=7)

ggplot(data=wwww2$wdat, aes(x=date, y=age)) +
  stat_density(
    aes(fill = after_stat(count)),
    geom = "raster",
    position = "identity",
    interpolate = TRUE
  )+
  scale_fill_paletteer_c("grDevices::Inferno")+
  # scale_fill_gradient(low="#040404FF", high= "#FFFE9EFF")+
  # scale_fill_distiller(palette = "Blues")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  # scale_x_discrete(name = "Admission week",labels = leb_month)+
  labs(tag = "D",fill = "Number of\nhospitalizations")

ch1_adm_time_series <- ch1_adm %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup()

ch1_adm_time_series %>%
  mutate(time = 1:nrow(.)) %>%
  ggplot(aes(x = time,y = n))+
  geom_point()

seir_model <- function(t, state, params) {
  with(as.list(c(state, params)), {
    N <- S + E + I + R
    beta_t <- beta
    dS <- mu * N - beta_t * S * I / N - mu * S
    dE <- beta_t * S * I / N - sigma * E - mu * E
    dI <- sigma * E - gamma * I - mu * I
    dR <- gamma * I - mu * R
    list(c(dS, dE, dI, dR))
  })
}

true_params <- list(
  beta  = .5,    # transmission rate
  sigma = 1,    # incubation rate (1/latent period)
  gamma = 1/52,    # recovery rate (1/infectious period)
  mu    = 1/(80*52)       # ignore demography for simplicity
)

init <- c(S = 10000, E = 1, I = 17, R = 0)
times <- seq(0, 52, by = 1)

out <- ode(y = init, times = times, func = seir_model, parms = true_params)
out <- as.data.frame(out)


out %>%
  ggplot(aes(x = time,y = I))+
  geom_line()


df1 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup() %>%
  mutate(time = 1:nrow(.)) %>%
  ggplot(aes(x = adm_week,y=n))+
  geom_point()+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")


