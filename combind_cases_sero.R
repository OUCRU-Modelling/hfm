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

library(deSolve)
library(bbmle)

run_seir <- function(beta0 = beta0, beta1 = beta1, omega = omega,
                     sigma = sigma, gamma = gamma, mu = mu,
                     S0 = S0, E0 = E0, I0 = I0, R0 = R0,
                     times = times) {

  seir_model <- function(t, state, params) {
    with(as.list(c(state, params)), {
      N <- S + E + I + R
      beta_t <- beta0 * (1 - beta1 * cos(omega * t))
      dS <- mu*N - beta_t * S * I/N - mu * S
      dE <- beta_t * S * I/N - sigma * E - mu * E
      dI <- sigma * E - gamma * I - mu * I
      dR <- gamma * I - mu * R
      list(c(dS, dE, dI, dR))
    })
  }

  true_params <- list(
    beta0  = beta0,
    beta1  = beta1,
    omega  = omega,
    sigma  = sigma,
    gamma  = gamma,
    mu     = mu
  )

  init <- c(S = S0, E = E0, I = I0, R = R0)

  out <- ode(y = init, times = times, func = seir_model, parms = true_params)
  out <- as.data.frame(out)
  out$C <- c(I0, diff(out$I + out$R))  # cumulative incidence
  out
}


mll <- function(beta0, beta1,omega,sigma) {
  # Make sure that parameters are positive
  beta0 <- exp(beta0)
  beta1 <- exp(beta1)
  omega <- exp(omega)
  sigma <- exp(sigma)
  # mu <- exp(mu)

  pred <- run_seir(beta0 = beta0, beta1 = beta1, omega = omega,sigma = sigma,
                   gamma = 1/52, mu = 1/(80*52),
                   S0 = 13000, E0 = 2, I0 = 1, R0 = 0,times = seq(0,52))
  pred <- pred$C
  llh <- lm(log(ch1_adm_time_series$n) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  - sum(dnorm(x = log(ch1_adm_time_series$n), mean = log(pred) + log(p), sd = sigma2))
}

starting_param_val <- list(beta0 = 0.3,beta1 = .5,sigma = .3,omega = (2*pi)/(52/2.2))
estimates <- mle2(minuslogl = mll, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))

out <- run_seir(beta0 = params[1], beta1 = params[2], omega = params[3],sigma = params[4],
                gamma = 1/52, mu = 1/(80*52),
                S0 = 13000, E0 = 2, I0 = 1, R0 = 0,times = seq(0,52))

out %>%
  mutate(beta_t = params[1]*(1 - params[2]*cos(params[3]*time)),
         foi = beta_t*I,
         date = ch1_adm_time_series$adm_week) %>%
  ggplot(aes(x = date))+
  geom_line(aes(y=C))+
  geom_line(aes(y=beta_t*1000),linetype = "dashed")+
  geom_line(aes(y=S/10),color = "blue",alpha = .3)+
  # geom_line(aes(y=foi),linetype = "dotted")+
  geom_point(aes(x = adm_week,y=n),
             data = ch1_adm_time_series)+
  # scale_y_continuous(
  #   name = "Incidence",
  #   sec.axis = sec_axis(~ . /1000, name = "Beta(t)")
  # )+
  geom_vline(xintercept = as.Date("2023-05-21"))+
  geom_vline(xintercept = as.Date("2023-09-10"))+
  annotate(
    geom = "text", x = as.Date("2023-06-12"), y = 1300,
    label = "Summer break", hjust = 0, vjust = 1, size = 10
  )+
  theme_minimal()

### combind with serology data

mll2 <- function(beta0, beta1,omega,sigma) {
  # Make sure that parameters are positive
  beta0 <- exp(beta0)
  beta1 <- exp(beta1)
  omega <- exp(omega)
  sigma <- exp(sigma)
  # mu <- exp(mu)

  out <- run_seir(beta0 = beta0, beta1 = beta1, omega = omega,sigma = sigma,
                   gamma = 1/52, mu = 1/(80*52),
                   S0 = 13000, E0 = 2, I0 = 1, R0 = 0,times = seq(0,18))
  pred <- out %>% filter(time != 0) %>% pull(C)
  llh <- lm(log(ch1_adm_time_series$n[1:length(pred)]) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  L1 <- - sum(dnorm(x = log(ch1_adm_time_series$n[1:length(pred)]), mean = log(pred) + log(p), sd = sigma2))

  ### serology

  lambda <- out %>%
    mutate(beta_t = beta0*(1 - beta1*cos(omega*time)),
           foi = beta_t*I) %>%
    pull(foi)

  sero_conv_prob <- function(t1, t3, lambda, dt = 1) {
    idx <- seq(t1, t3, by = dt)
    risk <- sum(lambda[idx]) * dt
    p_conv <- 1 - exp(-risk)
    return(p_conv)
  }

  L2 <- -sum(dbinom(15, 100, sero_conv_prob(1, 17, lambda), log = TRUE))

  return(L1 + L2)
}

starting_param_val <- list(beta0 = 0.5,beta1 = .05,sigma = .3,omega = (2*pi)/(52/2.2))
estimates <- mle2(minuslogl = mll2, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))

out <- run_seir(beta0 = params[1], beta1 = params[2], omega = params[3],sigma = params[4],
                gamma = 1/52, mu = 1/(80*52),
                S0 = 13000, E0 = 2, I0 = 1, R0 = 0,times = seq(0,17))

pred <- out %>% filter(time != 0) %>% pull(C)
llh <- lm(log(ch1_adm_time_series$n[1:length(pred)]) ~ 1 + offset(log(pred)))
p <- coef(llh) %>% as.numeric()
sigma2 <- summary(llh)$sigma

# Return the negative log-likelihood
L1 <- - sum(dnorm(x = log(ch1_adm_time_series$n[1:length(pred)]), mean = log(pred) + log(p), sd = sigma2))

### serology

lambda <- out %>%
  mutate(beta_t = beta0*(1 - beta1*cos(omega*time)),
         foi = beta_t*I) %>%
  pull(foi)

sero_conv_prob <- function(t1, t3, lambda, dt = 1) {
  idx <- seq(t1, t3, by = dt)
  risk <- sum(lambda[idx]) * dt
  p_conv <- 1 - exp(-risk)
  return(p_conv)
}

L2 <- -sum(dbinom(15, 100, sero_conv_prob(1, 17, lambda), log = TRUE))


out %>%
  mutate(beta_t = params[1]*(1 - params[2]*cos(params[3]*time)),
         foi = beta_t*I,
         # date = ch1_adm_time_series$adm_week
         ) %>%
  ggplot(aes(x = time))+
  geom_line(aes(y=C))+
  # geom_line(aes(y=beta_t*1000),linetype = "dashed")+
  # geom_line(aes(y=S/10),color = "blue",alpha = .3)+
  geom_line(aes(y=foi),linetype = "dotted")+
  # geom_point(aes(x = adm_week,y=n),
  #            data = ch1_adm_time_series)+
  # scale_y_continuous(
  #   name = "Incidence",
  #   sec.axis = sec_axis(~ . /1000, name = "Beta(t)")
  # )+
  # geom_vline(xintercept = as.Date("2023-05-21"))+
  # geom_vline(xintercept = as.Date("2023-09-10"))+
  # annotate(
  #   geom = "text", x = as.Date("2023-06-12"), y = 1300,
  #   label = "Summer break", hjust = 0, vjust = 1, size = 10
  # )+
  theme_minimal()



lambda <- out %>%
  mutate(beta_t = .3*(1 + .7*cos((2*pi)/(52/2.2)*time)),
         foi = beta_t*I,
         date = ch1_adm_time_series$adm_week) %>%
  pull(foi)


sero_conv_prob <- function(t1, t3, lambda, dt = 1) {
  idx <- seq(t1, t3, by = dt)
  risk <- sum(lambda[idx]) * dt
  p_conv <- 1 - exp(-risk)
  return(p_conv)
}

model_simulated <- out %>%
  mutate(beta_t = .3*(1 + .7*cos((2*pi)/(52/2.2)*time)),
         foi = beta_t*I,
         date = ch1_adm_time_series$adm_week)

# viro_res <- viro2 %>%
#   mutate(adm_week = floor_date(admission_date, "week")) %>%
#   group_by(adm_week) %>%
#   count(pos) %>%
#   ungroup() %>%
#   pivot_wider(names_from = pos,values_from = n,names_prefix = "EV_A71_") %>%
#   replace(is.na(.),0) %>%
#   mutate(total = EV_A71_TRUE+EV_A71_FALSE,
#          per = EV_A71_TRUE/total) %>%
#   select(adm_week,per)

sp_each <- data.frame()
for (i in 1:3){
  sp_each1 <- hfmd %>%
    filter(collection %in% c(5+i,6+i) & age <= 6) %>%
    count(pos) %>%
    pivot_wider(names_from = pos,values_from = n,names_prefix = "EV_A71_") %>%
    mutate(total = EV_A71_TRUE+EV_A71_FALSE,
           sp = EV_A71_TRUE/total,
           col_time = i) %>%
    select(col_time, sp)
  sp_each <- rbind(sp_each,sp_each1)
}

sp_each


sero_conv_prob(1, 17, lambda_t)

sero_conv_prob(13, 34, lambda_t)

sero_conv_prob(31, 52, lambda_t)


-sum(dbinom(15, 100, sero_conv_prob(1, 17, lambda_t), log = TRUE))


left_join(model_simulated,viro_res,by = join_by(date == adm_week))



### time series hfmd

hfmd_1224 <- df1 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup() %>%
  mutate(time = 1:nrow(.))

run_sir <- function(beta0 = beta0, beta1 = beta1, omega = omega,
                    gamma = gamma, mu = mu,
                    S0 = S0, I0 = I0, R0 = R0, C0 = C0,
                    times = times) {

  sir_model <- function(t, state, params) {
    with(as.list(c(state, params)), {
      N <- S + E + I + R
      beta_t <- beta0 * (1 + beta1 * cos(omega*t))
      dS <- mu*N - beta_t * S * I/N - mu * S
      dI <- beta_t * S * I/N - gamma * I - mu * I
      dR <- gamma * I - mu * R
      dC <- beta_t * S * I/N
      list(c(dS, dI, dR, dC))
    })
  }

  true_params <- list(
    beta0  = beta0,
    beta1  = beta1,
    omega  = omega,
    # sigma  = sigma,
    gamma  = gamma,
    mu     = mu
  )

  init <- c(S = S0, I = I0, R = R0, C0 = C0)

  out <- ode(y = init, times = times, func = sir_model, parms = true_params)
  out <- as.data.frame(out)
  out$C <- c(I0, diff(out$C))  # cumulative incidence
  out
}


mll <- function(beta0,beta1,sigma,omega) {
  # Make sure that parameters are positive
  beta0 <- exp(beta0)
  beta1 <- exp(beta1)
  sigma <- exp(sigma)
  omega <- exp(omega)
  # mu <- exp(mu)

  pred <- run_seir(beta0 = beta0,beta1 = beta1,sigma = sigma,omega = omega,
                   gamma = 1/365, mu = 10/365,
                   S0 = 500000, E0 = 1000, I0 = 1000,C0 = 0,times = seq(0,597))
  pred <- pred %>% filter(time != 0) %>% pull(C)
  llh <- lm(log(hfmd_1224$n) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  - sum(dnorm(x = log(hfmd_1224$n), mean = log(pred) + log(p), sd = sigma2))
}

starting_param_val <- list(beta0 = .075,beta1 = 1.3,sigma = 0.07,omega = 28)
estimates <- mle2(minuslogl = mll, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))


# out <- run_seir(beta0 = .6,beta1 = 2,sigma = 0.01,omega = 30,
#                 gamma = 7/365, mu = 5/365,
#                 S0 = 400000, E0 = 3000, I0 = 1000, R0 = 0, C0 = 0,times = seq(0,597))

run <- function(beta0 = .6,beta1 = 2,sigma = 0.01,omega = 30,
                gamma = 7/365, mu = 5/365,
                S0 = 400000, E0 = 3000, I0 = 1000){
  run_seir(beta0 = beta0, beta1 = beta1, omega = omega,
           sigma = sigma, gamma = gamma, mu = mu,
           S0 = S0, E0 = E0, I0 = I0, R0 = 0, C0 = 0,times = seq(0,1000)) %>%
    filter(time != 0) %>%
    ggplot(aes(x = time))+
    geom_line(aes(y=C))+
    geom_line(aes(y = S/100),color = "blue")+
    geom_point(aes(y=n),data = hfmd_1224) +
    # xlim(c(20,600))+
    # ylim(c(-2000,3000))
    {}
}

ggplot()+
geom_point(aes(x = time,y=n),data = hfmd_1224)

run(beta0 = .1,beta1 = 1.3,sigma = 0.04,omega = 2*pi/52,
    gamma = 2/365, mu = 10/365,
    S0 = 400000, E0 = 2000, I0 = 1000)


run_seir(beta0 = .1,beta1 = 1.3,omega = 2*pi/52,
         gamma = 2/365, mu = 10/365,
         S0 = 400000, I0 = 1000, R0 = 0, C0 = 0,times = seq(0,1000))

ggplot() +
geom_point(aes(x = time,y=n),data = hfmd_1224)


### from sratch


library(odin2)
library(dust2)

sir_seasonality <- odin2::odin({
  N <- S + I + R
  deriv(S) <- mu*N - (beta_t*(I/N) + mu)*S
  deriv(I) <- beta_t*(I/N)*S - (mu + gamma)*I
  deriv(R) <- gamma*I - R*mu
  deriv(CInc) <- beta_t*(I/N)*S

  # seasonality forcing
  beta_0 <- parameter(0.4)
  beta_1 <- parameter(0)
  omega <- parameter(2*3.14/52) # use week as time unit by default
  beta_t <- beta_0*(1 + beta_1*cos(omega*time))

  # initialize starting population
  init_S <- parameter(9500)
  init_I <- parameter(500)
  gamma <- parameter(0.05)
  mu <- parameter(0.05)

  initial(S) <- init_S
  initial(I) <- init_I
  initial(R) <- 0
  initial(CInc) <- 0

})

run_mod <- function(mod, pars, duration=100, timestep=1){
  # --- initialize simulation time ----
  times <- seq(0, duration, timestep)

  sys <- dust_system_create(mod, pars)
  dust_system_set_state_initial(sys)
  out <- dust_system_simulate(sys, times)
  out <- dust_unpack_state(sys, out)

  out <- out %>%
    as.data.frame() %>%
    mutate(
      t = times
    )

  out$Inc <- c(I0, diff(out$CInc))
  out
}

pars <- list(
  beta_1 = .5,
  beta_0 = 0.3,
  gamma = 1/52,
  mu = 1/(80*52),
  omega = 2*3.14/(52/2.2),
  init_S = 13000,
  init_I = 1
)

run_mod(sir_seasonality, pars, duration=52, timestep=1) %>%
  mutate(date = ch1_adm_time_series$adm_week) %>%
  ggplot(aes(x = date,y = Inc))+
  geom_line()+
  geom_point(aes(x = adm_week,y=n),
             data = ch1_adm_time_series)

mll <- function(beta_0, beta_1,omega) {
  # Make sure that parameters are positive
  beta_0 <- exp(beta_0)
  beta_1 <- exp(beta_1)
  omega <- exp(omega)
  # gamma <- exp(gamma)

  pars <- list(
    beta_1 = beta_1,
    beta_0 = beta_0,
    gamma = 1/52,
    mu = 1/(80*52),
    omega = omega,
    init_S = 13000,
    init_I = 1
  )

  pred <- run_mod(sir_seasonality, pars, duration=53, timestep=1)
  # Return the negative log-likelihood
  pred <- pred %>%  filter(t != 0) %>% pull(Inc)
  llh <- lm(log(ch1_adm_time_series$n) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  - sum(dnorm(x = log(ch1_adm_time_series$n), mean = log(pred) + log(p), sd = sigma2))
}

starting_param_val <- list(beta_1 = .5,
                           beta_0 = 0.3,
                           # gamma = 1/52,
                           # mu = 1/(80*52),
                           omega = 2*3.14/(52/2.2))
estimates <- mle2(minuslogl = mll, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))

run_mod(sir_seasonality,
        pars = list(
          beta_0 = params[1],
          beta_1 = params[2],
          gamma = 1/52,
          mu = 1/(80*52),
          omega = params[3],
          init_S = 13000,
          init_I = 1
        ),
        duration=53, timestep=1)%>%
  filter(t != 0) %>%
  cbind(ch1_adm_time_series) %>%
  ggplot(aes(x = adm_week,y = Inc))+
  geom_line()+
  geom_point(aes(x = adm_week,y=n),
             data = ch1_adm_time_series)

### seir

seir_seasonality <- odin2::odin({
  N <- S + E + I + R
  deriv(S) <- mu * N - beta_t * S * I / N - mu * S
  deriv(E) <- beta_t * S * I / N - sigma * E - mu * E
  deriv(I) <- sigma * E - gamma * I - mu * I
  deriv(R) <- gamma * I - mu * R
  deriv(CInc) <- sigma * E

  # seasonality forcing
  beta_0 <- parameter(0.4)
  beta_1 <- parameter(0)
  omega <- parameter(2*3.14/52) # use week as time unit by default
  sigma <- parameter(0.2)
  beta_t <- beta_0*(1 + beta_1*cos(omega*time))

  # initialize starting population
  init_S <- parameter(9500)
  init_I <- parameter(500)
  init_E <- parameter(500)
  gamma <- parameter(0.05)
  mu <- parameter(0.05)

  initial(S) <- init_S
  initial(E) <- init_E
  initial(I) <- init_I
  initial(R) <- 0
  initial(CInc) <- 0

})

mll_seir <- function(beta_0, beta_1,omega,sigma) {
  # Make sure that parameters are positive
  beta_0 <- exp(beta_0)
  beta_1 <- exp(beta_1)
  omega <- exp(omega)
  sigma <- exp(sigma)

  pars <- list(
    beta_1 = beta_1,
    beta_0 = beta_0,
    omega = omega,
    sigma = sigma,
    gamma = 1/52,
    mu = 1/(80*52),
    init_S = 13000,
    init_E = 1,
    init_I = 1
  )

  pred <- run_mod(seir_seasonality, pars, duration=53, timestep=1)
  # Return the negative log-likelihood
  pred <- pred %>%  filter(t != 0) %>% pull(Inc)
  llh <- lm(log(ch1_adm_time_series$n) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  - sum(dnorm(x = log(ch1_adm_time_series$n), mean = log(pred) + log(p), sd = sigma2))
}

starting_param_val <- list(beta_1 = .5,
                           beta_0 = 0.3,
                           # gamma = 1/52,
                           # mu = 1/(80*52),
                           sigma = .3,
                           omega = 2*3.14/(52/2.2))

estimates <- mle2(minuslogl = mll_seir, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))

out <- run_mod(seir_seasonality,
        pars = list(
          beta_0 = params[1],
          beta_1 = params[2],
          sigma = params[4],
          gamma = 1/52,
          mu = 1/(80*52),
          omega = params[3],
          init_S = 13000,
          init_E = 1,
          init_I = 1
        ),
        duration=53, timestep=1)%>%
  filter(t != 0) %>%
  cbind(ch1_adm_time_series) %>%
  mutate(beta_t = params[1]*(1 + params[2]*cos(params[3]*t)),
         N = S+E+I+R,
         foi = beta_t*(I/N))
out %>%
  ggplot(aes(x = adm_week))+
  geom_line(aes(y = Inc))+
  # geom_line(aes(y=beta_t*1000),linetype = "dashed")+
  # geom_line(aes(y=S/10),color = "blue",alpha = .3)+
  geom_line(aes(y=foi*1500),linetype = "dotted")+
  geom_point(aes(y=n))+
  geom_vline(xintercept = as.Date("2023-05-21"))+
    geom_vline(xintercept = as.Date("2023-09-10"))+
    annotate(
      geom = "text", x = as.Date("2023-06-12"), y = 1300,
      label = "Summer break", hjust = 0, vjust = 1, size = 10
    )+
    theme_minimal()

lambda_t <- run_mod(seir_seasonality,
        pars = list(
          beta_0 = params[1],
          beta_1 = params[2],
          sigma = params[4],
          gamma = 1/52,
          mu = 1/(80*52),
          omega = params[3],
          init_S = 13000,
          init_E = 1,
          init_I = 1
        ),
        duration=53, timestep=1)%>%
  filter(t != 0) %>%
  cbind(ch1_adm_time_series) %>%
  mutate(beta_t = params[1]*(1 + params[2]*cos(params[3]*t)),
         N = S+E+I+R,
         foi = beta_t*(I/N)) %>%
  select(foi,t)

get_seroconversion <- function(df) {

  df_sub <- df %>% filter(t %in% c(1:nrow(df)))

  # Integrate lambda(t) over time using the trapezoidal rule
  risk <- sum(diff(df_sub$t) * zoo::rollmean(df_sub$foi, 2))

  # Convert cumulative hazard to probability
  p_conv <- 1 - exp(-risk)
  return(p_conv)
}

get_seroconversion(lambda_t,1,17)

get_seroconversion(lambda_t,13,34)

get_seroconversion(lambda_t,31,52)


sero_consider <- hfmd %>% filter(collection %in% c(7))

sero_obs <- sero_consider %>%
  filter(age <= 6) %>%
  count(pos) %>%
  pivot_wider(names_from = pos,values_from = n,names_prefix = "pos") %>%
  mutate(total = posTRUE+posFALSE,
         sp = posTRUE/total) %>%
  pull(sp)

seroconversion_model <- out %>%
  filter(adm_week >= range(sero_consider$col_date)[1] &
           adm_week <= range(sero_consider$col_date)[2]) %>%
  get_seroconversion()

0.1666667 - 0.1333333

-dbinom(round((0.1666667-0.1333333)*100),100,prob = seroconversion_model,log = T)
?dnorm



dbinom(46:54, 100, 0.5)
## fit for 2022-2023

ch1_adm_2123 <- df1 %>%
  filter(year(adm_date) %in% c(2021,2022,2023) &
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
           tolower()) %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup() %>%
  filter(adm_week > as.Date("2021-06-06"))

mll_seir <- function(beta_0, beta_1,omega,sigma,gamma,mu) {
  # Make sure that parameters are positive
  beta_0 <- exp(beta_0)
  beta_1 <- exp(beta_1)
  omega <- exp(omega)
  sigma <- exp(sigma)
  gamma <- exp(gamma)
  mu <- exp(mu)

  pars <- list(
    beta_1 = beta_1,
    beta_0 = beta_0,
    omega = omega,
    sigma = sigma,
    gamma = gamma,
    mu = mu,
    init_S = 15000,
    init_E = 1,
    init_I = 1
  )

  pred <- run_mod(seir_seasonality, pars, duration=nrow(ch1_adm_2123-1), timestep=1)
  # Return the negative log-likelihood
  pred <- pred %>%  filter(t != 0) %>% pull(Inc)
  llh <- lm(log(ch1_adm_2123$n) ~ 1 + offset(log(pred)))
  p <- coef(llh) %>% as.numeric()
  sigma2 <- summary(llh)$sigma

  # Return the negative log-likelihood
  - sum(dnorm(x = log(ch1_adm_2123$n), mean = log(pred) + log(p), sd = sigma2))
}

starting_param_val <- list(beta_0 = 0.9,
                           beta_1 = 1,
                           sigma = .8,
                           omega = 0.8,
                           gamma = 12/52,
                           mu = 25/(80*52))

estimates <- mle2(minuslogl = mll_seir, start = lapply(starting_param_val, log), method = "Nelder-Mead")
params <- exp(coef(estimates))
params

run_mod(seir_seasonality,
        pars = list(
          beta_0 = params[1],
          beta_1 = params[2],
          sigma = params[4],
          omega = params[3],
          gamma = params[5],
          mu = params[6],
          init_S = 15000,
          init_E = 1,
          init_I = 1
        ),
        duration=nrow(ch1_adm_2123-1), timestep=1)%>%
  filter(t != 0) %>%
  cbind(ch1_adm_2123) %>%
  ggplot(aes(x = adm_week,y = Inc))+
  geom_line()+
  geom_point(aes(y=n))


run_mod(seir_seasonality,
        pars = list(
          beta_0 = 0.9,
          beta_1 = 1,
          sigma = .8,
          omega = 0.8,
          gamma = 12/52,
          mu = 25/(80*52),
          init_S = 15000,
          init_E = 1,
          init_I = 1
        ),
        duration=nrow(ch1_adm_2123-1), timestep=1)%>%
  filter(t != 0) %>%
  cbind(ch1_adm_2123) %>%
  ggplot(aes(x = adm_week,y = Inc))+
  geom_line()+
  geom_point(aes(y=n))

