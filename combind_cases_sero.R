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

## age distribution of CH1 admission
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

########

ch1_adm_time_series <- ch1_adm %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup()

ch1_adm_time_series %>%
  mutate(time = 1:nrow(.)) %>%
  ggplot(aes(x = time,y = n))+
  geom_point()

### code model deSolve
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


### time series hfmd

hfmd_1224 <- df1 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup() %>%
  mutate(time = 1:nrow(.))

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

  out$Inc <- c(pars$init_I, diff(out$CInc))
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

## fit for 2022-2023

ch1_adm_2123 <- df1 %>%
  filter(year(adm_date) %in% c(2022,2023) &
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

seir_seasonality <- odin2::odin({
  N <- S + E + I + R
  deriv(S) <- b - beta_t * S * I / N - mu * S
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
  b <- parameter(2000)

  initial(S) <- init_S
  initial(E) <- init_E
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

  out$Inc <- c(1, diff(out$CInc))
  out
}

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
    init_S = 28000,
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

starting_param_val <- list(beta_0 = .9,
                           beta_1 = 1.6,
                           sigma = .1,
                           omega = .2,
                           gamma = 4/52,
                           mu = 1/(80*52))

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

parms <- list(
  beta_0 = 0.5,
  beta_1 = .5,
  sigma = 0.5,
  omega = 2*pi/52*2,
  gamma = 1/52,
  mu = 1/52,
  init_S = 13000,
  init_E = 1,
  init_I = 1,
  b = 100
)

run_mod(seir_seasonality,
        pars = parms,
        duration=nrow(ch1_adm_time_series-1), timestep=1) %>%
  filter(t != 0) %>%
  cbind(ch1_adm_time_series) %>%
  mutate(N = S+E+I+R,
         s_pro = S/N,
         beta = parms$beta_0*(1 + parms$beta_1*cos(parms$omega*t))) %>%
  ggplot(aes(x = adm_week))+
  geom_line(aes(y = Inc))+
  geom_line(aes(y = s_pro*4000),color = "blue")+
  geom_line(aes(y = beta*6000),linetype = "dashed")+
  geom_point(aes(y=n))+
  scale_x_datetime(breaks = "1 year",date_labels = "%Y" )


hfmd_1224 <- df1 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup() %>%
  mutate(time = 1:nrow(.))

model$df %>%
  mutate(time2 = sprintf("%04d-W%02d-1", year, week),
         time2 = ISOweek2date(time2)) %>%
  filter(year(time2) >= 2023) %>%
  pull(fit) %>% mean()

## term time

sir_term <- odin2::odin({
  N <- S + I + R
  deriv(S) <- b*N - (beta_t*(I/N) + b)*S
  deriv(I) <-beta_t*(I/N)*S - (b + gamma)*I
  deriv(R) <- gamma*I - R*b

  # seasonality forcing
  beta_0 <- parameter(0.4)
  beta_1 <- parameter(0)

  school_open <- parameter()
  school_time <- parameter()
  school_data_dim <- parameter()
  dim(school_time) <- school_data_dim
  dim(school_open) <- school_data_dim

  term <- interpolate(school_time, school_open, "constant")
  beta_t <- beta_0*(1 + beta_1*term)

  # initialize starting population
  init_S <- parameter(9500)
  init_I <- parameter(500)
  gamma <- parameter(0.05)
  b <- parameter(0.05)

  initial(S) <- init_S
  initial(I) <- init_I
  initial(R) <- 0

})

format_schooldays <- function(school_time, school_open){
  prev <- 0
  school_days <- list()
  holidays <- list()
  sapply(1:length(school_time), \(i){
    if(school_open[i] == -1 && school_time[i]>0){
      school_days[[length(school_days) + 1]] <<- list(min = prev, max = school_time[i]-1)
      prev <<- school_time[i]
    }else if (school_open[i] == 1 && school_time[i]>0){
      holidays[[length(holidays) + 1]] <<- list(min = prev, max = school_time[i]-1)
      prev <<- school_time[i]
    }
  })

  # handle cases when the end of school time is not the end of the year
  if(prev < 365){
    if(school_open[length(school_open)] == -1){
      holidays[[length(holidays) + 1]] <- list(min = prev, max = 365)
    }else{
      school_days[[length(school_days) + 1]] <- list(min = prev, max = 365)
    }
  }

  list(
    school_days = school_days,
    holidays = holidays
  )
}

c("2014-05-31","2014-08-01","2015-05-31","2015-08-01","2016-05-27",
  "2016-08-15","2017-05-31","2017-08-14","2018-05-31","2018-08-20",
  "2019-05-25","2019-09-05","2020-07-31","2020-09-01","2021-05-31",
  "2021-09-05","2022-06-30","2022-09-05","2023-05-26","2023-08-21")

hfmd_1224

school_time <- c(0, 7, 100, 116, 200, 252, 300,308,356)
school_open <- c(-1, 1, -1, 1,   -1,   1,   -1, 1,-1)

# format school data for generating annotation layer
school_data <- format_schooldays(school_time, school_open)

term_pars <- list(
  school_time = school_time,
  school_open = school_open,
  school_data_dim = length(school_time),
  beta_0 = 0.5,
  beta_1 = 0.25,
  gamma = 1/52,
  b = 1/(80*52),
  init_S = 20000,
  init_I = 1
)

run_mod(sir_term, term_pars, duration = nrow(hfmd_1224-1), timestep = 1) %>%
  filter(t != 0) %>%
  cbind(hfmd_1224) %>%
  ggplot(aes(x = adm_week)) +
  geom_line(aes(y = I), color = "cornflowerblue") +
  # get_annotate_layers(school_data$holiday, alpha = 0.3)+
  geom_point(aes(y = n))
