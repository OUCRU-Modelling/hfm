## serocatalytic model type

### age-dependent foi

# variables
deriv(S)<--lambda*S+mu*X
deriv(X) <- lambda*S-mu*X

# initial conditions of the variables
initial(S)<-1-X0
initial(X) <- X0

# parameter values
X0 <- user(0)
lambda <- if(t>=5 && t<=12) lambda_1 else lambda_2
lambda_1<-user(.1)
lambda_2<-user(.04)
mu<-user(.02) #rate of seroreversion


### antibodies reversion

# variables
deriv(S)<--lambda*S+mu*X
deriv(X) <- lambda*S-mu*X

# initial conditions of the variables
initial(S)<-1-X0
initial(X) <- X0

# parameter values
X0 <- user(0)
lambda <- user(0.1) #FOI
mu<-user(.02) #rate of seroreversion

### constant foi

# variables
deriv(X) <- lambda*(1-X)

# initial conditions of the variables
initial(X) <- X0

# parameter values
X0 <- user(0)             # initial proportion seropositive at birth
lambda <- user(0.1)       # FOI

# additional output
output(X_analytical) <- 1-exp(-lambda*t) #here 't' is used to denote age

### maternal antibodies

# variables
deriv(M)<--phi*M
deriv(S)<-phi*M-lambda*S
deriv(X) <- lambda*S

# initial conditions of the variables
initial(M)<-M0
initial(S)<-1-(M0+X0)
initial(X) <- X0

# parameter values
M0<-user(1) # proportion of births with maternal antibodies
X0 <- user(0)
lambda <- user(0.1) #FOI
phi<-user(2) #rate of maternal antibody loss

output(Total_seropos)<-1-S #overall proportion of the population that is seropositive


#####

hfmd_sero <- readRDS("D:/OUCRU/hfmd/data/hfmd_sero.rds") %>%
  as_tibble() |>
  mutate(collection = id |>
           str_remove(".*-") |>
           as.numeric() |>
           divide_by(1e4) |>
           round(),
         col_date2 = as.numeric(col_date),
         across(pos, ~ .x > 0))

hfmd_obs1 <- hfmd_sero %>%
  filter(collection == 6) %>%
  mutate(age2 = cut(age,breaks = seq(0,15),right = FALSE)) %>%
  group_by(age2) %>%
  count(pos) %>%
  ungroup() %>%
  pivot_wider(names_from = pos,values_from = n,names_prefix = "pos_") %>%
  replace(is.na(.),0) %>%
  mutate(n_sample = pos_TRUE+pos_FALSE,
         age_min = 0:14,
         age_max = age_min,
         n_seropositive = pos_TRUE,
         survey_year = 2023) %>%
  select(survey_year,n_sample,n_seropositive,age_min,age_max)


# install.packages("serofoi")
# library(serofoi)

plot_serosurvey(hfmd_obs1, size_text = 15,bin_step = 2)

seromodel <- fit_seromodel(serosurvey = hfmd_obs1,
                           model_type = "age",
                           iter = 10000,
                           warmup = 1000,
                           chains = 4)

plot_seromodel(
  seromodel,
  serosurvey = hfmd_obs1,
  # foi_df = foi_df,
  size_text = 6
)

seromodel %>% str()


