df_cases <- paste0(path2data, cases_file) |>
  readRDS()

df_cases <- df_cases |>
  mutate(reported_date = as.Date(reported_date),
         onset_date = as.Date(onset_date)) |>
  distinct(.keep_all = TRUE)  |>
  mutate(
    onset_date = ifelse(
      onset_date > reported_date & day(onset_date) == month(reported_date),
      ydm(onset_date),
      ymd(onset_date)
      ),
    onset_date = as.Date(onset_date),
    adm_date = ifelse(
      adm_date > reported_date & day(adm_date) == month(reported_date),
      ydm(adm_date),
      ymd(adm_date)
    ),
    adm_date = as.Date(adm_date),
    severity = case_when(
      severity %in% c("2a","2A","2","a","A","-2") ~ "2A",
      severity %in% c("1","11","1a") ~ "1",
      severity %in% c("2b","2B","2b nhom2","B","b") ~ "2B",
      severity == "3" ~ "3",
      severity == "4" ~ "4")
    )

saveRDS(df_cases,paste0(path2data, "hfmd_full.rds"))

df_cases |>
  mutate(delay = reported_date - adm_date) |>
  filter(delay > 10) |>
  select(reported_date,onset_date,adm_date,delay) |>
  mutate(
    reported_date2 = update(reported_date, year = year(adm_date)),
    delay2 = reported_date2 - adm_date
    ) |>
  # select(reported_date,onset_date,adm_date,delay) |>
  filter(delay2 < 50 & delay2 > 0)


df_cases |>
  mutate(delay = reported_date - adm_date) |>
  ifelse(delay > 50,reported_date2 = update(reported_date, year = year(adm_date)))


onset_date > reported_date & day(onset_date) == month(reported_date)

df_cases |>
  mutate(delay = adm_date - onset_date) |>
  filter(delay < 0 & day(onset_date) == month(adm_date) | month(onset_date) == day(adm_date)) |>
  select(reported_date,onset_date,adm_date,delay) |> View()


  group_by(delay) |>
  count() |>
  ungroup() |>
  ggplot(aes(x = delay, y = n))+
  geom_col() +
  theme_minimal()


df_cases |>
  mutate(delay = onset_date - adm_date) |>
  ggplot(aes(delay))+
  geom_histogram()

df_cases |>
  mutate(delay = reported_date - adm_date) |>
  filter(delay == 30*6) |> View()


###

df_cases |>
  mutate(check = year(reported_date) == year(admission_date),
         check2 = year(admission_date) != year(onset_date)) |>
  filter(check == TRUE & check2 == TRUE)

df_cases |>
  mutate(check = year(reported_date) == year(admission_date),
         check2 = year(reported_date) != year(onset_date)) |>
  filter(check == TRUE & check2 == TRUE)


df_cases |>
  mutate(delay = reported_date - admission_date) |>
  group_by(delay) |>
  count() |>
  summary()


df_cases |>
  mutate(delay = reported_date - admission_date) |>
  group_by(delay) |>
  count()

df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(reported_date != as.Date("2018-03-09")) |>
  group_by(delay) |>
  count() |>
  ggplot(aes(x = delay, y = n))+
  geom_col()

df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(reported_date != as.Date("2018-03-09")) |>
  group_by(delay) |>
  count()


df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(delay > 365) |>
  mutate(
    reported_date2 = update(reported_date, year = year(admission_date)),
    delay2 = reported_date2 - admission_date
  ) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2) |>
  filter(delay2 <= 14 & delay2 > 0) |>
  mutate(check = year(admission_date) == year(onset_date)) |>
  group_by(check) |>
  count()


df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(delay > 365) |>
  mutate(
    reported_date2 = update(reported_date, year = year(admission_date)),
    delay2 = reported_date2 - admission_date
  ) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2) |>
  mutate(check = year(admission_date) == year(onset_date)) |>
  filter(check == FALSE & delay2 <= 14 ) |>
  ggplot(aes(delay2))+
  geom_histogram(bins = 30)+
  scale_x_continuous(breaks = seq(0,70,by = 15),limits = c(0,70))

df_cases |>
  mutate(delay = reported_date - admission_date) |>  ## add delay variable
  filter(delay > 365) |>                             ## filter cases had delay > 1 year
  mutate(                                            ## change reported year = admission year
    reported_date2 = update(reported_date, year = year(admission_date)),
    delay2 = reported_date2 - admission_date         ## add delay2 variable to see new reported day
  ) |>                                               ## makes sense
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2) |>
  filter(delay2 < 0 ) |>               ## filter the day with delay2 less than 2 weeks
  mutate(check = year(admission_date) == year(onset_date))

df_cases |>
  mutate(delay = reported_date - admission_date,
         reported_date2 = update(reported_date, year = year(admission_date)),
         delay2 = reported_date2 - admission_date,
         check = year(admission_date) == year(onset_date))  |>
  mutate(reported_date = case_when(
    delay > 365 & delay2 <= 14 & delay2 > 0 & check %in% c("TRUE",NA) ~ reported_date2,
    .default = reported_date
  )) |>
  filter(delay > 365 & delay2 <= 14 & delay2 > 0 & check %in% c("TRUE",NA)) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2)

## cases were enter wrong admission year (same report and admission year)

df_cases |>
  mutate(delay = reported_date - admission_date,
         reported_date2 = update(reported_date, year = year(admission_date)),
         delay2 = reported_date2 - admission_date,
         check = year(admission_date) == year(onset_date))  |>
  mutate(reported_date = case_when(    ## those case match criteria will be changed
    delay > 365-14 & delay2 <= 14 & delay2 > 0 & check %in% c("TRUE",NA) ~ reported_date2,
    .default = reported_date
  )) |>
  filter(delay > 365-14 & delay2 <= 14 & delay2 > 0 & check %in% c("TRUE",NA)) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2)


df_cases |>
  mutate(delay = reported_date - admission_date,
         reported_date2 = update(reported_date, year = year(admission_date)),
         delay2 = reported_date2 - admission_date,
         check = year(admission_date) == year(onset_date)) |>
  # filter(delay < 365 & delay > 360)|>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2,check) |>
  filter(delay2 < 0 & delay2 > -14 & check == TRUE)
  # pull(reported_date) |>
  # unique()


df_cases |>
  mutate(delay_adm = admission_date - onset_date) |>
  filter(onset_date > reported_date & day(onset_date) == month(reported_date)) |>
  mutate(onset_date2 = ydm(onset_date),
         delay_adm2 = admission_date - onset_date2) |>
  select(reported_date,admission_date,onset_date,delay_adm,onset_date2,delay_adm2) |>
  as.data.frame()

df_cases |>
  mutate(check = case_when(
    onset_date > reported_date & day(onset_date) == month(reported_date) ~ TRUE,
    .default = FALSE
  )) |>
  mutate(
    onset_date = ifelse(check == TRUE,ydm(onset_date),ymd(onset_date)),
    onset_date = as.Date(onset_date)
  ) |>
  filter(check == TRUE) |>
  mutate(delay = reported_date - onset_date) |>
  select(reported_date,admission_date,onset_date,check,delay) |>
  as.data.frame()

## year wrong: onset
## year right: adm,report

df_cases |>
  mutate(check = year(reported_date) == year(admission_date),
         check2 = year(admission_date) != year(onset_date)) |>
  filter(check == TRUE & check2 == TRUE) |>
  mutate(delay = reported_date - onset_date) |>
  select(reported_date,admission_date,onset_date,check,check2,delay) |>
  filter(delay > 365)


df_cases |>
  mutate(check = year(reported_date) == year(admission_date),
         check2 = year(admission_date) != year(onset_date),
         delay = reported_date - onset_date,
         onset_date2 = update(onset_date, year = year(admission_date)),
         delay2 = reported_date - onset_date2) |>
  mutate(
    onset_date = case_when(
      check == TRUE & check2 == TRUE & delay > 365 & delay2 > 0 & delay2 <= 14 & delay2 > 14 ~ onset_date2,
      .default = onset_date),
  ) |>
  filter(check == TRUE & check2 == TRUE & delay > 365) |>
  select(reported_date,admission_date,onset_date,check,check2,delay,delay2) |>
  as.data.frame()

df_cases



## year wrong: adm
## year right: onset,report
df_cases |>
  mutate(check = year(reported_date) == year(onset_date),
         check2 = year(admission_date) != year(onset_date)) |>
  filter(check == TRUE & check2 == TRUE)

## year wrong: report
## year right: onset,adm  (1)



## date and year swaped

### onset swapped
### adm vs reported: right

df_cases |>
  mutate(delay_adm = admission_date - onset_date,
         onset_date2 = ydm(onset_date),
         delay_adm2 = admission_date - onset_date2,
         check = onset_date > reported_date & day(onset_date) == month(reported_date)) |>
  mutate(
    onset_date = case_when(
      check = TRUE ~ onset_date2,
      .default = onset_date),
  ) |>
  filter(check == TRUE) |>
  select(reported_date,admission_date,onset_date,delay_adm,onset_date2,delay_adm2) |>
  as.data.frame()

adm_date = ifelse(
  adm_date > reported_date & day(adm_date) == month(reported_date),
  ydm(adm_date),
  ymd(adm_date)
)

df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(admission_date > reported_date & day(admission_date) == month(reported_date)) |>
  mutate(admission_date2 = ydm(admission_date),
         delay2 = reported_date - admission_date2) |>
  select(reported_date,admission_date,onset_date,delay,admission_date2,delay2) |>
  as.data.frame()

### adm swapped
### onset vs reported: right

df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(admission_date > reported_date & day(admission_date) == month(reported_date)) |>
  mutate(admission_date2 = ydm(admission_date),
         delay2 = reported_date - admission_date2) |>
  select(reported_date,admission_date,onset_date,delay,admission_date2,delay2) |>
  as.data.frame()

df_cases <- paste0(path2data, cases_file) |>
  read_excel()

### reported swapped
### onset vs adm: right

df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(month(admission_date) == day(reported_date) & delay > 30) |>
  mutate(reported_date2 = ydm(reported_date),
         delay2 = reported_date2 - admission_date) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2) |>
  filter(delay2 < 30)



df_cases |>
  mutate(delay = reported_date - admission_date) |>
  filter(month(admission_date) == day(reported_date) & delay > 30) |>
  mutate(reported_date2 = ydm(reported_date),
         delay2 = reported_date2 - admission_date) |>
  select(reported_date,admission_date,onset_date,delay,reported_date2,delay2) |>
  filter(delay2 < 14) |>
  as.data.frame()

df_cases |>
  pull(district) |>
  unique()

library(stringi)

df_cases |>
  pull(inout) |>
  unique()
