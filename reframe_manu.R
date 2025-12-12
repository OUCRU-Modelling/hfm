df23 <- df_cases %>% filter(year(adm_date) == 2023)
df23 <- df23[!duplicated(df23), ]

df23 <- df23 %>%
  mutate(
    wave = case_when(
      adm_date <= as.Date("2023-09-03") ~ "1st wave",
      adm_date > as.Date("2023-09-03") ~ "2nd wave"
      ),
    severity2 = case_when(
      severity %in% c("1","1a") ~ "1",
      severity %in% c("3","4") ~ "3 or 4",
      severity %in% c("2") ~ "2a",
      is.na(severity) ~ "Undefined",
      !severity %in% c("1","1a","3","4",NA) ~ severity
    ) %>% as.factor(),
    inout2 = case_when(
      inout == "Điều trị nội trú" ~ "Inpatients",
      inout == "Điều trị ngoại trú" ~ "Outpatients",
      !inout %in% c("Điều trị nội trú","Điều trị ngoại trú") ~ "Others"
    ) %>% factor(levels = c("Inpatients","Outpatients","Others")),
    gender2 = case_when(
      gender %in% c("Nam","nam","NAM") ~ "Male",
      gender %in% c("Nữ","nữ","NỮ") ~ "Female"
    ) %>% as.factor()
  )

df23$inout |> unique()

library(gtsummary)

# -----------------------------
# 1. Age table
# -----------------------------
tab_age <-
  df23 %>%
  tbl_summary(by = wave,
              label = list(age1 ~ "Age (Years)",
                           gender ~ "Gender"),
              digits = list(all_continuous() ~ c(2,2),
                            all_categorical() ~ c(0,2)),
              include = c(age1,gender)) %>%
  remove_row_type(type = "missing") %>%
  add_p()

# -----------------------------
# 2. Severity table
# -----------------------------
calculate_prop_test <- function(data, variable, by, ...) {
  # subset to non-missing
  data <- tidyr::drop_na(data, dplyr::all_of(c(variable, by)))

  prop.trend.test(
    x = table(data[[variable]], data[[by]])[2, ], # get the second row (the positive row)
    n = table(data[[by]])
  ) |>
    broom::tidy()
}

tab_sev <-
  df23 |>
  filter(severity2 != "Undefined") |>
  select(wave, severity2)  |>
  mutate(rn = row_number()) |>
  tidyr::spread(severity2, severity2) |>
  mutate(across(-c(wave, rn), ~ ifelse(is.na(.), 0, 1))) %>%
  select(-rn) %>%
  tbl_summary(by = wave,
              statistic = list(
                all_categorical() ~ "{n} / {N} ({p}%)"
              ),
              digits = all_categorical() ~ c(0,0,2)) %>%
  add_p() %>%
  modify_table_body(
    ~ .x %>%
      mutate(row_type = "level",
             variable = "wave") %>%
      {bind_rows(
        tibble(variable = "wave", row_type = "label", label = "Severity"),
        .
      )}
  )

# -----------------------------
# 3. In/out table
# -----------------------------
tab_inout <-
  df23 %>%
  filter(inout %in% c("Inpatient","Outpatient")) |>
  select(wave, inout) %>%
  mutate(rn = row_number()) %>%
  tidyr::spread(inout, inout) %>%
  mutate(across(-c(wave, rn), ~ ifelse(is.na(.), 0, 1))) %>%
  select(-rn) %>%
  tbl_summary(by = wave,
              statistic = list(
                all_categorical() ~ "{n} / {N} ({p}%)"
              ),
              digits = all_categorical() ~ c(0,0,2)) %>%
  add_p() %>%
  modify_table_body(
    ~ .x %>%
      mutate(row_type = "level",
             variable = "wave") %>%
      {bind_rows(
        tibble(variable = "wave", row_type = "label", label = "Treatment"),
        .
      )}
  )

# -----------------------------
# 4. Combine all three tables
# -----------------------------
final_table <-
  tbl_stack(list(tab_age, tab_sev, tab_inout))

final_table

###

df23 %>%
  group_by(adm_week, district) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n)) +
  geom_col() +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b",
    name = "Admission week",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31"))
  ) +
  theme_bw() +
  labs(y = "Number of admission") +
  facet_wrap(~district, ncol = 4) +
  theme(
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  )


ggsave("./fig_manuscript/sup1_new.svg",
       width = 12,height = 12,dpi = 500)

## age distribution of severity

df23 %>% filter(!is.na(severity2)) %>%
ggplot(aes(x=age1, group=severity2, fill=severity2)) +
  geom_density(alpha = 0.6) +
  paletteer::scale_fill_paletteer_d("beyonce::X78")+
  scale_x_continuous(limit = c(0,6),
                  breaks = seq(0,6,by=1),
                  minor_breaks = NULL)+
  scale_y_continuous(minor_breaks = NULL)+
  labs(x = "Age (years)",y = "Density",fill = "Severity")+
  facet_wrap(~wave)+
  # coord_flip()+
  theme_bw()


## attack rate of sero

### attack rate matrix

df_sero <- df_sero |>
  as_tibble() |>
  mutate(collection = id |>
           str_remove(".*-") |>
           as.numeric() |>
           divide_by(1e4) |>
           round(),
         col_date2 = as.numeric(col_date),
         across(pos, ~ .x > 0))

df_cases_ch1_23 <- df23 |>
  filter(medi_cen %in% c("Bệnh viện Nhi đồng 1",
                         "Bênh viện Nhi Đồng 1",
                         "Bệnh viện Nhi Đồng 1"))

ts_ch1 <- df_cases_ch1_23 %>%
  group_by(adm_week,wave) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n,fill = wave))+
  geom_col(alpha = 0.6,color="black")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y",
               name = "Collection date",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  scale_y_continuous(limits = c(0,900),breaks = seq(0,900,le = 4))+
  scale_fill_manual(values = c("#582C83FF","#FFC72CFF"))+
  theme_minimal()+
  annotate("rect", fill = "grey",
           xmin = as.Date(c("2023-01-01","2023-04-05","2023-08-02","2023-12-06")),
           xmax = as.Date(c("2023-01-15","2023-04-26","2023-08-30","2023-12-27")),
           ymin = 0, ymax = Inf, alpha = .5)+
  labs(y = "Cases")+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20),
         color = "none")

hm_ch1 <- df_cases_ch1_23 %>%
  ggplot(aes(x = adm_week, y = age1)) +
  stat_density_2d(
    aes(fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    interpolate = TRUE
  ) +
  geom_line(
    data = cohort_lines,
    aes(x = date, y = age, group = cohort),
    color = "white",
    linewidth = 0.25,
    alpha = 0.3
  ) +
  scale_fill_viridis_c(option = "inferno",n.breaks = 10)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31"))) +
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Number of\nadmissions",tag = "B")+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

outttt <- age_profile_constrained_cohort2(df_sero)

atk_sero <- map2(head(outttt, -15),
     tail(outttt,-15),
     ~ left_join(na.exclude(.x), na.exclude(.y), "cohort")|>
       mutate(attack = (fit.y - fit.x) / (1 - fit.x),
              date = as.Date(collection_time.y))) |>
  bind_rows() |>
  ggplot() +
  geom_raster(aes(x=date, y=cohort,fill = attack),interpolate = TRUE)+
  scale_fill_viridis_c(option = "inferno",name = "Attack rate")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,14)),breaks = seq(0,14,by=2))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               name = "Collection date in 2023",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.ticks.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

cohort_lines <- function(cohort_ages = seq(0, 5, by = 1),viro = FALSE){
  start_date <- as.Date("2023-01-01")
  end_date   <- as.Date("2023-12-31")

  ## generate the cohort line
  tseq <- seq(start_date, end_date, by = "day")
  cohort_lines <- lapply(cohort_ages, function(a0){
    data.frame(
      date = tseq,
      age  = a0 + as.numeric(tseq - start_date) / 365,
      cohort = as.factor(a0)
    )
  }) |> dplyr::bind_rows()

  color_cohort <- ifelse(viro == TRUE,"black","#80FFFFFF")

  ## create mid-opacity white of cohort lines before June
  cohort_lines <- cohort_lines |>
    mutate(trend = case_when(
      date < as.Date("2023-06-01") ~ color_cohort,
      date >= as.Date("2023-06-01") ~ "#FFFFFFFF",
    ))
  cohort_lines$group <- consecutive_id(cohort_lines$trend)
  cohort_lines <- head(do.call(rbind, by(cohort_lines, cohort_lines$group, rbind, NA)), -1)
  cohort_lines[, c("trend", "group")] <- lapply(cohort_lines[, c("trend", "group")], na.locf)
  cohort_lines[] <- lapply(cohort_lines, na.locf, fromLast = T)

  cohort_lines$trend <- factor(cohort_lines$trend,
                               levels = c(color_cohort,"#FFFFFFFF"))
  cohort_lines
}



atk_sero+
geom_line(
  data = cohort_lines(seq(0,15,by=1)),
  aes(x = date, y = age, group = cohort),
  color = "white",
  linewidth = 0.25,
  alpha = 0.3
)

age_profile_sp_cm <- outttt |>
  bind_rows() |>
  mutate(col_date = as.Date(collection_time)) |>
  filter(col_date %in% as.Date(c("2022-12-29","2023-04-11","2023-08-13","2023-12-31"))) |>
  mutate(col_lab = format(col_date, format = "%b %Y"),
         col_lab = factor(col_lab,levels = c("Dec 2022","Apr 2023","Aug 2023","Dec 2023"))) |>
  ggplot(aes(x = cohort,y = fit))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "blue", alpha = .3)+
  facet_wrap(~ col_lab,nrow = 1)+
  labs(y = "Seroprevalence (%)",x = "Age (years)",tag="B")+
  scale_y_continuous(labels = scales::label_percent(scale = 100),limits = c(0,1))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))



## expected incidence calculation

ch1_outpatient <- paste0(path2data, "CH1_outpatient_HCM_cleaned.rds") |>
  readRDS()

census2019 <- paste0(path2data, "census2019.rds") |>
  readRDS()

N_ad <- census2019 |>
  filter(province == "Thành phố Hồ Chí Minh") |>
  mutate(district = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  group_by(district,age) |>
  summarise(n = sum(n),.groups = "drop") |>
  mutate(across(age, ~ stringr::str_remove(.x, " tuổi| \\+") |> as.integer())) |>
  arrange(age) |>
  filter(age < 17)

expected_catchment_CH1 <- left_join(ch1_outpatient,
                                    N_ad,
                                    by = join_by(district,age_gr == age)) %>%
  na.omit(n.y) |>
  mutate(p_hos = n.x/n.y) %>%
  arrange(age_gr) %>%
  group_by(age_gr) %>%
  mutate(weight = p_hos/sum(p_hos),
         expected_cm = n.x*weight) %>%
  ungroup()

df_cases_ch1_23 |>
  mutate(age3 = ceiling(age1)) |>
  group_by(age3,district2) |>
  count() |>
  left_join(ch1_outpatient,by = join_by(district2 == district,age3 == age_gr)) |>
  ungroup() |>
  set_colnames(c("age","district","hfmd_cases","outpatient")) |>
  ggplot(aes(x = age))+
  geom_line(aes(y = outpatient))+
  geom_point(aes(y = hfmd_cases*6))+
  scale_y_continuous(
    name = "Outpatients admissions",
    # Define the secondary axis
    sec.axis = sec_axis(
      trans = ~ . / 6,
      name = "HFMD cases")
  )+
  facet_wrap(~district)


ch1_outpatient |>
  ggplot(aes(x = age_gr, y = n))+
  geom_col()+
  facet_wrap(~district)

expected_age_cm <- expected_catchment_CH1  %>%
  group_by(age_gr) %>%
  summarise(e_admission = sum(expected_cm)) %>%
  gam(e_admission~s(age_gr),method = "REML",data = .) %>%
  predict(list(age_gr = seq(0,15,le = 512)),type="response",se.fit = TRUE) %>%
  as.tibble() %>%
  mutate(age = seq(0,15,le = 512),
         lwr = fit - qt(0.95,nrow(.))*se.fit,
         upr = fit + qt(0.95,nrow(.))*se.fit) %>%
  select(-se.fit)


expected_age_cm %>%
  ggplot(aes(x = age,y = fit))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr,ymax=upr),fill = "blue",alpha = .3)+
  geom_point(aes(x = age_gr, y = e_admission),
             data = expected_catchment_CH1  %>%
               group_by(age_gr) %>%
               summarise(e_admission = sum(expected_cm)))+
  theme_minimal()+
  labs(y = "Expected hospitalization",x = "Age (years)")

ouut <- list()
dat <- list()

for (i in 1:3){
  dat[[i]] <- df_cases_ch1_23 |>
    filter(adm_date >= as.Date(mean_collection_times[i]) &
             adm_date <= as.Date(mean_collection_times[i+1]))
}

exp_in_ch1 <- map2(head(outttt, -15),
                   tail(outttt,-15),
                   ~ left_join(na.exclude(.x), na.exclude(.y), "cohort")|>
                     mutate(attack = (fit.y - fit.x) / (1 - fit.x),
                            date = as.Date(collection_time.y))) |>
  bind_rows() |>
  filter(date %in% as.Date(c("2023-04-11","2023-08-13","2023-12-31"))) |>
  group_by(date) |>
  group_split() |>
  map(~ left_join(.x,expected_age_cm, by = join_by(cohort == age))) %>%
  bind_rows(.id = "id") |>
  mutate(sp_gap = fit.y-fit.x,
         exp_inc = sp_gap*fit) |>
  ggplot()+
  geom_line(aes(x = cohort,y = exp_inc))+
  geom_histogram(data = dat %>% bind_rows(.id = "id") %>% filter(age1 >0),
                 aes(age1),binwidth = 0.5,
                 color = "white",fill = "black",alpha = 0.2)+
  facet_wrap(~factor(id,labels = c("12/2022 - 4/2023",
                                   "4/2023 - 8/2023",
                                   "8/2023 - 12/2023")))+
  labs(y = "Expected incidence",x = "Age (years)",tag = "A")+
  coord_cartesian(ylim = c(0,1200))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())

exp_in_ch1/
  age_profile_sp_cm/
  (ts_ch1+labs(tag="C"))/
  (hm_ch1+labs(tag="D"))/
  (atk_sero+labs(tag="E"))

ggsave("./fig_manuscript/fig2_new.svg",
       width = 13,height = 20,dpi = 500)

### virological analysis

time_f <- df_cases_ch1_23 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup()

df_viro <- df_viro %>%
  mutate(pos = case_when(
    sero_gr == "EV-A71" ~ TRUE,
    sero_gr != "EV-A71" ~ FALSE),
    pos_cv = case_when(
      sero_gr == "CV-A6" ~ TRUE,
      sero_gr != "CV-A6" ~ FALSE),
    peak = case_when(
      month(admission_date) >= 6 & month(admission_date) <= 8 ~ "6/2023 - 8/2023",
      month(admission_date) >= 9 ~ "9/2023 - 12/2023"
    ),
    adm_week = as.Date(floor_date(admission_date, "week")),
    adm_month = as.Date(floor_date(admission_date, "month")),
    time = as.numeric(admission_date)
  )

dt_vr <- df_viro %>%
  group_by(adm_month) %>%
  count(pos) %>%
  ungroup() %>%
  pivot_wider(names_from = pos,values_from = n,names_prefix = "pos") %>%
  replace(is.na(.),0) %>%
  mutate(total = posTRUE+posFALSE,
         per = posTRUE/total,
         time = adm_month %>% as.numeric()
         )%>%
  rowwise() %>%                          # needed for prop.test()
  mutate(
    lwr = prop.test(posTRUE, total)$conf.int[1],   # 95% CI lower bound
    upr = prop.test(posTRUE, total)$conf.int[2]    # optional upper bound
  )


modelll <- gam(pos~s(time,bs = "bs"),family = binomial,method = "REML",data = df_viro)

link_inv <- modelll$family$linkinv

recon_epicurve_dt <- modelll %>%
  predict(list(time = time_f %>%
                 filter(adm_week >= min(df_viro$admission_date)) %>%
                 pull(adm_week) %>%
                 as.numeric()
  ),se.fit = TRUE) %>%
  as_tibble() %>%
  mutate(time = time_f %>%
           filter(adm_week >= min(df_viro$admission_date)) %>%
           pull(adm_week),
         lwr = link_inv(fit - 1.96*se.fit),
         upr = link_inv(fit + 1.96*se.fit),
         fit = link_inv(fit)
         ) %>%
  select(-se.fit) %>%
  left_join(time_f,., by = join_by(adm_week == time)) %>%
  mutate(e_ev71 = fit*n,
         e_ev71_lwr = lwr*n,
         e_ev71_upr = upr*n) %>%
  replace(is.na(.),-1)

recon_epicurve <- recon_epicurve_dt %>%
  ggplot(aes(x = adm_week))+
  geom_line(aes(y = fit*1000),linetype = "dashed")+
  geom_ribbon(aes(y = fit*1000,ymin = lwr*1000,ymax = upr*1000),fill = "blue",alpha = .3)+
  geom_line(aes(y = e_ev71))+
  geom_ribbon(aes(y = e_ev71_upr,ymin = e_ev71_lwr,ymax = e_ev71_upr),fill = "red",alpha = .5)+
  scale_y_continuous(
    name = "CH1 admission",
    sec.axis = sec_axis(~ . /1000, name = "EV-A71 percentage"),
    limits = c(0,1000)
  ) +
  geom_col(aes(y=n),alpha = 0.3)+
  geom_point(data = df_viro, aes(x = admission_date, y = pos*1000),shape = "|")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

per_serotype <- df_viro %>%
  group_by(adm_month) %>%
  count(sero_gr) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  ggplot(aes(x = adm_month,y=per,fill = sero_gr))+
  geom_bar(position="fill", stat="identity")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  scale_y_continuous(labels = scales::label_percent())+
  labs(fill = "Serotype group",y = "Percentage (%)",tag = "B")+
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))


## model virology with age and time


model_viro_age_time <- gam(pos ~ s(age_adm) + s(time) + ti(age_adm,time),
                           family = binomial,method = "REML",data = df_viro)

collection_date_val <- seq(min(df_viro$time),
                           max(df_viro$time), le = 365)

new_data2 <- expand.grid(age_adm = age_val,
                         time = as.numeric(collection_date_val))

new_data2$pred <- predict(model_viro_age_time, newdata = new_data2, type = "response")


ev_a71_age_time_p <- ggplot() +
  geom_raster(data = new_data2,
              aes(x = as.Date(time), y = age_adm, fill = pred), interpolate = TRUE) +
  scale_fill_viridis_c(option = "inferno",n.breaks = 10) +
  geom_line(
    data = cohort_lines(viro = TRUE),
    aes(x = date, y = age, group = cohort,color = trend),
    linewidth = 0.25,
    alpha = 0.3
  ) +
  labs(x = "Week", y = "Age", fill = "Smoothed EV-A71 percentage")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme_void()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(-0.5,6.5)),breaks = seq(-1,7))+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=25),
         color = "none")

(recon_epicurve+labs(tag="A"))/
  (per_serotype+labs(tag="B"))/
  (ev_a71_age_time_p+labs(tag="C"))

ggsave("./fig_manuscript/fig3_new2.svg",
       width = 13,height = 12,dpi = 500)

## correct by combinning the cases model and virological model

df_cases_count_ch1_23 <- df_cases_ch1_23 |>
  group_by(age,time) |>
  count() |>
  ungroup()

fit_gau_bs_ch1 <- gam(n ~ s(age,bs = "bs") +
                    s(time,bs = "bs") +
                    ti(age, time, bs = c("bs", "bs")) , data = df_cases_count_ch1_23,
                  family = gaussian(link = "log"),method = "REML")


cases_fit <- fit_gau_bs_ch1 |>
    predict(newdata = df_cases_count_ch1_23, se.fit = TRUE) |>
    as.tibble() |>
    cbind(df_cases_count_ch1_23) |>
    mutate(
      # c_lwr = fit_gau_bs_ch1$family$linkinv(fit - 1.96 * se.fit),
      # c_upr = fit_gau_bs_ch1$family$linkinv(fit + 1.96 * se.fit),
      c_fit = fit_gau_bs_ch1$family$linkinv(fit)
           ) |>
    select(-c(se.fit,fit))

recontructed_df <- model_viro_age_time |>
  predict(newdata = df_cases_count_ch1_23 |> rename(age_adm = age), se.fit = TRUE) |>
  as.tibble() |>
  cbind(df_cases_count_ch1_23) |>
  mutate(v_lwr = model_viro_age_time$family$linkinv(fit - 1.96 * se.fit),
         v_upr = model_viro_age_time$family$linkinv(fit + 1.96 * se.fit),
         v_fit = model_viro_age_time$family$linkinv(fit)) |>
  select(-c(se.fit,fit)) |>
  full_join(cases_fit, by = join_by(age, time,n)) |>
  mutate(ev71_c = c_fit*v_fit,
         ev71_u = c_fit*v_upr,
         ev71_l = c_fit*v_lwr)

recontructed_cases_count_df <- recontructed_df |>
  mutate(day = as.Date(time),
         week = as.Date(floor_date(day, "week"))) |>
  group_by(week) |>
  summarise(cases = sum(c_fit),
            # cases_u = sum(c_upr),
            # cases_l = sum(c_lwr),
            ev71_cases = sum(ev71_c),
            ev71_cases_u = sum(ev71_u),
            ev71_cases_l = sum(ev71_l),
            raw_n = sum(n))

recontructed_cases_count_df |>
  ggplot(aes(x = week))+
  geom_line(aes(y = cases),linetype = "dashed")+
  # geom_ribbon(aes(ymin = cases_u,ymax = cases_l),fill = "blue",alpha = .3)+
  geom_line(aes(y = ev71_cases))+
  geom_ribbon(aes(ymin = ev71_cases_u,ymax = ev71_cases_l),fill = "red",alpha = .3)+
  geom_col(aes(y = raw_n),alpha = .3) +
  geom_point(data = df_viro, aes(x = admission_date, y = pos*1000),shape = "|",size = 2)+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               name = "Collection date",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31"))) +
  labs(x = "Admission day (2023)",y = "Total number of cases") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20),
         color = "none")


recontructed_df |>
  mutate(day = as.Date(time),
         week = as.Date(floor_date(day, "week")),
         period = case_when(
           week <= as.Date("2023-04-30") ~ "12/2022 - 4/2023",
           week > as.Date("2023-04-30") & week <= as.Date("2023-08-31") ~ "4/2023 - 8/2023",
           week > as.Date("2023-08-31") ~ "8/2023 - 12/2023"
         )) |>
  group_by(period,age) |>
  summarise(ev_a71_cases = sum(ev71_c),
            raw_n = sum(n),
            .groups = "drop") |>
  ggplot(aes(x = age, y = ev_a71_cases))+
  geom_col(alpha = 0.3)+
  geom_line(data = expected_incidence|>
              mutate(period = case_when(
                id == 1 ~ "12/2022 - 4/2023",
                id == 2 ~ "4/2023 - 8/2023",
                id == 3 ~ "8/2023 - 12/2023"
              )),aes(x = cohort,y=exp_inc))+
  scale_x_continuous(limits = c(0,15))+
  facet_wrap(~period)+
  labs(y = "Expected incidence",x = "Age (years)",tag = "A")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())

recontructed_df <- df_cases_count_ch1_23 |>
  select(age,time,n) |>
  mutate(cases_fit = predict(fit_gau_bs_ch1,df_cases_count_ch1_23,type = "response"),
         ev_per = predict(model_viro_age_time,
                          newdata = df_cases_count_ch1_23 |> rename(age_adm = age),
                          type = "response"),
         ev71_cases = cases_fit*ev_per)

recontructed_cases_count_df <- recontructed_df |>
  group_by(time) |>
  summarise(cases = sum(cases_fit),
            ev71_cases = sum(ev71_cases))

recontructed_cases_count_df |>
  mutate(day = as.Date(time),
         week = as.Date(floor_date(day, "week"))) |>
  group_by(week) |>
  summarise(cases = sum(cases),
            ev71 = sum(ev71_cases)) |>
  ggplot(aes(x = week))+
  geom_line(aes(y = cases),linetype = "dashed")+
  geom_line(aes(y = ev71))+
  geom_col(data = df_cases_count_ch1_23 |>
             mutate(day = as.Date(time),
                    week = as.Date(floor_date(day, "week")))|>
             group_by(week) |>
             summarise(cases = sum(n)),
           aes(x = week, y  = cases),alpha = .3)+
  scale_x_date(date_breaks = "1 month",
             date_labels = "%b",
             name = "Collection date",
             limits = c(as.Date("2023-01-01"),as.Date("2023-12-31"))) +
  labs(x = "Admission day (2023)",y = "Total number of cases") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20),
         color = "none")

recontructed_df |>
  mutate(day = as.Date(time),
         week = as.Date(floor_date(day, "week"))) |>
  # group_by(week) |>
  # summarise(cases = sum(cases),
  #           ev71 = sum(ev71_cases)) |>
  mutate(period = case_when(
    week <= as.Date("2023-04-30") ~ "12/2022 - 4/2023",
    week > as.Date("2023-04-30") & week <= as.Date("2023-08-31") ~ "4/2023 - 8/2023",
    week > as.Date("2023-08-31") ~ "8/2023 - 12/2023"
  )) |> group_by(period,age) |>
  summarise(ev_a71_cases = sum(ev71_cases),.groups = "drop") |>
  ggplot(aes(x = age, y = ev_a71_cases))+
  geom_col(alpha = 0.3)+
  geom_line(data = expected_incidence|>
              mutate(period = case_when(
                id == 1 ~ "12/2022 - 4/2023",
                id == 2 ~ "4/2023 - 8/2023",
                id == 3 ~ "8/2023 - 12/2023"
              )),aes(x = cohort,y=exp_inc))+
  scale_x_continuous(limits = c(0,15))+
  facet_wrap(~period)+
  labs(y = "Expected incidence",x = "Age (years)",tag = "A")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())

recontructed_df |>
  filter(age <=6) |>
  ggplot(aes(x = as.Date(time)))+
  geom_line(aes(y = cases_fit))+
  geom_point(aes(y = n),alpha = .1)+
  facet_wrap(~age)





model_viro_age_time <- gam(pos ~ s(age_adm) + s(time) + ti(age_adm,time),
                           family = binomial,method = "REML",data = df_viro)

reconstructed_cases_df <- new_data |>
  mutate(ev_a71_per = predict(model_viro_age_time,
                              newdata = new_data |> rename(age_adm = age),
                              type = "response"),
         reconstruct_cases = pred_gau_bs*ev_a71_per)

new_data |>
  group_by(time) |>
  summarise(cases = sum(pred_gau_bs),.groups = "drop") |>
  ggplot(aes(x = as.Date(time), y = cases)) +
  geom_line()+
  geom_col(data = df23 |>
             group_by(adm_week) |>
             count() |>
             ungroup(),
           aes(x = adm_week, y = n),alpha = .3)

reconstructed_cases_df |>
  group_by(time) |>
  summarise(ev_a71_cases = sum(reconstruct_cases),
            reported = sum(pred_gau_bs)) |>
  ggplot(aes(x = as.Date(time)))+
  geom_line(aes(y = ev_a71_cases))+
  geom_point(aes(y = reported))+
  geom_col(data = df23 |>
             group_by(adm_week) |>
             count() |>
             ungroup(),
           aes(x = adm_week, y = n),alpha = .3)


