df23 <- df1 %>% filter(year(adm_date) == 2023)
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

library(gtsummary)

# -----------------------------
# 1. Age table
# -----------------------------
tab_age <-
  df23 %>%
  tbl_summary(by = wave,
              label = list(age ~ "Age (Years)",
                           gender2 ~ "Gender"),
              include = c(age,gender2)) %>%
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
  df23 %>%
  select(wave, severity2) %>%
  mutate(rn = row_number()) %>%
  tidyr::spread(severity2, severity2) %>%
  # select(-`<NA>`) %>%
  mutate(across(-c(wave, rn), ~ ifelse(is.na(.), 0, 1))) %>%
  select(-rn) %>%
  tbl_summary(by = wave) %>%
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
  select(wave, inout2) %>%
  mutate(rn = row_number()) %>%
  tidyr::spread(inout2, inout2) %>%
  mutate(across(-c(wave, rn), ~ ifelse(is.na(.), 0, 1))) %>%
  select(-rn) %>%
  tbl_summary(by = wave) %>%
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


ts <- data.frame(wwww$wdat$date,wwww$wdat$age) %>%
  filter(!is.na(wwww$wdat$date) & !is.na(wwww$wdat$age)) %>%
  count(wwww$wdat$date) %>%
  set_colnames(c("time","n")) %>%
  mutate(peak = c(rep("1st",36),rep("2nd",nrow(.)-36))) %>%
  ggplot(aes(x = time, y = n,fill = peak)) +
  geom_col(alpha = 0.6,color="black") +
  geom_vline(xintercept = 36.5) +
  theme_minimal()+
  scale_y_continuous(name = "Cases",
                     breaks = seq(0,3000,by = 1000),
                     limit =c(0,3000),
                     minor_breaks = NULL)+
  scale_fill_manual(values = c("#582C83FF","#FFC72CFF"))+
  labs(tag = "A")+
  # annotate("rect", fill = "grey",
  #          xmin = c("2023-01-04","2023-04-05","2023-08-02","2023-12-06"),
  #          xmax = c("2023-01-11","2023-04-26","2023-08-30","2023-12-27"),
  #          ymin = 0, ymax = Inf, alpha = .5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18))


hm2 <- ggplot(data=wwww$wdat, aes(x=date, y=age)) +
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
  scale_x_discrete(name = "Admission week",labels = leb_month)+
  labs(fill = "Number of admissions",tag = "B")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 8))+
  geom_line(data = ch,aes(x = date,y = c0,col = trend),
            group = 1,lwd = 0.25)+
  geom_line(data = ch,aes(x = date,y = c1,col = trend),
            group = 1,lwd = 0.25)+
  geom_line(data = ch,aes(x = date,y = c2,col = trend),
            group = 1,lwd = 0.25)+
  geom_line(data = ch,aes(x = date,y = c3,col = trend),
            group = 1,lwd = 0.25)+
  geom_line(data = ch,aes(x = date,y = c4,col = trend),
            group = 1,lwd = 0.25)+
  geom_line(data = ch,aes(x = date,y = c5,col = trend),
            group = 1,lwd = 0.25)+
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


ad <- ggplot(data=data, aes(x=age, group=peak, fill=peak)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#582C83FF","#FFC72CFF")) +
  scale_x_reverse(limit = c(6,0),
                  breaks = seq(0,6,by=1),
                  minor_breaks = NULL)+
  scale_y_continuous(minor_breaks = NULL)+
  coord_flip()+
  theme_minimal()+
  labs(x = "Age", y ="Density",fill = "Wave",tag = "C")+
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        # legend.position = "top",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))


((ts/hm2)|(plot_spacer()/ad))+
  plot_layout(widths = c(2.5,1))

ggsave("./fig_manuscript/fig1_new.svg",
       width = 10,height = 6,dpi = 500)

## attack rate of sero

### attack rate matrix



age_profile_constrained_cohort2 <- function(data, age_values = seq(0, 15, le = 512),
                                            ci = .95, n = 100) {
  dpy <- 365 # number of days per year

  mean_collection_times <- data |>
    group_by(collection) |>
    summarise(mean_col_date = mean(col_date2)) |>
    with(setNames(mean_col_date, collection))

  cohorts <- cumsum(c(0, diff(mean_collection_times))) |>
    divide_by(dpy * mean(diff(age_values))) |>
    round() |>
    map(shift_right, age_values)

  age_time <- map2(mean_collection_times, cohorts,
                   ~ tibble(collection_time = .x, cohort = .y))

  age_time_inv <- age_time |>
    map(~ cbind(.x, age = age_values)) |>
    bind_rows() |>
    na.exclude()

  data |>
    # Step 1:
    group_by(collection) |>
    group_modify(~ .x |>
                   age_profile(age_values, ci) |>
                   mutate(across(c(fit, lwr, upr), ~ map(.x, ~ rbinom(n, 1, .x))))) |>
    group_split() |>
    map2(age_time, bind_cols) |>
    bind_rows() |>
    unnest(c(fit, lwr, upr)) |>
    pivot_longer(c(fit, lwr, upr), names_to = "line", values_to = "seropositvty") |>
    # Step 2a:
    filter(cohort < max(age) - diff(range(mean_collection_times)) / dpy) |>
    group_by(cohort, line) |>
    group_modify(~ .x %>%
                   scam(seropositvty ~ s(collection_time, bs = "mpi"), binomial, .) |>
                   predict2(list(collection_time = seq(19358,19722,le=52))) %>%
                   tibble(collection_time = seq(19358,19722,le=52),
                          seroprevalence  = .)) |>
    ungroup() |>
    # Step 2b:
    # left_join(age_time_inv, c("cohort", "collection_time")) |>
    # pivot_wider(names_from = line,values_from = seroprevalence) %>%
    # ggplot(aes(x = cohort,y = fit))+
    # geom_line()+
    # geom_ribbon(aes(x = cohort,y = fit,ymin = lwr,ymax = upr),fill = "blue",alpha = .5)+
    # facet_wrap(~collection_time)
    group_by(collection_time, line) |>
    group_modify(~ .x |>
                   mutate(across(seroprevalence, ~ gam(.x ~ s(cohort), betar) |>
                                   predict2()))) |>  ### modified
    ungroup() |>
    pivot_wider(names_from = line, values_from = seroprevalence) |>
    group_by(collection_time) |>
    group_split()
}
# as.Date("2023-01-01") %>% as.numeric()
outttt <- age_profile_constrained_cohort2(hfmd)

# outttt %>%
#   bind_rows() %>%
#   ggplot(aes(x = cohort,y = fit))+
#   geom_line()+
#   geom_ribbon(aes(x = cohort,y = fit,ymin = lwr,ymax = upr),fill = "blue",alpha = .5)+
#   facet_wrap(~collection_time)

library(paletteer)

atk_sero <- map2(head(outttt, -1),
     outttt[-1],
     ~ left_join(na.exclude(.x), na.exclude(.y), "cohort")|>
       mutate(attack = (fit.y - fit.x) / (1 - fit.x),
              date = as.Date(collection_time.y))) %>%
  bind_rows() %>%
  ggplot() +
  geom_raster(aes(x=date, y=cohort,fill = attack),interpolate = TRUE)+
  scale_fill_paletteer_c("grDevices::Inferno",name = "Attack rate")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,14)),breaks = seq(0,14,by=2))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               name = "Collection date in 2023",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20),
         color = "none")

ts_ch1 <- incidence1 %>%
  group_by(adm_week) %>%
  count() %>%
  mutate(peak = case_when(
    adm_week <= as.Date("2023-09-03") ~ "1st",
    adm_week >  as.Date("2023-09-03") ~ "2nd",
  )) %>%
  ggplot(aes(x = adm_week, y = n,fill = peak))+
  geom_col(alpha = 0.6,color="black")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y",
               name = "Collection date",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  geom_vline(xintercept = as.Date("2023-09-06"))+
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

hm_ch1 <- incidence1 %>%
  ggplot(aes(x = adm_week, y = cohort)) +
  stat_density_2d(
    aes(fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    interpolate = TRUE
  ) +
  scale_fill_paletteer_c("grDevices::Inferno") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31"))) +
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Number of admissions",tag = "B")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 8))+
  # geom_line(data = ch,aes(x = date,y = c0,col = trend),
  #           group = 1,lwd = 0.25)+
  # geom_line(data = ch,aes(x = date,y = c1,col = trend),
  #           group = 1,lwd = 0.25)+
  # geom_line(data = ch,aes(x = date,y = c2,col = trend),
  #           group = 1,lwd = 0.25)+
  # geom_line(data = ch,aes(x = date,y = c3,col = trend),
  #           group = 1,lwd = 0.25)+
  # geom_line(data = ch,aes(x = date,y = c4,col = trend),
  #           group = 1,lwd = 0.25)+
  # geom_line(data = ch,aes(x = date,y = c5,col = trend),
  #           group = 1,lwd = 0.25)+
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

0.3171508

exp_in_ch1/
  age_profile_sp_cm/
  (ts_ch1+labs(tag="C"))/
  (hm_ch1+labs(tag="D"))/
  (atk_sero+labs(tag="E"))

ggsave("./fig_manuscript/fig2_new.svg",
       width = 13,height = 20,dpi = 500)

incidence1

viro2 %>%
  ggplot(aes(x = admission_date, y = age)) +
  stat_density_2d(
    aes(fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    interpolate = TRUE
  ) +
  scale_fill_paletteer_c("grDevices::Inferno") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Percentage of EV-A71 samples",tag = "B")+
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


ev_per <- viro2 %>%
  filter(pos) %>%
  ggplot(aes(x = admission_date, y = age)) +
  stat_density_2d(
    aes(fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    interpolate = TRUE
  ) +
  scale_fill_paletteer_c("grDevices::Inferno") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Percentage of EV-A71 samples",tag = "B")+
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


cv_per <- viro2 %>%
  mutate(pos_cv = case_when(
    sero_gr == "CV-A6" ~ TRUE,
    sero_gr != "CV-A6" ~ FALSE
  )) %>% filter(pos_cv) %>%
  ggplot(aes(x = admission_date, y = age)) +
  stat_density_2d(
    aes(fill = after_stat(count)),
    geom = "raster",
    contour = FALSE,
    interpolate = TRUE
  ) +
  scale_fill_paletteer_c("grDevices::Inferno") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")),
               name = "Collection date in 2023")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Percentage of CV-A6 samples")+
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


ts2_ch1 <- incidence1 %>%
  group_by(adm_week) %>%
  count() %>%
  mutate(peak = case_when(
    adm_week <= as.Date("2023-09-03") ~ "1st",
    adm_week >  as.Date("2023-09-03") ~ "2nd",
  )) %>%
  ggplot(aes(x = adm_week, y = n,fill = peak))+
  geom_col(alpha = 0.6,color="black")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y",
               name = "Collection date",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  geom_vline(xintercept = as.Date("2023-09-06"))+
  scale_fill_manual(values = c("#582C83FF","#FFC72CFF"))+
  theme_minimal()+
  # annotate("rect", fill = "grey",
  #          xmin = as.Date(c("2023-01-01","2023-04-05","2023-08-02","2023-12-06")),
  #          xmax = as.Date(c("2023-01-15","2023-04-26","2023-08-30","2023-12-27")),
  #          ymin = 0, ymax = Inf, alpha = .5)+
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

ts3 <- time_f %>%
  ggplot(aes(x = adm_week))+
  geom_col(aes(y=n))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  geom_vline(xintercept = as.Date("2023-09-06"))+
  theme_minimal()+
  labs(y = "Number of admissions")+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

(ts3+labs(tag="A"))/
  (hm_ch1+labs(tag="B"))/
  (ev_per+labs(tag="C"))/
  (cv_per+labs(tag="D"))

ggsave("./fig_manuscript/fig3_new.svg",
       width = 10,height = 15,dpi = 500)

time_f <- incidence1 %>%
  group_by(adm_week) %>%
  count() %>%
  ungroup()

viro2 <- viro2 %>%
  mutate(pos = case_when(
    sero_gr == "EV-A71" ~ TRUE,
    sero_gr != "EV-A71" ~ FALSE),
    peak = case_when(
      month(admission_date) >= 6 & month(admission_date) <= 8 ~ "6/2023 - 8/2023",
      month(admission_date) >= 9 ~ "9/2023 - 12/2023"
    ),
    adm_week = as.Date(floor_date(admission_date, "week")),
    adm_month = as.Date(floor_date(admission_date, "month")),
    time = as.numeric(adm_month)
  )

modelll <- gam(pos~s(time,bs = "bs",k=7),family = binomial,method = "REML",data = viro2)

link_inv <- modelll$family$linkinv

recon_epicurve <- modelll %>%
  predict(list(time = time_f %>%
                 filter(adm_week >= min(viro2$admission_date)) %>%
                 pull(adm_week) %>% head(-4) %>%
                 as.numeric()
  ),se.fit = TRUE) %>%
  as_tibble() %>%
  mutate(time = time_f %>%
           filter(adm_week >= min(viro2$admission_date)) %>%
           pull(adm_week) %>% head(-4),
         lwr = link_inv(fit - 1.96*se.fit),
         upr = link_inv(fit + 1.96*se.fit),
         fit = link_inv(fit)
         ) %>%
  select(-se.fit) %>%
  left_join(time_f,., by = join_by(adm_week == time)) %>%
  mutate(e_ev71 = fit*n,
         e_ev71_lwr = lwr*n,
         e_ev71_upr = upr*n) %>%
  replace(is.na(.),-1) %>%
  ggplot(aes(x = adm_week))+
  geom_line(aes(y = fit*1000))+
  geom_ribbon(aes(y = fit*1000,ymin = lwr*1000,ymax = upr*1000),fill = "blue",alpha = .1)+
  geom_line(aes(y = e_ev71))+
  geom_ribbon(aes(y = e_ev71_upr,ymin = e_ev71_lwr,ymax = e_ev71_upr),fill = "red",alpha = .7)+
  scale_y_continuous(
    name = "CH1 admission",
    sec.axis = sec_axis(~ . /1000, name = "EV-A71 percentage"),
    limits = c(0,1000)
  ) +
  geom_col(aes(y=n),alpha = 0.3)+
  geom_point(data = dt_vr, aes(x = adm_month,y = per*1000))+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")))+
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

recon_epicurve

