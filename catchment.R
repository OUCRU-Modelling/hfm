
hfmd

number_sero <- data_pt %>%
  group_by(qhchuan) %>%
  count()

data_pt %>%
  group_by(qhchuan) %>%
  count() %>%
  left_join(qhtp, ., by = join_by(varname_2 == qhchuan)) %>%
  ggplot() +
  geom_sf(aes(fill = n),show.legend = T)+
  scale_fill_continuous(low="yellow", high="red",
                        guide="colorbar",na.value="white")+
  geom_sf_text(aes(label = nl_name_2),size=2)+
  geom_sf(data = tdnd1, shape = 17,
          color = "blue", size = 2)+
  theme_void()  +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    ))


df1 %>% filter(year(adm_week) == "2023") %>%
  filter(!is.na(adm_week) ) %>%
  mutate(district2 = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  group_by(district2) %>%
  count() %>%
  left_join(.,number_sero, by = join_by(district2 == qhchuan)) %>%
  ggplot(aes(x =n.x, y =n.y))+
  geom_point()


library(ggstatsplot)

timepoint <- c("Aug 2023","Dec 2023")

date_range <- data_pt %>%
  filter(col_time %in% timepoint & qhchuan %in% district_consider) %>%
  pull(col_date) %>%
  range()

num_sero_1222 <- data_pt %>%
  filter(col_time %in% timepoint & qhchuan %in% district_consider) %>%
  group_by(qhchuan) %>%
  count()

df1 %>% filter(year(adm_week) == "2023" &
                 !is.na(adm_week) &
                 adm_week >= date_range[1] &
                 adm_week <= date_range[2]) %>%
  mutate(district2 = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  group_by(district2) %>%
  count()  %>%
  left_join(.,num_sero_1222, by = join_by(district2 == qhchuan)) %>%
  ggscatterstats(
    x = n.x,
    y = n.y,
    bf.message = FALSE,
    marginal = FALSE,
    label.var = district2,
    xlab = "Number of HFMD cases in 2023",
    ylab = "Number of serum samples in 2023"
  )


## calculate catchment area by cases ratio

library(readr)
## cases district per age
table1 <- read_csv("D:/OUCRU/hfmd/data/table1.csv")
## total cases cases district
table2 <- read_csv("D:/OUCRU/hfmd/data/table2.csv")
## pop district
table3 <- read_csv("D:/OUCRU/hfmd/data/table3.csv")

pop_dis_hcm <- table3 %>%
  mutate(age2 = word(age,1),
         age2 = as.numeric(age2)) %>%
  filter(age <= 16) %>%
  mutate(district1 = district1 %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  group_by(district1) %>%
  summarise(pop = sum(pop))



CH1_obs <- table1$CH1 %>% sum(na.rm = TRUE)

ch1_cum_case_rate <- CH1_obs/sum(pop_dis_hcm$pop)

library(epitools)
library(sf)

tdnd1 <- data.frame(long = 106.6708,
                    lat  = 10.7692) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


cm_ch1_cases_ratio <- table1 %>% select(district1,CH1) %>%
  mutate(district1 = district1 %>%
           str_replace_all(
             c("Thanh pho Thu Duc" = "Thu Đuc")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  group_by(district1) %>%
  summarise(cases = sum(CH1,na.rm = TRUE)) %>%
  left_join(.,pop_dis_hcm, by = join_by(district1)) %>%
  replace(is.na(.), 0) %>%
  mutate(expected_cases = pop*ch1_cum_case_rate,
         cum_case_ratio = cases/expected_cases,
         lwr = pois.exact(cases)$lower/expected_cases,
         upr = pois.exact(cases)$upper/expected_cases,
         cut = cut(upr,
                   breaks = c(0,1,2,3,10),
                   labels = c("< 1",">= 1",">= 2",">= 3"),
                   right = F)
  )

cm_ch1_cases_ratio %>%
  left_join(qhtp, ., by = join_by(varname_2 == district1)) %>%
  ggplot() +
  geom_sf(aes(fill = cut,geometry = geom),show.legend = T)+
  scale_fill_manual(
    values = c(
      "< 1" = "#FFFFFFFF",
      ">= 1" = "#6BAED6FF",
      ">= 2" = "#2171B5FF",
      ">= 3" = "#08306BFF"
    ),
    name = "95% CI Upper bound of \n cumulative case ratio of each districts"
  )+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=2,color = "black")+
  geom_sf(data = tdnd1, shape = 17,
          color = "yellow", size = 1)+
  theme_void()

##

district_consider <- cm_ch1_cases_ratio %>%
  filter(cut != "< 1") %>%
  pull(district1) %>%
  as.character()


### caculate the age profile of catchment area from serological data
library(stringi)
constrained_age_profiles_cohort2 <- age_profile_constrained_cohort2(hfmd)

age_structure <- census2019 |>
  filter(province == "Thành phố Hồ Chí Minh") |>
  mutate(district = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  filter(district %in% district_consider) %>%
  group_by(age) |>
  summarise(n = sum(n)) |>
  mutate(across(age, ~ stringr::str_remove(.x, " tuổi| \\+") |> as.integer())) |>
  arrange(age) |>
  filter(age < 17)

mod <- lm(n ~ age, age_structure)

incidences <- map(attack_rates,
                  ~ mutate(.x, incidence = (1 - fit.x) * attack *
                             predict(mod, list(age = age.x))))

incidences %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = age.x, y = incidence,color = id))+
  geom_line()+
  coord_cartesian(ylim = c(0,20000))+
  theme(legend.position = "hide")+
  theme_minimal()+
  theme(legend.position = "hide")

df1 %>%
  filter(medi_cen %in% c("Bệnh viện Nhi đồng 1",
                           "Bênh viện Nhi Đồng 1",
                           "Bệnh viện Nhi Đồng 1"))


incidence1 <- df1 %>%
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
           tolower()) %>%
  filter(district2 %in% district_consider) %>%
  mutate(adm_date2 = as.numeric(adm_date),
         cohort = interval(dob, "2023-01-01") / years(1)) %>%
  select(adm_date2,cohort)

mean_collection_times <- hfmd |>
  group_by(collection) |>
  summarise(mean_col_date = mean(col_date2)) |>
  with(setNames(mean_col_date, collection))

ouut <- list()

for (i in 1:3){
  ouut[[i]] <- incidence1 %>%
    filter(adm_date2 >= as.numeric(mean_collection_times[i]) &
             adm_date2 <= as.numeric(mean_collection_times[i+1])) %>%
    mutate(age_gr = cut(cohort, breaks = seq(0,16),right = T)) %>%
    na.omit(age_gr) %>%
    group_by(age_gr) %>%
    count() %>%
    mutate(age_gr2 = as.numeric(age_gr)) %>%
    gam(n ~ s(age_gr2),method = "REML",data = .) %>%
    predict(list(age_gr2 = incidences[[i]]$age.x))%>%
    tibble(age = incidences[[i]]$age.x,
           incidence  = .)
}

ouut %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = age, y = incidence,color = id))+
  geom_line()+
  coord_cartesian(ylim = c(0,10000))+
  theme_minimal()+
  theme(legend.position = "hide")


map2(ouut,
     incidences, ~ inner_join(.x, .y, by = join_by(age == age.x)) %>%
       mutate(prob = (incidence.x/incidence.y)*100)) %>%
  bind_rows(.id = "id") %>%
  pivot_longer(cols = c(incidence.x, incidence.y),
               names_to = "series", values_to = "y") %>%
  ggplot(aes(x = age, y = y, color = factor(series,labels = c("Cases","Sero"))))+
  geom_line()+
  theme_bw()+
  labs(tag = "A",color = "Data",
       y = "Cummulative incidence",
       x = "Age (years)")+
  facet_wrap(~factor(id,labels = c("Dec 2022 - Apr 2023",
                                   "Apr 2023 - Aug 2023",
                                   "Aug 2023 - Dec 2023")))+
  coord_cartesian(ylim=c(0,20000))+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18))


###

## function to calculate age profile
hfmd <- data_pt %>%
  as_tibble() |>
  mutate(collection = id |>
           str_remove(".*-") |>
           as.numeric() |>
           divide_by(1e4) |>
           round(),
         col_date2 = as.numeric(col_date),
         across(pos, ~ .x > 0))

age_profile <- function(data, age_values = seq(0, 15, le = 512), ci = .95) {
  model <- gam(pos ~ s(age), binomial, data)
  link_inv <- family(model)$linkinv
  df <- nrow(data) - length(coef(model))
  p <- (1 - ci) / 2
  model |>
    predict(list(age = age_values), se.fit = TRUE) %>%
    c(list(age = age_values), .) |>
    as_tibble() |>
    mutate(lwr = link_inv(fit + qt(    p, df) * se.fit),
           upr = link_inv(fit + qt(1 - p, df) * se.fit),
           fit = link_inv(fit)) |>
    select(- se.fit)
}


age_profile_unconstrained <- function(data, age_values = seq(0, 15, le = 512),
                                      ci = .95) {
  data |>
    group_by(collection) |>
    group_map(~ age_profile(.x, age_values, ci))
}

shift_right <- function(n, x) {
  if (n < 1) return(x)
  c(rep(NA, n), head(x, -n))
}

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
                   predict2(list(collection_time = mean_collection_times)) %>%
                   tibble(collection_time = mean_collection_times,
                          seroprevalence  = .)) |>
    ungroup() |>
    # Step 2b:
    left_join(age_time_inv, c("cohort", "collection_time")) |>
    group_by(collection_time, line) |>
    group_modify(~ .x |>
                   right_join(tibble(age = age_values), "age") |>          ### added
                   arrange(age) |>                                         ### added
                   mutate(across(seroprevalence,
                                 ~ gam(.x ~ s(age), betar) |>
                                   predict2(list(age = age_values))))) |>  ### modified
    ungroup() |>
    pivot_wider(names_from = line, values_from = seroprevalence) |>
    group_by(collection_time) |>
    group_split()
}

constrained_age_profiles <- age_profile_constrained(hfmd)

hfmd_cm <- hfmd %>% filter(qhchuan %in% district_consider)
constrained_age_profiles_cohort2_cm <- age_profile_constrained_cohort2(hfmd_cm)

constrained_age_profiles_cm %>%
  bind_rows() %>%
  ggplot(aes(x = age, y = fit)) +
  geom_line(aes(x = age, fit))+
  # geom_point(data = data_pt,aes(x = age, y = pos),shape = "|")+
  facet_wrap(~factor(collection_time,
                     labels = c("Dec 2022","Apr 2023","Aug 2023","Dec 2023")),
             ncol = 4)+
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.3) +
  labs(y = "Seroprevalence (%)",x = "Age (years)")+
  scale_y_continuous(labels = scales::label_percent(scale = 100),limits = c(0,1))+
  # coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 18))

attack_rates <- map2(head(constrained_age_profiles_cohort2_cm, -1),
                     constrained_age_profiles_cohort2_cm[-1],
                     ~ left_join(na.exclude(.x), na.exclude(.y), "cohort") |>
                       mutate(attack = (fit.y - fit.x) / (1 - fit.x),
                              upr_atk = (upr.y - upr.x) / (1 - upr.x),
                              lwr_atk = (lwr.y - lwr.x) / (1 - lwr.x)))
attack_rates %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = cohort, y = attack))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr_atk, ymax = upr_atk),fill = "blue", alpha = 0.4)+
  # coord_cartesian(ylim = c(0,15000))+
  facet_wrap(~id)+
  theme_minimal()+
  theme(legend.position = "hide")

## population

age_structure <- census2019 |>
  filter(province == "Thành phố Hồ Chí Minh") |>
  mutate(district = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  filter(district %in% district_consider) %>%
  group_by(age) |>
  summarise(n = sum(n)) |>
  mutate(across(age, ~ stringr::str_remove(.x, " tuổi| \\+") |> as.integer())) |>
  arrange(age) |>
  filter(age < 17)

mod <- lm(n ~ age, age_structure)

incidences <- map(attack_rates,
                  ~ mutate(.x,
                           incidence = (1 - fit.x) * attack * predict(mod, list(age = cohort)),
                           upr_incidence = (1 - upr.x) * upr_atk * predict(mod, list(age = cohort)),
                           lwr_incidence = (1 - lwr.x) * lwr_atk * predict(mod, list(age = cohort))))

incidences %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = cohort, y = incidence))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr_incidence, ymax = upr_incidence),fill = "blue", alpha = 0.4)+
  coord_cartesian(ylim = c(0,15000))+
  facet_wrap(~id)+
  theme_minimal()+
  theme(legend.position = "hide")


incidence1 <- df1 %>%
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
           tolower()) %>%
  filter(district2 %in% district_consider) %>%
  mutate(adm_date2 = as.numeric(adm_date),
         cohort = interval(dob, "2023-01-01") / years(1)) %>%
  select(adm_date2,cohort)

mean_collection_times <- hfmd |>
  group_by(collection) |>
  summarise(mean_col_date = mean(col_date2)) |>
  with(setNames(mean_col_date, collection))

ouut <- list()

for (i in 1:3){
  ouut[[i]] <- incidence1 %>%
    filter(adm_date2 >= as.numeric(mean_collection_times[i]) &
             adm_date2 <= as.numeric(mean_collection_times[i+1])) %>%
    mutate(age_gr = cut(cohort, breaks = seq(0,16),right = T)) %>%
    na.omit(age_gr) %>%
    group_by(age_gr) %>%
    count() %>%
    mutate(age_gr2 = as.numeric(age_gr)) %>%
    gam(n ~ s(age_gr2),method = "REML",data = .) %>%
    predict(list(age_gr2 = incidences[[i]]$cohort))%>%
    tibble(age = incidences[[i]]$cohort,
           incidence  = .)
}

ouut %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = age, y = incidence,color = id))+
  geom_line()+
  coord_cartesian(ylim = c(0,10000))+
  theme_minimal()+
  theme(legend.position = "hide")


map2(ouut,
     incidences, ~ inner_join(.x, .y, by = join_by(age == age.x)) %>%
       mutate(prob = (incidence.x/incidence.y)*100)) %>%
  bind_rows(.id = "id") %>%
  pivot_longer(cols = c(incidence.x, incidence.y),
               names_to = "series", values_to = "y") %>%
  ggplot(aes(x = cohort, y = y, color = factor(series,labels = c("Cases","Sero"))))+
  geom_line()+
  theme_bw()+
  labs(tag = "A",color = "Data",
       y = "Cummulative incidence",
       x = "Age (years)")+
  facet_wrap(~factor(id,labels = c("Dec 2022 - Apr 2023",
                                   "Apr 2023 - Aug 2023",
                                   "Aug 2023 - Dec 2023")))+
  coord_cartesian(ylim=c(0,15000))+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18))

map2(ouut,
     incidences, ~ inner_join(.x, .y, by = join_by(age == age.x)) %>%
       mutate(prob = (incidence.x/incidence.y)*100)) %>%
  bind_rows(.id = "id") %>%
  pivot_longer(cols = c(incidence.x, incidence.y),
               names_to = "series", values_to = "y") %>%
  ggplot(aes(x = cohort, y = y, color = factor(series,labels = c("Cases","Sero"))))+
  geom_line()+
  theme_bw()+
  labs(tag = "A",color = "Data",
       y = "Cummulative incidence",
       x = "Age (years)")+
  facet_wrap(~factor(id,labels = c("Dec 2022 - Apr 2023",
                                   "Apr 2023 - Aug 2023",
                                   "Aug 2023 - Dec 2023")))+
  coord_cartesian(ylim=c(0,15000))+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18))



# map2(ouut,
#      incidences, ~ inner_join(.x, .y, by = join_by(age == age.x)) %>%
#        mutate(prob = incidence.y/incidence.x)) %>%
#   bind_rows(.id = "id") %>%
#   ggplot(aes(x = cohort, y = prob))+
#   geom_line()+
#   facet_wrap(~id,scale = "free")

