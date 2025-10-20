
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
                       mutate(attack = (fit.y - fit.x) / (1 - fit.x)))

attack_rates %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = cohort, y = attack))+
  geom_line()+
  # geom_ribbon(aes(ymin = lwr_atk, ymax = upr_atk),fill = "blue", alpha = 0.4)+
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
                           incidence = (1 - fit.x) * attack *
                             predict(mod, list(age = cohort))))

incidences %>%
  bind_rows(.id = "id") %>%
  group_by(id) %>%
  group_modify(~.x %>%
                 scam(incidence ~ s(cohort,bs = "po"),data = .) %>%
                 predict(se.fit = TRUE) %>%
                 c(list(age_gr2 = incidences[[1]]$cohort), .) |>
                 as_tibble() |>
                 mutate(lwr = link_inv(fit - 1.96 * se.fit),
                        upr = link_inv(fit + 1.96 * se.fit),
                        fit = link_inv(fit)) |>
                 select(- se.fit)) %>%
  ggplot(aes(x = age_gr2, y = fit))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "blue")+
  facet_wrap(~id,scale = "free")


incidences %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = cohort, y = incidence))+
  geom_line()+
  # geom_ribbon(aes(ymin = lwr_incid, ymax = upr_incid),fill = "blue", alpha = 0.4)+
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
    mutate(age_gr2 = as.numeric(age_gr)-0.5) %>%
    scam(n ~ s(age_gr2,bs = "po"),data = .) %>%
    predict(list(age_gr2 = incidences[[i]]$cohort))%>%
    tibble(age = incidences[[i]]$cohort,
           incidence  = .)
}

ouut %>%
  bind_rows(.id = "id") %>%
  filter(fit >0) %>%
  ggplot(aes(x = age_gr2, y = fit))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "blue",alpha = 0.5)+
  facet_wrap(~id)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,2000))+
  scale_x_continuous(breaks = seq(0,15))+
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
  scale_x_continuous(breaks = seq(0,15))+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())


ouut <- list()
dat <- list()

for (i in 1:3){

   dat[[i]] <- incidence1 %>%
    filter(adm_date2 >= as.numeric(mean_collection_times[i]) &
             adm_date2 <= as.numeric(mean_collection_times[i+1])) %>%
     mutate(age_gr = cut(cohort, breaks = seq(0,16),right = T)) %>%
    na.omit(age_gr) %>%
    group_by(age_gr) %>%
    count() %>%
    mutate(age_gr2 = as.numeric(age_gr)) %>%
    ungroup() %>%
    select(-age_gr)

   ouut[[i]] <- scam(n ~ s(age_gr2,bs = "po"),data = dat[[i]]) %>%
    predict(list(age_gr2 = incidences[[i]]$cohort))%>%
    tibble(age = incidences[[i]]$cohort,
           incidence  = .)
}


cbind(bind_rows(dat,.id = "id"),
          bind_rows(ouut,.id = "id"))



cases_incid <- ggplot()+
  geom_col(data = dat %>%
             bind_rows(.id = "id"),
           aes(x = age_gr2, y = n))+
  geom_line(data = ouut %>%
              bind_rows(.id = "id"),
            aes(x = age,y = incidence)) +
  facet_wrap(~factor(id,labels = c("Dec 2022 - Apr 2023",
                                   "Apr 2023 - Aug 2023",
                                   "Aug 2023 - Dec 2023")))+
  coord_cartesian(ylim=c(0,2000))+
  scale_x_continuous(breaks = seq(0,14,by = 2),
                     limits = c(0,14))+
  scale_y_continuous(breaks = seq(0,2000,by = 200))+
  labs(tag = "A",
       y = "Total cases count",
       x = "Age (years)")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())


ex_incid_cm <- incidences %>%
  bind_rows(.id = "id") %>%
  ggplot(aes(x = cohort, y = incidence))+
  geom_line()+
  theme_bw()+
  labs(tag = "B",
       y = "Total cases count",
       x = "Age (years)")+
  facet_wrap(~factor(id,labels = c("Dec 2022 - Apr 2023",
                                   "Apr 2023 - Aug 2023",
                                   "Aug 2023 - Dec 2023")))+
  coord_cartesian(ylim=c(0,12000))+
  scale_x_continuous(breaks = seq(0,14,by = 2),
                     limits = c(0,14))+
  scale_y_continuous(breaks = seq(0,12000,by = 2000))+
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank())


cases_incid/
  ex_incid_cm/
  ts_cm

# map2(ouut,
#      incidences, ~ inner_join(.x, .y, by = join_by(age == age.x)) %>%
#        mutate(prob = incidence.y/incidence.x)) %>%
#   bind_rows(.id = "id") %>%
#   ggplot(aes(x = cohort, y = prob))+
#   geom_line()+
#   facet_wrap(~id,scale = "free")


## probability of hospitalization

#### prob_hos * age_distribution 2023 = children catched in CH1
#### children catched in CH1*seroprevalence = expected infectious in catchment area


centroids <- st_centroid(qhtp)

district_xy <- centroids %>%
  mutate(
    lon = st_coordinates(centroids)[,1],
    lat = st_coordinates(centroids)[,2]
  ) %>%
  select(district = varname_2, lon, lat) %>%
  as.data.frame() %>%
  select(-geom)

c_ad <- df1 %>%
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
  select(age,district2) %>%
  group_by(district2,age) %>%
  filter(age < 17) %>%
  count() %>%
  ungroup()

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


birth_2019 <- count_dangky_week %>%
  filter(birth_year == 2019) %>%
  mutate(district = district_reg %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  filter(district %in% unique(N_ad$district)) %>%
  group_by(district) %>%
  summarise(n=sum(n),.groups = "drop") %>%
  mutate(age = 0)


## hospitalizations probability
prob_a_d <- left_join(c_ad,rbind(birth_2019,N_ad),
          by = join_by(age == age,
                       district2 == district)) %>%
  mutate(prob_hos=n.x/n.y)


ggplot(prob_a_d, aes(x = age,y=prob_hos,color = district2))+
  geom_line()

district_xy

# prob_h_xy <- left_join(prob_a_d,district_xy, by = join_by(district2 == district)) %>%
#   mutate(lon2 = lon*100-10000,
#          lat2 = lat*100-1000)

model_prob_h <- gam(prob_hos ~ s(age)+te(lon2,lat2),
    method = "REML",
    data = prob_h_xy)


age_new <- age_dis_fn %>% filter(date == range(date)[2]) %>%
  select(age,fit) %>% pull(age)

new_data <- expand.grid(age = age_new,
                        lon2 = unique(prob_h_xy$lon2),
                        lat2 = unique(prob_h_xy$lat2)
                        )

pred_prob <- cbind(new_data,fit = predict(model_prob_h,newdata = new_data,"response"))

pred_prob %>% left_join(.,unique(prob_h_xy[,c("district2","lon2","lat2")]),
                        by = join_by(lon2,lat2)) %>% na.omit() %>%
  ggplot(aes(x = age,y = fit))+
  geom_line()+
  geom_point(data = prob_a_d,aes(x = age,y = prob_hos))+
  facet_wrap(~district2)

## model for each district

pred_prob <- prob_a_d %>%
  group_by(district2) %>%
  group_modify(~ {
    # Fit the GAM for each district
    mod <- gam(prob_hos ~ s(age,bs = "bs"), data = .x, method = "REML")

    # Predict for new ages
    pred <- predict(mod, newdata = tibble(age = age_new), type = "response")

    # Return tidy tibble
    tibble(age = age_new, fit = pred)
  }) %>%
  ungroup()

ggplot()+
  geom_line(data = pred_prob, aes(x = age,y = fit))+
  geom_point(data = prob_a_d,aes(x = age,y = prob_hos))+
  facet_wrap(~district2)

pred_prob %>% View()

exp_age_23 <- age_dis_fn %>% filter(date == range(date)[2]) %>%
  select(age,fit) %>%
  right_join(.,pred_prob,by = join_by(age)) %>%
  arrange(district2) %>%
  mutate(expected_admission = fit.x*fit.y)


expected_age_cm <- exp_age_23 %>%
  filter(district2 %in% district_consider) %>%
  group_by(age) %>%
  summarise(n = sum(expected_admission))

age_pro5 <- age_profile_constrained_cohort2(hfmd_cm,age_values = expected_age_cm$age)

attack_rates2 <- map2(head(age_pro5, -1),
                      age_pro5[-1],
                     ~ left_join(na.exclude(.x), na.exclude(.y), "cohort")|>
                       mutate(sp_gap = (fit.y - fit.x)))

attack_rates2 %>% bind_rows() %>% View()

map(attack_rates2, ~ left_join(.x,expected_age_cm, by = join_by(cohort ==age))) %>%
  bind_rows(.id = "id") %>%
  mutate(exp_in = sp_gap*n) %>%
  ggplot()+
  geom_line(aes(x = cohort,y = exp_in))+
  geom_histogram(data = dat %>% bind_rows(.id = "id"),
                 aes(cohort),binwidth = 0.5,
                 color = "white",fill = "black",alpha = 0.2)+
  facet_wrap(~factor(id,labels = c("12/2022 - 4/2023",
                                   "4/2023 - 8/2023",
                                   "8/2023 - 12/2023")))


left_join(attack_rates2,expected_age_cm, by = join_by(cohort == age))

age_structure

exp_age_23 %>%
  ggplot(aes(x = age,y = expected_admission))+
  geom_line()+
  scale_x_continuous(limits = c(0,7),
                     breaks = seq(0,7,by=1))+
  facet_wrap(~district2)


age_dis_fn %>% filter(date == range(date)[2]) %>%
  select(age,fit) %>%
  right_join(.,pred_prob,by = join_by(age)) %>%
  arrange(district2) %>%
  mutate(expected_admission = fit.x*fit.y)


#   ggplot(aes(x= age,y = fit))+
#   geom_line()
#
# prob_a_d


age_structure

## smooth heatmap as function of age and time

count_a_t_23 <- df23 %>% select(adm_week,age) %>%
  group_by(adm_week,age) %>%
  count() %>%
  filter(age <7) %>%
  ungroup() %>%
  complete(adm_week,age, fill = list(n = 0))




co <- data.frame()

for (i in 0:6){
  gen <- seq(0,1,le=53) + i
  co <- rbind(co,gen)
}


ch <- data.frame(date = count_a_t_23$adm_week %>% unique(),
                 c0 = as.numeric(co[1,]),
                 c1 = as.numeric(co[2,]),
                 c2 = as.numeric(co[3,]),
                 c3 = as.numeric(co[4,]),
                 c4 = as.numeric(co[5,]),
                 c5 = as.numeric(co[6,]))

ch <- ch %>% mutate(
  trend = c(rep("#80FFFFFF",23),
            rep("#FFFFFFFF",30))
)

ch$group <- consecutive_id(ch$trend)
ch <- head(do.call(rbind, by(ch, ch$group, rbind, NA)), -1)
ch[, c("trend", "group")] <- lapply(ch[, c("trend", "group")], na.locf)
ch[] <- lapply(ch, na.locf, fromLast = T)

ch$trend <- factor(ch$trend,
                   levels = c("#80FFFFFF","#FFFFFFFF"))

count_a_t_23 %>%
ggplot(aes(x=adm_week, y=age, fill = n)) +
  theme_minimal()+
  geom_raster(interpolate = TRUE)+
  scale_y_reverse(name = "Age (years)",
                  breaks = seq(0,6,by=1))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")+
  labs(
    # tag = "C",
    fill = "Number of admission")+
  scale_fill_paletteer_c("grDevices::Inferno")+
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
        legend.position = "top",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20,
                              label.position="top"),
         color = "none")

ggplot(data=wwww$wdat, aes(x=date, y=age)) +
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
  labs(tag = "D",fill = "Number of hospitalizations")+
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
        legend.position = "top",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20,
                              label.position="top"),
         color = "none")


# constrained_age_profiles_cohort2_cm %>%
#   bind_rows() %>%
  # ggplot(aes(x = age, y = fit)) +
  # geom_line(aes(x = age, fit))+

mean_collection_times

constrained_age_profiles_cohort2_cm %>%
  bind_rows() %>%
  ggplot(aes(x = age, y = fit)) +
  geom_line(aes(x = age, fit))+
  geom_point(data = data_pt %>%
               mutate(collection_time = factor(col_time,
                                               levels = c("Dec 2022","Apr 2023","Aug 2023","Dec 2023"))),
             aes(x = age, y = pos),shape = "|")+
  facet_wrap(~factor(collection_time,
                     labels = c("Dec 2022","Apr 2023","Aug 2023","Dec 2023")),
             ncol = 4)+
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.3) +
  labs(y = "Seroprevalence (%)",x = "Age (years)",tag="B")+
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

age_new <- age_dis_fn %>% filter(date == range(date)[2]) %>%
  select(age,fit) %>% pull(age)

c(age_new,seq(7,15,le = 512))



c_ad <- df1 %>%
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
  select(age,district2) %>%
  group_by(district2,age) %>%
  filter(age < 17) %>%
  count() %>%
  ungroup()


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

birth_2019 <- count_dangky_week %>%
  filter(birth_year == 2019) %>%
  mutate(district = district_reg %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower()) %>%
  filter(district %in% unique(N_ad$district)) %>%
  group_by(district) %>%
  summarise(n=sum(n),.groups = "drop") %>%
  mutate(age = 0)

prob_a_d <- left_join(c_ad,rbind(birth_2019,N_ad),
                      by = join_by(age == age,
                                   district2 == district)) %>%
  mutate(prob_hos=n.x/n.y)


pred_prob <- prob_a_d %>%
  group_by(district2) %>%
  group_modify(~ {
    mod <- gam(prob_hos ~ s(age,bs = "bs"), data = .x, method = "REML")
    pred <- predict(mod,
                    newdata = tibble(age = c(age_new,seq(7,15,le = 512))),
                    type = "response")
    tibble(age = c(age_new,seq(7,15,le = 512))  , fit = pred)
  }) %>%
  ungroup()


exp_age_23 <- age_dis_fn %>% filter(date == range(date)[2]) %>%
  select(age,fit) %>%
  right_join(.,pred_prob,by = join_by(age)) %>%
  arrange(district2) %>%
  mutate(expected_admission = fit.x*fit.y)

exp_age_23 %>%
  ggplot(aes(x = age,y = expected_admission))+
  geom_line()+
  scale_x_continuous(limits = c(0,15),
                     breaks = seq(0,15,by=1))+
  labs(y = "Expected CH1 admission")+
  facet_wrap(~district2)+
  theme_bw()+
  theme(axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))
