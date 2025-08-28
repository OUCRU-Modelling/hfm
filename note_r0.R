library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)
library(patchwork)
library(odin)
library(tsiR)
library(janitor)
invisible(Sys.setlocale("LC_TIME", "English"))
library(gtsummary)

cases1723 <- rbind(d17,d18,d19,d20,d21,d22,d23)

birth1723 <- model$df %>%
  select(week,year,fit)


cases1723 <- cases1723 %>%
  mutate(
    biweek = (week + 1) %/% 2   # groups weeks into pairs
  ) %>%
  group_by(year, biweek) %>%
  summarise(biweek_cases = sum(cases), .groups = "drop")

birth1723 <- birth1723 %>%
  mutate(
    biweek = (week + 1) %/% 2   # groups weeks into pairs
  ) %>%
  group_by(year, biweek) %>%
  summarise(biweek_birth = sum(fit), .groups = "drop")

hcm1723 <- left_join(cases1723,birth1723, by = c("biweek" = "biweek","year" = "year"))


generator <- odin::odin({
  deriv(N) <- r * N * (1 - N / K)
  initial(N) <- N0

  N0 <- user(1)
  K <- user(100)
  r <- user()
})

mod2 <- generator$new(N0 = 8446000,r = 7.4/52,K= 9456700)
y1723 <- mod2$run(1:nrow(hcm1723))

hcm1723 <- cbind(hcm1723,y1723)

datatsir <- hcm1723 %>%
  mutate(
    time = seq(2017, 2023 + 1, length.out = n())[-(n() + 1)]
  ) %>% select(time,biweek_cases,biweek_birth,N)%>%
  magrittr::set_colnames(c("time","cases","births","pop"))

hcm_hfmd1723 <- runtsir(data = datatsir, IP = 2, xreg = "cumcases",
                        regtype = "lowess",alpha = NULL, sbar = NULL,
                        method = "negbin", nsim = 1000,
                        family = "gaussian", link = "identity")

datatsir %>%
  ggplot() +
  geom_line(aes(x = time ,group = 1, y = hcm_hfmd1723$res$mean,
                linetype = "model fitted"))+
  geom_line(aes(x= time, group = 1, y = cases,
                linetype = "cases reported"))+
  # geom_bar(aes(x= as.character(date), y = cases),stat = "identity")+
  theme_minimal()+
  labs(y = "Cases",x="Time")+
  scale_linetype_manual(values = c("cases reported" = "dashed",
                                   "model fitted" = "solid"),
                        name="Analysis Type")+
  geom_vline(xintercept = 2021.491,
             alpha = 0.25,col = "blue",lwd = 0.5)+
  geom_vline(xintercept = 2021.491+(7/365*4)*4,
             alpha = 0.25,col = "blue",lwd = 0.5)


after <- datatsir %>%  filter(time >= 2022)
hcmafter <- runtsir(data = after, IP = 2, xreg = "cumcases",
                    regtype = "lowess",alpha = NULL, sbar = NULL,
                    method = "deterministic", nsim = 1000,
                    family = "gaussian", link = "identity")

after %>%
  ggplot() +
  geom_line(aes(x = time ,group = 1, y = hcmafter$res$mean,
                linetype = "model fitted"))+
  geom_line(aes(x= time, group = 1, y = cases,
                linetype = "cases reported"))+
  # geom_bar(aes(x= as.character(date), y = cases),stat = "identity")+
  theme_minimal()+
  labs(y = "Cases",x="Time")+
  scale_linetype_manual(values = c("cases reported" = "dashed",
                                   "model fitted" = "solid"),
                        name="Analysis Type")

## district

#### birth data
hcm_birth_data <- readRDS("D:/OUCRU/hfmd/hcm_birth_data.rds")
hcm_birth_data$district_reg <- hcm_birth_data$district_reg %>%
  str_replace_all(c("Quận 2"  = "Thủ Đức",
                    "Quận 9"  = "Thủ Đức"))

hcm_birth_data$district_reg <- hcm_birth_data$district_reg %>% str_replace_all(
  c( "Gò vấp"  = "Gò Vấp"))

hcm_birth_data$district_reg <- hcm_birth_data$district_reg %>%
  str_remove("Quận|Huyện|Thành phố") %>%
  trimws(which = "both")


hcm_birth_data$district_reg %>% unique()

birth_district <- hcm_birth_data %>%
  group_by(district_reg) %>%
  group_modify(~ .x |> filter(birth_year >= 2017)%>%
                 group_by(birth_week,birth_year) %>%
                 summarise(n = sum(n),.groups = "drop") %>%
                 mutate(birth_week = replace(birth_week, birth_week == 53, 52),
                        biweek = (birth_week + 1) %/% 2) %>%
                 arrange(birth_year) %>%
                 group_by(birth_year, biweek) %>%
                 summarise(biweek_birth = sum(n), .groups = "drop") %>%
                 mutate(biweek2 = 1:nrow(.))
                 )

# birth_district %>%
#   # group_by(district_reg) %>%
#   group_split() %>%
#   gam(biweek_birth ~ s(biweek) + s(biweek2),method = "REML",data = .) %>%
#                  predict(list(biweek = rep(c(1:26),2024-2017),
#                               biweek2 = 1:(26*(2024-2017)))) %>% as_vector()

cutpoint <- 135

birth_district2 <- birth_district %>% filter(biweek2 <= cutpoint) %>%
  group_split()

modelaa <- birth_district %>% filter(biweek2 <= cutpoint) %>%
  group_split() %>%
  map(\(df)gam(biweek_birth ~ s(biweek) + s(biweek2),method = "REML",data = df))

newdata <- data.frame(
  biweek  = rep(1:26, 2024 - 2017),
  biweek2 = 1:(26 * (2024 - 2017))
)

map2(modelaa, birth_district2,
     ~ predict(.x, newdata = newdata,type = "response")|> as_vector())

predicted_birth_district <- map2_dfr(modelaa, birth_district2, \(mod, df) {
  newdata %>%
    mutate(
      predicted   = predict(mod, newdata = newdata, type = "response"),
      district_reg = unique(df$district_reg),
      birth_year = rep(2017:2023, each = 26)
    )
})


## plot
predicted_birth_district %>%
  ggplot(aes(x = biweek2,y = predicted)) +
  geom_line()+
  geom_point(data = birth_district2 %>% bind_rows(),aes(x = biweek2,y = biweek_birth))+
  facet_wrap(~district_reg,ncol  = 4,scales = "free")


## cases data

df1 <- read_excel("D:/OUCRU/hfmd/data/TCM_full.xlsx",
                  col_types = c("date", "numeric", "text",
                                "text", "text", "date", "date", "date",
                                "text", "text", "text"))
colnames(df1) <- c("dob", "age", "gender", "commune", "district",
                   "reported_date", "onset_date","adm_date",
                   "medi_cen","inout","severity")
df1$dob <- df1$dob %>% as_date()
df1$adm_date <- df1$adm_date %>% as_date()

df1$age1 <- interval(df1$dob, df1$adm_date) / years(1)
df1$adm_week <- as.Date(floor_date(df1$adm_date, "week"))
df1$district <- df1$district %>% str_replace_all(
  c( "Quận Gò vấp"  = "Quận Gò Vấp"))
df1$district <- df1$district %>%
  str_remove("Quận|Huyện|Thành phố") %>%
  trimws(which = "both")


case_district <- df1 %>%
  group_by(district) %>%
  group_modify(~ .x |> filter(year(adm_date) >= 2017 & year(adm_date) <= 2023) %>%
                 group_by(adm_week) %>% count() %>%
                 mutate(week = week(adm_week),
                        year = year(adm_week),
                        week = replace(week, week == 53, 52),
                        biweek = (week + 1) %/% 2) %>%
                 group_by(year, biweek) %>%
                 summarise(biweek_cases = sum(n), .groups = "drop")
               )

case_district %>% group_split()

predicted_birth_district %>% group_by(district_reg) %>% group_split()

colnames(predicted_birth_district)
colnames(case_district)

predicted_birth_district %>% View()
case_district %>% View()

case_birth_district_1723 <- full_join(predicted_birth_district,case_district,
          by =c(
            "district_reg" = "district",
            "birth_year" = "year",
            "biweek" = "biweek"
          ))


## population
census2019 <- readRDS("D:/OUCRU/hfmd/data/census2019.rds")
census2019$district <- census2019$district %>%
  str_replace_all(c("Quận 2"  = "Quận Thủ Đức",
                    "Quận 9"  = "Quận Thủ Đức")) %>%
  str_remove_all("Quận|Huyện") %>%
  trimws(which = "both")


popqh <- census2019 %>% filter(province == "Thành phố Hồ Chí Minh") %>%
  group_by(district) %>% summarise(pop = sum(n))

r1920 <- 9227.6/9038.6
r2021 <- 9166.8/9227.6
r2122 <- 9389.7/9166.8
r2223 <- 9456.7/9389.7

popqh$pop20 <- popqh$pop*r1920
popqh$pop21 <- popqh$pop20*r2021
popqh$pop22 <- popqh$pop21*r2122
popqh$pop23 <- popqh$pop22*r2223
popqh$pop24 <- popqh$pop23*r2223

case_birth_district_1723

pop_district <- popqh %>%
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = case_when(
      year == "pop" ~ 2019L,  # base year
      TRUE ~ as.integer(sub("pop", "20", year))
    )
  ) %>%
  arrange(district, year) %>%
  group_by(district) %>%
  arrange(year) %>%
  mutate(next_pop = lead(population),
         next_year = lead(year)) %>%
  filter(!is.na(next_pop)) %>%
  group_by(district, year) %>%
  do({
    tibble(
      district = .$district,
      year = .$year,
      biweek = 1:26,
      population = seq(.$population, .$next_pop, length.out = 27)[-27]  # exclude endpoint
    )
  })

pop_district2223 <- pop_district %>%
  filter(year >= 2022)

case_birth_district_2223 <- case_birth_district_1723 %>%
  mutate(biweek_cases = replace_na(biweek_cases, 1)) %>%
  filter(birth_year >= 2022)


dt_tsir_district_2223 <- full_join(case_birth_district_2223,pop_district2223,
                                      by =c(
                                        "district_reg" = "district",
                                        "birth_year" = "year",
                                        "biweek" = "biweek"
                                      )) %>%
  select(district_reg,birth_year,biweek_cases,predicted,population)

outcome <- dt_tsir_district_2223 %>%
  group_by(district_reg) %>%
  group_modify(~.x %>% mutate(time = seq(2022, 2024, length.out = 52))) %>%
  ungroup() %>% select(district_reg,time,biweek_cases,predicted,population) %>%
  magrittr::set_colnames(c("district","time","cases","births","pop")) %>%
  group_by(district) %>%
  group_split() %>%
  map(\(df)runtsir(data = df[,-1], IP = 2, xreg = "cumcases",
                   regtype = "lowess",alpha = NULL, sbar = NULL,
                   method = "deterministic", nsim = 1000,
                   family = "gaussian", link = "identity"))


# dt_tsir_district_2223 %>%
#   group_by(district_reg) %>%
#   group_modify(~.x %>% mutate(time = seq(2022, 2024, length.out = 52))) %>%
#   ungroup() %>% select(-birth_year) %>%
#   magrittr::set_colnames(c("district","cases","births","pop","time")) %>%
#   ggplot(aes(x = time,y = cases)) +
#   geom_line()+
#   facet_wrap(~district)

bbbbb <- map(outcome, ~ data.frame(
  time = .x$time,
  fit = .x$res$mean,
  s = .x$simS$mean,
  beta = .x$contact
))

aaaaa <- dt_tsir_district_2223 %>%
  group_by(district_reg) %>%
  group_modify(~.x %>% mutate(time = seq(2022, 2024, length.out = 52))) %>%
  ungroup() %>% select(-birth_year) %>%
  magrittr::set_colnames(c("district","cases","births","pop","time")) %>%
  group_by(district) %>%
  group_split()


data_result <- map2(aaaaa, bbbbb, ~ left_join(.x, .y, by = "time"))%>%
  bind_rows()

data_result %>%
  ggplot(aes(x = time))+
  geom_line(aes(group = 1,y = cases,linetype = "cases reported"))+
  geom_line(aes(group = 1,y = fit,linetype = "model fitted"))+
  facet_wrap(~district,scales = "free",ncol = 4)+
  scale_linetype_manual(values = c("cases reported" = "dashed",
                                   "model fitted" = "solid"),
                        name="Analysis Type")+
  theme_minimal()+
  theme(legend.position = "bottom")

data_result %>% filter(district != "Thủ Đức") %>%
  ggplot(aes(x = time))+
  geom_line(aes(y = beta.beta))+
  geom_ribbon(aes(ymin = beta.betalow,
                  ymax = beta.betahigh),fill = "blue",alpha = 0.3)+
  facet_wrap(~district,ncol = 4)+
  labs(x = "time", y ="contact rate")+
  scale_x_continuous(breaks = c(2022,2023,2024))+
  theme_minimal()


# heatmap_r0 <- data_result %>%
#   group_by(district) %>%
#   group_modify(~.x %>% mutate(r0 = beta.beta*s)) %>%
#   bind_rows() %>% filter(time > 2023 & district != "Thủ Đức") %>%
#   ggplot(aes(x = as.factor(beta.time),
#               y = district,
#               fill = r0)) +
#   geom_tile()+
#   scale_fill_gradient(low="yellow", high="red",
#                       name = "Median of R(0)")+
#   theme_minimal()+
#   theme(legend.position = "bottom")

r0_hit <- data_result %>%
  group_by(district) %>%
  filter(time > 2023 & district != "Thủ Đức") %>%
  group_modify(~.x %>%
                 mutate(r0 = beta.beta*s)) %>%
  summarise(p25_r0 = quantile(r0,0.25),
            median_r0 = median(r0),
            p75_r0 = quantile(r0,0.75),
            hit = (1 - (1/median_r0))*100)

## HAC

beta_2023_district <- data_result %>%
  filter(time > 2023) %>%
  select(district,beta.time,beta.beta) %>%
  pivot_wider(names_from = beta.time,values_from = beta.beta)

library(ggdendro)
beta_2023_district <- column_to_rownames(beta_2023_district, var = "district")

d <- stats::dist(beta_2023_district)

treeC <- hclust(d, method="ward.D2")
dg <- as.dendrogram(treeC)
ddata_analytes <- dendro_data(dg, type = "rectangle")

HAC_district <- ggplot() +
  geom_segment(
    data = segment(ddata_analytes),
    aes(x = x, y = y, xend = xend, yend = yend),
    position = position_nudge(x = -0.5)
  ) +
  coord_flip(clip = "off") +
  scale_y_reverse() +
  scale_x_continuous(limits = c(0, 22), expand = c(0, 0)) +
  theme_dendro() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))
  geom_text(
    data = label(ddata_analytes),
    aes(x = x, y = -1, label = label),
    size = 3.5, color = "#444444", vjust = 2, angle = 0, hjust = 0
  )

disaaa <- label(ddata_analytes)[3] %>% as.data.frame() %>% pull(label) %>% as.character()

heatmap_r0 <- data_result %>%
  group_by(district) %>%
  group_modify(~.x %>% mutate(r0 = beta.beta*s)) %>%
  bind_rows() %>% filter(time > 2023 & district != "Thủ Đức") %>%
  ggplot(aes(x = as.factor(beta.time),
             y = factor(district,levels = disaaa),
             fill = r0)) +
  geom_tile()+
  # scale_fill_paletteer_c("grDevices::Inferno")+
  scale_fill_gradient(low="yellow", high="red",
                      name = "Median of R(0)")+
  scale_y_discrete(position = "right")+
  scale_x_discrete(name = "Contact rate")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank())

HAC_district + heatmap_r0 +
  plot_layout(widths = c(1,2))

library(paletteer)
library(patchwork)


cut <- cutree(treeC, k=5) %>% as.data.frame() %>%
  rownames_to_column(var = "district")
colnames(cut) <- c("district","cluster")

cut %>% mutate(district2 = stri_trans_general(cut$district, "latin-ascii") %>%
                    tolower() %>%
                    str_remove("district") %>%
                    trimws(which = "both")) %>%
left_join(qhtp, ., by = join_by(varname_2 == district2)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(cluster)),show.legend = T)+
  geom_sf_text(aes(label = nl_name_2),size=2.5)+
  theme_void()

# HCMC map
library(sf)
library(stringi)
map_path <- "D:/OUCRU/HCDC/project phân tích sero quận huyện/"
vn_qh <- st_read(dsn = file.path(map_path, "gadm41_VNM.gpkg"), layer = "ADM_ADM_2")

vn_qh1 <- vn_qh %>%
  clean_names() %>%     ## cho thành chữ thường
  filter(
    str_detect(
      name_1,
      "Hồ Chí Minh"
    )
  )
qhtp <- vn_qh1[-c(14,21),]

qhtp$geom[qhtp$varname_2 == "Thu Duc"] <- vn_qh1[c("21","24","14"),] %>%
  st_union()

qhtp <- qhtp %>% st_cast("MULTIPOLYGON")
qhtp$varname_2 <- stri_trans_general(qhtp$varname_2, "latin-ascii") %>%
  tolower() %>%
  str_remove("district") %>%
  trimws(which = "both")

qhtp$nl_name_2 <- c("BC","BTân","BT","CG","CC","GV",
                    "HM","NB","PN","1","10","11","12",
                    "3","4","5","6","7","8","TB",
                    "TP","TĐ")


left_join(qhtp, data.frame(data), by = join_by(varname_2 == qhchuan))

r0_hit %>% mutate(district2 = stri_trans_general(r0_hit$district, "latin-ascii") %>%
                      tolower() %>%
                      str_remove("district") %>%
                      trimws(which = "both")) %>%
left_join(qhtp, ., by = join_by(varname_2 == district2)) %>%
  ggplot() +
    geom_sf(aes(fill = median_r0),show.legend = T)+
    scale_fill_continuous(low="yellow", high="red",
                          guide="colorbar",na.value="white",
                          name = "Median of R0")+
    geom_sf_text(aes(label = nl_name_2),size=2.5)+
    theme_void()

data_result %>%
  group_by(district) %>%
  group_modify(~.x %>% mutate(r0 = beta.beta*s,
                              beta.time = beta.time*2)) %>%
  bind_rows() %>% filter(time > 2023 & district != "Thủ Đức") %>%
  ggplot(aes(x = as.factor(beta.time),
             y = factor(district,levels = disaaa),
             fill = r0)) +
  geom_tile()+
  # scale_fill_paletteer_c("grDevices::Inferno")+
  scale_fill_gradient(low="yellow", high="red",
                      name = "R0")+
  scale_y_discrete(position = "right")+
  scale_x_discrete(name = "Week")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank())

ca <- dt_tsir_district_2223 %>%
  group_by(district_reg) %>%
  group_modify(~.x %>% mutate(time = seq(2022, 2024, length.out = 52))) %>%
  ungroup() %>% select(district_reg,time,biweek_cases,predicted,population) %>%
  group_by(time) %>%
  summarise(cases = sum(biweek_cases)) %>%
  filter(time > 2023) %>% mutate(biweek = (1:nrow(.))*2) %>%
  ggplot(aes(x = biweek, y =cases)) +
  geom_bar(stat = "identity")+
  theme_minimal()+
  scale_x_discrete(breaks = seq(2,52,by=2),expand = expansion(add = c(0, 0)))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())


