library(readxl)
viro_df <- read_excel("D:/OUCRU/hfmd/data/03EI Data 2023 shared.xlsx")

library(lazymod)
devtools::install_github("OUCRU-Modelling/lazymod")

viro_df %>% group_by(SeroGroup1) %>% count()

viro_df %>% pull(DateAdmission) %>% range()

viro2 <- viro_df %>%
  mutate(city = City %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower(),
         sero_gr = case_when(
           SeroGroup1 == "ENT" ~ "EV",
           SeroGroup1 != "ENT" ~ SeroGroup1
         ),
         admission_date = as.Date(DateAdmission),
         adm_month = month(admission_date),
         age_adm = interval(DateBirth, admission_date) / years(1)) %>%
  select(city,sero_gr,admission_date,adm_month,age_adm,DateBirth)


viro_df %>%
  mutate(admission_date = as.Date(DateAdmission),
         adm_month = month(admission_date),
         age_adm = interval(DateBirth, admission_date) / years(1))
  filter(adm_month > 8) %>%
  select(age_adm,SeroGroup1) %>%
  mutate(age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  group_by(age_bin, SeroGroup1) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = age_bin, y = perc, fill = SeroGroup1)) +
  geom_col(position = "stack", color = "black") +
  labs(x = "Age (years)", y = "Percentage (%)", fill = "Serotype") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  ggplot(aes(age_adm))+
  geom_histogram(binwidth = 0.5,
                 color = "white",
                 fill = "black",
                 alpha = 0.2)

  group_by(adm_month,SeroGroup1) %>%
  count() %>%
  ggplot(aes(x = adm_month,y = n,fill = SeroGroup1))+
  geom_col()

library(gtsummary)

city_viro <- viro_df %>%
  select(City,SeroGroup1) %>%
  mutate(city = City %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower(),
         sero_gr = case_when(
           SeroGroup1 == "ENT" ~ "EV",
           SeroGroup1 != "ENT" ~ SeroGroup1
         ))

city_viro$city <- factor(city_viro$city,
                         levels = c("tp hcm",city_viro$city[!city_viro$city == "tp hcm"] %>%
                                      unique()))

city_viro %>%
  tbl_summary(by=sero_gr,
              include = c(city,sero_gr))


link <- "https://data.opendevelopmentmekong.net/dataset/999c96d8-fae0-4b82-9a2b-e481f6f50e12/resource/2818c2c5-e9c3-440b-a9b8-3029d7298065/download/diaphantinhenglish.geojson?fbclid=IwAR1coUVLkuEoJRsgaH81q6ocz1nVeGBirqpKRBN8WWxXQIJREUL1buFi1eE"

vn_spatial <- sf::st_read(link) %>% mutate(
  city =
    case_when(
      Name == "TP. Ho Chi Minh" ~ "tp hcm",
      Name != "TP. Ho Chi Minh" ~ Name
    ) %>%
    trimws(which = "both") %>%
    stri_trans_general("latin-ascii") %>%
    tolower()
)

city_viro %>%
  filter(sero_gr == "EV-A71") %>%
  group_by(city,sero_gr) %>%
  count() %>%
  ungroup() %>%
  full_join(.,vn_spatial, by = join_by(city)) %>%
  ggplot() +
  geom_sf(aes(fill = n,geometry = geometry))+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    na.value="white",
                                    name = "Number of EV-A71 samples")+
  theme_void()+
  theme(legend.position="bottom")


ggplot() +
  geom_sf(data = vn_spatial)+
  # geom_sf(data = tdoucru, shape = 23,
  #         fill = "red", size = 2)+
  theme_void()


viro2 %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month")),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  group_by(adm_month,sero_gr) %>%
  count() %>%
  ggplot(aes(x = adm_month,y=n,fill = sero_gr))+
  geom_col()+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")+
  labs(fill = "Sero group", y = "Number of samples",x = "Month of admission")+
  theme_minimal()+
  theme(legend.position="bottom")



viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  group_by(adm_month,age_bin, sero_gr) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  complete(age_bin,sero_gr,fill = list(perc=0)) %>%
  na.omit() %>%
  ggplot(aes(x = age_bin, y = perc, fill = sero_gr)) +
  geom_col(position = "stack", color = "black") +
  ggsci::scale_fill_jco() +
  # scale_x_date(date_labels = "%b",
  #              date_breaks = "1 month")+
  facet_wrap(~adm_month,ncol = 2)+
  labs(x = "Age (years)", y = "Percentage (%)", fill = "Serotype") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




aaa <- viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         adm_month2 = as.Date(floor_date(admission_date, "month"))%>% as.character(),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  filter(sero_gr == "EV-A71") %>%
  select(adm_month2,age_adm) %>%
  as.data.frame()


library(paletteer)

viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         adm_month2 = as.Date(floor_date(admission_date, "month"))%>% as.character(),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  filter(sero_gr == "EV-A71") %>%
  select(adm_month2,age_adm) %>%
  as.data.frame() %>%
ggplot(aes(x=adm_month2, y = age_adm)) +
  stat_density(
    aes(fill = after_stat(count)),
    geom = "raster",
    position = "identity",
    interpolate = TRUE
  )+
  scale_fill_paletteer_c("grDevices::Inferno")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",lim= rev(c(0,6)),breaks = seq(0,6))+
  labs(fill = "Number of EV-A71 samples")

viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         adm_month2 = as.Date(floor_date(admission_date, "month"))%>% as.character(),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  filter(sero_gr == "EV-A71" & month(adm_month2) >= 8) %>%
  select(adm_month2,age_adm) %>%
  as.data.frame() %>%
  ggplot(aes(x=adm_month2, y = age_adm)) +
  stat_density(
    aes(fill = after_stat(count)),
    geom = "raster",
    position = "identity",
    interpolate = TRUE
  )+
  scale_fill_paletteer_c("grDevices::Inferno")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",breaks = seq(0,12,by = 1))+
  labs(fill = "Number of EV-A71 samples",x = "Month of admission")+
  theme(legend.position = "bottom",
        legend.key.width =  unit(1, "cm"))


viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         adm_month2 = as.Date(floor_date(admission_date, "month"))%>% as.character(),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  filter(sero_gr == "EV-A71" & month(adm_month2) >= 6) %>%
  select(adm_month2,age_adm) %>%
  as.data.frame() %>%
  ggplot(aes(x=adm_month2, y = age_adm)) +
  stat_density(
    aes(fill = after_stat(density)),
    geom = "raster",
    position = "identity",
    interpolate = TRUE
  )+
  scale_fill_paletteer_c("grDevices::Inferno")+
  theme_minimal()+
  scale_y_reverse(name = "Age (years)",
                  breaks = seq(0,12,by = 1))+
  labs(fill = "Number of EV-A71 samples",x = "Month of admission")+
  theme(legend.position = "bottom",
        legend.key.width =  unit(1, "cm"))


viro2 %>%
  mutate(adm_month = month(admission_date,label = TRUE),
         adm_month2 = as.Date(floor_date(admission_date, "month"))%>% as.character(),
         age_bin = cut(age_adm,
                       breaks = seq(0, max(age_adm, na.rm = TRUE) + 0.5, by = 0.5),
                       right = FALSE)) %>%
  na.omit() %>%
  filter(month(adm_month2) >= 8) %>%
  group_by(age_bin,sero_gr) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  complete(age_bin,sero_gr,fill = list(perc =0)) %>%
  filter(sero_gr == "EV-A71") %>%
  ggplot(aes(x = age_bin, y = perc))+
  geom_col()+
  scale_y_continuous(name = "Percentage of EV-A71 samples",
                     labels = scales::label_percent(scale = 100))+
  labs(x = "Age group")+
  theme_minimal()



viro2 %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month"))) %>%
  group_by(adm_month,sero_gr) %>%
  count()  %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  complete(adm_month,sero_gr,fill = list(perc =0)) %>%
  filter(sero_gr == "EV-A71") %>%
  ggplot(aes(x = adm_month,y=perc))+
  geom_col()+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")+
  labs(y = "Number of samples",x = "Month of admission")+
  theme_minimal()+
  theme(legend.position="bottom")

###3


viro2 %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month"))) %>%
  group_by(adm_month,sero_gr) %>%
  count()  %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(adm_month,sero_gr,fill = list(n =0)) %>%
  group_by(adm_month) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  filter(sero_gr == "EV-A71") %>%
  ggplot(aes(x = adm_month,y=perc))+
  geom_col()+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")+
  labs(y = "Number of EV-A71 samples",x = "Month of admission")+
  theme_minimal()+
  theme(legend.position="bottom")

viro2 %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month"))) %>%
  group_by(adm_month,sero_gr) %>%
  count()  %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(adm_month,sero_gr,fill = list(n =0)) %>%
  group_by(adm_month) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  group_by(adm_month,sero_gr) %>%
  summarise(n = sum(n),.groups = "drop") %>%
  ggplot(aes(x = adm_month,y=n,fill = sero_gr))+
  geom_col()+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")+
  labs(fill = "Sero group", y = "Number of samples",x = "Month of admission")+
  theme_minimal()+
  theme(legend.position="bottom")



viro_count_p <- viro2 %>%
  mutate(adm_month = as.Date(floor_date(admission_date, "month"))) %>%
  group_by(adm_month,sero_gr) %>%
  count() %>%
  ungroup() %>%
  group_by(adm_month) %>%
  mutate(perc = n / sum(n)) %>%
  ungroup

viro_count_p %>% filter(sero_gr == "EV-A71")  %>%
  ggplot(aes(x = adm_month,y=perc))+
  geom_col()+
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month",
               limits = c(as.Date("2023-05-01"),
                          as.Date("2023-12-01")))+
  labs(y = "Percentage of EV-A71 samples",x = "Month of admission")+
  theme_minimal()+
  theme(legend.position="bottom")


###


viro2 %>%
  mutate() %>%
  na.omit() %>%
  filter(month(adm_month2) >= 9) %>%
  na.omit() %>%
  filter(sero_gr == "EV-A71") %>%
  ggplot(aes(age_adm)) +
  geom_histogram(binwidth = 0.5,
                 color = "white",fill = "black",alpha = 0.5)+
  labs(x = "Age (years)", y = "Number of EV-A71 samples") +
  theme_minimal()

#####


viro2 <- viro2 %>%
  mutate(pos = case_when(
    sero_gr == "EV-A71" ~ TRUE,
    sero_gr != "EV-A71" ~ FALSE),
    peak = case_when(
      month(admission_date) >= 6 & month(admission_date) <= 8 ~ "6/2023 - 8/2023",
      month(admission_date) >= 9 ~ "9/2023 - 12/2023"
    )
  ) %>%
  rename(age = age_adm)


lab_ev71_p <- viro2 %>%
  group_by(peak) %>%
  group_modify(~.x %>% age_profile(age_values = seq(0,6,le = 512)))  %>%
  ggplot(aes(x = age,y = fit))+
  geom_line()+
  geom_point(data = viro2,aes(x = age,y=pos),shape = "|")+
  geom_ribbon(aes(ymin = lwr,ymax = upr),fill = "blue",alpha = 0.3)+
  scale_x_continuous(breaks = seq(0,6,by = 1))+
  scale_y_continuous(name = "Percentage (%)",
                     labels = scales::label_percent(scale = 100),
                     position = "right")+
  labs(x = "Age (years)")+
  facet_wrap(~peak)+
  theme_bw()+
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        legend.position = "none",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 18))

# lab_ev71_p <- (plot_spacer()|lab_ev71_p)+
#   plot_layout(widths = c(1,1))

width_ratio <- c(2.5, 2)

row01 <- (lab_ev71_p|plot_spacer())+
  plot_layout(widths = width_ratio)

row0 <- (exp_in_ch1 | plot_spacer())+
  plot_layout(widths = width_ratio)


row1 <- (age_profile_sp_cm | lab_ev71_p)+
  plot_layout(widths = width_ratio)


row2 <-  ((ts/hm2) | (plot_spacer()/
                        wrap_elements(full = ad  +
                                        theme(plot.margin = margin(-15,150, 7, 0)))))+
  plot_layout(widths = width_ratio)

row3 <- (birth_23 | plot_spacer())+
  plot_layout(widths = width_ratio)

(row0 /row1 /row2 / row3) +
  plot_layout(heights = c(1,1,3,1))

ggsave("./fig_manuscript/fig2_3t.svg",
       width = 15,height = 15,dpi = 500)
