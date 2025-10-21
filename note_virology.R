library(readxl)
viro_df <- read_excel("D:/OUCRU/hfmd/data/03EI Data 2023 shared.xlsx")

library(lazymod)
devtools::install_github("OUCRU-Modelling/lazymod")

viro_df %>% group_by(SeroGroup1) %>% count()

viro_df %>% pull(DateAdmission) %>% range()

viro_df %>%
  mutate(admission_date = as.Date(DateAdmission),
         adm_month = month(admission_date),
         age_adm = interval(DateBirth, admission_date) / years(1)) %>%
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
