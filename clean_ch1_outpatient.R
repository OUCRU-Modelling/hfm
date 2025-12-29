library(readxl)
CH1_outpatient_010623 <- read_excel("D:/OUCRU/hfmd/data/CH1_outpatient.xlsx",
                             sheet = "0106 2023")
CH1_outpatient_071223 <- read_excel("D:/OUCRU/hfmd/data/CH1_outpatient.xlsx",
                             sheet = "0712 2023")

## px_old_new

px_qh_change <- read_excel("D:/OUCRU/hfmd/data/AP Transform Loc.xlsx",
                               sheet = "Mapping") %>%
  filter(tinhThanh_cu == "TP.Hồ Chí Minh") %>%
  mutate(
    district = quanHuyen_cu %>%
      stri_trans_general("latin-ascii") %>%
      tolower() %>% trimws(which = "both"),
    commune = tenXa_moi %>%
      stri_trans_general("latin-ascii") %>%
      tolower() %>% trimws(which = "both")
  )

new_dup_dis <- px_qh_change %>%
  group_by(commune) %>%
  summarise(n_district = n_distinct(district),
            districts = paste(unique(district), collapse = ", ")) %>%
  filter(n_district > 1) %>%
  pull(commune)


px_qh_pair <- px_qh_change %>%
  filter(!commune %in% new_dup_dis) %>%
  select(district,commune) %>% unique()


##

CH1_outpatient_hcm_23 <- rbind(CH1_outpatient_010623,CH1_outpatient_071223) %>%
  mutate(dob = as.Date(ngaysinh),
         adm = as.Date(ngaykham)) %>%
  mutate(city = str_split_i(string = diaphuong,
                          pattern = ",",i=-1),
         district = str_split_i(string = diaphuong,
                                pattern = ",",i=-2) %>% trimws(which = "both"),
         district2 = district %>%
           stri_trans_general("latin-ascii") %>%
           tolower() %>% trimws(which = "both"),
         commune = str_split_i(string = diaphuong,
                               pattern = ",",i=-3) %>% trimws(which = "both"),
         commune2 = commune %>%
           stri_trans_general("latin-ascii") %>%
           tolower() %>% trimws(which = "both"),) %>%
  filter(city %in% c(" TP.Hồ Chí Minh"," Thành phố Hồ Chí Minh")) %>%
  distinct(mahoso,dob,PHAI, .keep_all = TRUE)


CH1_outpatient_hcm_23a <- CH1_outpatient_hcm_23 %>%
  filter(district != "Không xác định" &
           !commune2 %in% new_dup_dis &
           district == "") %>%
  mutate(commune2 = commune %>%
           stri_trans_general("latin-ascii") %>%
           tolower() %>% trimws(which = "both")) %>%
  left_join(.,px_qh_pair,by = join_by(commune2 == commune)) %>%
  na.omit(district.y) %>%
  select(dob,adm,district.y,commune2) %>%
  set_colnames(c("dob","adm","district","commune"))


CH1_outpatient_hcm_23b <- CH1_outpatient_hcm_23 %>%
  filter(!district %in% c("Không xác định","")) %>%
  select(dob,adm,district2,commune2) %>%
  set_colnames(c("dob","adm","district","commune"))

CH1_out_cleaned <- rbind(CH1_outpatient_hcm_23a,CH1_outpatient_hcm_23b) %>%
  mutate(
    district = district %>%
      str_remove_all("quan|huyen|thanh pho") %>%
      trimws(which = "both"),
    district = case_when(
      district %in% c(2,9) ~ "thu duc",
      !district %in% c(2,9) ~ district),
    commune = commune %>%
      str_remove_all("phuong|xa") %>%
      trimws(which = "both"),
    age = interval(dob,adm) / years(1),
  ) |>
  filter(age < 17 & age >= 0) %>%
  mutate(
    age_gr = cut(age,
                 breaks = seq(0,17),
                 right = FALSE,
                 labels = seq(0,16)) %>% as.character() %>% as.numeric(),
    period = case_when(
      adm <= as.Date("2023-04-30") ~ "12/2022 - 4/2023",
      adm > as.Date("2023-04-30") & adm <= as.Date("2023-08-31") ~ "4/2023 - 8/2023",
      adm > as.Date("2023-08-31") ~ "8/2023 - 12/2023"
    )
  ) %>%
  group_by(district,age_gr,period) %>%
  count() %>%
  ungroup()

saveRDS(CH1_out_cleaned,"D:/OUCRU/hfmd/data/CH1_out_cleaned.rds")

