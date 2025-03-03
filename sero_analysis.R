library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(magrittr)
library(mgcv)
library(tidyverse)
library(patchwork)
library(sf)
library(janitor)
library(ggsci)
library(cowplot)
invisible(Sys.setlocale("LC_TIME", "English"))

apr_2023 <- read_excel("D:/OUCRU/hfmd/data/4_2023.xlsx")
aug_2023 <- read_excel("D:/OUCRU/hfmd/data/08_2023.xlsx")
dec_2022 <- read_excel("D:/OUCRU/hfmd/data/12_2022.xls")
dec_2023 <- read_excel("D:/OUCRU/hfmd/data/12_2023.xlsx")

t423 <- data.frame(apr_2023[-c(1,2),c(6,8,10:14)])
t423$pos <- replace(t423$...14,is.na(t423$...14),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t423) <- c("id","age_gr","age","col_day","col_month","col_year","neutralization","pos")
t423$age <- as.numeric(t423$age)
t423$col_time <- rep("Apr 2023",nrow(t423))


t823 <- data.frame(aug_2023[-c(1,2),c(6,8,9,14:17)])
t823$pos <- str_detect(t823$...17,regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t823) <- c("id","age_gr","age","col_day","col_month","col_year","neutralization","pos")
t823$age <- as.numeric(t823$age)
t823$col_time <- rep("Aug 2023",nrow(t823))

t1222 <- data.frame(dec_2022[-c(1,2),c(6,8,10:14)])
t1222$pos <- replace(t1222$...14,is.na(t1222$...14),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t1222) <- c("id","age_gr","age","col_day","col_month","col_year","neutralization","pos")
t1222$age <- as.numeric(t1222$age)
t1222$col_time <- rep("Dec 2022",nrow(t1222))


t1223 <- data.frame(dec_2023[-c(1,2),c(6,8,9,14:17)])
t1223$pos <- replace(t1223$...17,is.na(t1223$...17),0) %>%
  str_detect(regex(paste(2^(4:10), collapse = "|"))) %>%
  as.integer(as.logical())
colnames(t1223) <- c("id","age_gr","age","col_day","col_month","col_year","neutralization","pos")
t1223$age <- as.numeric(t1223$age)
t1223$col_time <- rep("Dec 2023",nrow(t1223))

####
cleaned <- read_csv("D:/OUCRU/HCDC/project phân tích sero quận huyện/cleaned.csv")

sero <- rbind(t1222,t1223,t423,t823)

sero_add <- full_join(cleaned,sero, by =  c("id" = "id"))

data_pt <- sero_add %>% filter(!is.na(age)&!is.na(qhchuan)) %>%
  select(-c(add_mod,pxchuan,neutralization,id)) %>%
  as.data.frame()

data_pt$age_gr2 <- cut(data_pt$age+0.00000001, breaks = seq(0, 15, by = 3),
                       labels = c(" <0 & ≤3 yo",
                                  "<3 & ≤6 yo",
                                  "<6 & ≤9 yo",
                                  "<9 & ≤12 yo",
                                  "<12 & ≤15 yo"))

## figure 1

data_pt %>%
  # filter(qhchuan == data_pt$qhchuan[1]) %>%
  ggplot() +
  geom_bar(aes(x = col_time))+
  scale_x_discrete(limits = c("Dec 2022",
                              "Apr 2023",
                              "Aug 2023",
                              "Dec 2023"))+
  scale_y_continuous(breaks = seq(0,14,by=4),
                     limits = c(0,14))+
  facet_wrap(~qhchuan,
             ncol = 5)+
  labs(x = "Collection date",y = "Number of samples",
       title = "Number of samples per district")+
  theme_bw()

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sample_per_dis.svg",
       width = 15,height = 8,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sample_per_dis.png",dpi = 500,
       width = 15,height = 8,bg = "white")


## figure 2

data_pt %>%
  ggplot() +
  geom_bar(aes(x = age_gr2,fill = factor(col_time,levels = c("Dec 2022",
                                                             "Apr 2023",
                                                             "Aug 2023",
                                                             "Dec 2023"))))+
  scale_y_continuous(breaks = seq(0,14,by=4),
                     limits = c(0,14))+
  scale_fill_discrete(limits = c("Dec 2022",
                              "Apr 2023",
                              "Aug 2023",
                              "Dec 2023"))+
  facet_wrap(~qhchuan,
             ncol = 5)+
  labs(x = "Age group",y = "Number of samples",
       fill ="Collection date",
       title = "Number of samples for each age group per district")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5,angle = 45))

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sample_per_agegr.svg",
       width = 15,height = 8,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sample_per_agegr.png",dpi = 500,
       width = 15,height = 8,bg = "white")

## seroprevalence same age group over time

data_pt$col_date <- make_date(data_pt$col_year,data_pt$col_month,data_pt$col_day)

# atdf <- data.frame()
# atdf2 <- data.frame()
# for (i in 1:11)
#   i = 1
#   atdf <- data_pt %>% filter(age_gr == unique(data_pt$age_gr)[i])
#
#   mod1 <- glm(pos ~ col_date + I(col_date ^ 2) + I(col_date ^ 3) ,
#                   binomial,data =  mutate(atdf, across(col_date, as.numeric)))
#
#   # atdf$fit <- gam(pos ~ s(col_date,bs = "bs"),method = "REML",
#   #                 family = "binomial",
#   #                 data =  mutate(atdf, across(col_date, as.numeric)))$fitted
#   atdf2 <- rbind(atdf2,atdf)

binomial_smooth <- function(...) {
    geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  }

unique(data_pt$age_gr)

data_pt$age_gr <- factor(data_pt$age_gr,
                         levels = c(unique(data_pt$age_gr)))

## figure 3
ggplot(data_pt,
       aes(x = col_date, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~age_gr,
             ncol = 5)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence",
       title = "Seroprevalence by time in each age group")+
  theme_bw()

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_overtime.svg",
       width = 15,height = 8,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_overtime.png",dpi = 500,
       width = 15,height = 8,bg = "white")

ggplot(data_pt,
       aes(x = col_date, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~age_gr2,
             ncol = 5)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence",
       title = "Seroprevalence by time in each age group")+
  theme_bw()

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_overtime2.svg",
       width = 15,height = 6,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_overtime2.png",dpi = 500,
       width = 15,height = 6,bg = "white")

### fit model for sp over district

ggplot(data_pt,
       aes(x = col_date, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 5)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()


ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_per_dis.svg",
       width = 15,height = 8,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/sp_per_dis.png",dpi = 500,
       width = 15,height = 8,bg = "white")

ggplot(data_pt,
       aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 5)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence",
       title = "Seroprevalence by age of each district")+
  theme_bw()

ggsave("D:/OUCRU/hfmd/figure/EV71 present/age_sp_per_dis.svg",
       width = 15,height = 8,bg = "white")

ggsave("D:/OUCRU/hfmd/figure/EV71 present/age_sp_per_dis.png",dpi = 500,
       width = 15,height = 8,bg = "white")


data_pt %>% group_by(qhchuan,col_time) %>% count()

select_dis <- c("1","4","7","binh thanh","go vap","nha be","phu nhuan")

data_sle <- data_pt %>% filter(!qhchuan %in% select_dis)

age_str_1222 <-
  data_sle %>% filter(col_time == "Dec 2022") %>%
  ggplot(aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 1)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()

age_str_423 <-
  data_sle %>% filter(col_time == "Apr 2023") %>%
  ggplot(aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 1)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()

age_str_823 <-
  data_sle %>% filter(col_time == "Aug 2023") %>%
  ggplot(aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 1)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()

age_str_1223 <-
  data_sle %>% filter(col_time == "Dec 2023") %>%
  ggplot(aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(~qhchuan,
             ncol = 1)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()

age_str_1222|age_str_423 |age_str_823 |age_str_1223


data_sle  %>%
  ggplot(aes(x = age, y = pos)) +
  geom_jitter(height = 0.05)+
  facet_wrap(vars(qhchuan, col_time),ncol=4)+
  binomial_smooth(formula = y ~ splines::ns(x, 2))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  theme_bw()

