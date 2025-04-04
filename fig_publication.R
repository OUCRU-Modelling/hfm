## data input
library(readxl)
library(tidyverse)
library(gtsummary)
library(patchwork)
library(lubridate)
library(stringr)
library(mgcv)
library(janitor)
invisible(Sys.setlocale("LC_TIME", "English"))

## case notification
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

## serological data

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

## census data

census2019 <- readRDS("D:/OUCRU/hfmd/data/census2019.rds")


## sliding windown function

slide_age <- function(time1,age1,w1,s1){
  df1 <- data.frame(time1,age1) %>%                   ## line listing data frame
    filter(!is.na(time1) & !is.na(age1)) %>%
    arrange(time1)

  a_df1 <- df1 %>% count(time1)                      ## aggregate data frame

  total1 <- nrow(a_df1)
  spots1 <- seq(from = 1, to = (total1 - w1 + 1), by = s1)

  out_total <- data.frame()

  for (i in 1:length(spots1)){
    range1 <- data.frame(a_df1[spots1[i]:(spots1[i] + w1 - 1),1])
    result1 <- df1$age1[df1$time1 >= range1[1,] & df1$time1 <= range1[w1,]]
    out <- data.frame(date = rep(as.character(range1[ceiling(w1/2),]),length(result1)),
                      age = result1)
    out <- out[order(out$age),]
    out_total <- rbind(out_total,out)
  }
  output <- list()
  output$wdat <- out_total
  output$adat <- out_total %>% count(date)
  output$parms <- data.frame(w = w1,s =s1)
  return(output)
}


case_noti <- function(timee, agee,lab = FALSE){

  dat <- data.frame(timee,agee) %>%
    filter(!is.na(timee) & !is.na(agee)) %>%
    count(timee)

  ts <- ggplot(dat, aes(x =timee, y = n)) +
    geom_bar(stat = "identity") +
    labs(y = "Cases") +
    theme_minimal()

  if(lab == TRUE){
    ts
  } else {
    ts + theme(axis.title.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank())
  }

}


## fig 1 (heat map)

df23 <- df1 %>% filter(year(adm_week) == 2023)

co <- data.frame()

for (i in 0:6){
  gen <- seq(0,1,le=52) + i
  co <- rbind(co,gen)
}


df23 <- df1 %>% filter(year(adm_date) == 2023)
wwww <- slide_age(time1 = df23$adm_date,
                  age1 =  df23$age1,
                  w1 = 7, s1=7)

ch <- data.frame(date = wwww$adat$date,
                 c0 = as.numeric(co[1,]),
                 c1 = as.numeric(co[2,]),
                 c2 = as.numeric(co[3,]),
                 c3 = as.numeric(co[4,]),
                 c4 = as.numeric(co[5,]),
                 c5 = as.numeric(co[6,]))

leb_month <- c("Jan",rep("",3),"Feb",rep("",3),"Mar",rep("",4),"Apr",rep("",3),
               "May",rep("",4),"Jun",rep("",3),"Jul",rep("",3),"Aug",rep("",4),
               "Sep",rep("",3),"Oct",rep("",3),"Nov",rep("",4),"Dec",rep("",3))

ts <- case_noti(wwww$wdat$date,wwww$wdat$age)
hm <- ggplot(data=wwww$wdat, aes(x=date, y=age)) +
  stat_density(
    aes(fill = after_stat(density)),
    geom = "raster",
    position = "identity"
  )+
  scale_fill_gradient(low="yellow", high="red")+
  theme_minimal()+
  scale_y_reverse(name = "Age",lim= rev(c(0,6)),breaks = seq(0,6))+
  scale_x_discrete(name = "Admission week",labels = leb_month)+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 8))+
  geom_line(data = ch,aes(x = date,y = c0),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)+
  geom_line(data = ch,aes(x = date,y = c1),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)+
  geom_line(data = ch,aes(x = date,y = c2),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)+
  geom_line(data = ch,aes(x = date,y = c3),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)+
  geom_line(data = ch,aes(x = date,y = c4),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)+
  geom_line(data = ch,aes(x = date,y = c5),col = "white",
            group = 1,lwd = 0.25,alpha = 0.5)



## density plot

fi_peak <- df1 %>% filter(year(adm_date) == "2023") %>%
  filter((adm_date <= as.Date("2023-09-03")&
            !is.na(adm_date) & !is.na(age1)))

se_peak <- df1 %>% filter(year(adm_date) == "2023") %>%
  filter((adm_date > as.Date("2023-09-03")) &
           !is.na(adm_date) & !is.na(age1))

data <- data.frame(
  peak = c( rep("1st peak",nrow(data.frame(se_peak$age1))),
            rep("2nd peak",nrow(data.frame(fi_peak$age1)))),
  age = c( fi_peak$age1, se_peak$age1 )
)

ad <- ggplot(data=data, aes(x=age, group=peak, fill=peak)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_fill_manual( values = c("red","blue")) +
  scale_x_reverse(limit = c(6,0),breaks = seq(0,6,by=1))+
  coord_flip()+
  theme_minimal()+
  labs(x = "Age", y ="Density")+
  theme(axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "inside",
    legend.position.inside =  c(.80, .20))




((ts/hm)|(plot_spacer()/ad))+
  plot_layout(widths = c(2.5,1))


ggsave("./fig_manuscript/fig1.svg",
       width = 14,height = 7,dpi = 500)

tb <- data %>%
  tbl_summary(by = peak,
              digits = list(all_continuous() ~ 3)) %>%
  add_p()


## fig2



df23$agr <- cut(df23$age1, breaks = c(0, 1, 2, 3, 4,5,6,Inf), right = F)
df23$agr <- factor(df23$agr, labels = c("[0-1)", "[1-2)", "[2-3)",
                                        "[3-4)","[4-5)","[5-6)","6+"))

df23$age_gr2 <- cut(df23$age1+0.00000001, breaks = c(seq(0, 15, by = 3),82),
                       labels = c("<0 & ≤3 years",
                                  "<3 & ≤6 years",
                                  "<6 & ≤9 years",
                                  "<9 & ≤12 years",
                                  "<12 & ≤15 years",
                                  "16+"))

## population data to calculate attack rate


hcm19 <- census2019 %>% filter(province == "Thành phố Hồ Chí Minh")

hcm19$district <- hcm19$district %>%
  str_remove_all("Quận|Huyện") %>%
  str_replace_all(
    c("\\b2\\b|\\b9\\b"  = "Thủ Đức")) %>%
  trimws(which = "both")




hcm19$age2 <- word(hcm19$age,end = 1) |> as.numeric()

hcm19$age3 <- cut(hcm19$age2,breaks = c(seq(0, 15, by = 3),82),
                     labels = c("<0 & ≤3 years",
                                "<3 & ≤6 years",
                                "<6 & ≤9 years",
                                "<9 & ≤12 years",
                                "<12 & ≤15 years",
                                "16+"))

pop_agegr <- hcm19 %>%
  group_by(age3) %>%
  summarise(n = sum(n))

atk_plot_df <- df23 %>%
  select(adm_week,age_gr2)%>%
  count(age_gr2,adm_week) %>%
  left_join(pop_agegr,by = join_by(age_gr2 == age3)) %>%
  group_by(age_gr2) %>%
  mutate(pop = n.y - cumsum(lag(n.x, default = 0)),
         atr = n.x/pop)  %>%
  select(-n.y)


atk_rate <- atk_plot_df %>% na.omit() %>% filter(age_gr2 != "16+") %>%
  ggplot(aes(x = adm_week,y=log(atr*100000))) +
  # geom_bar(stat = "identity")+
  geom_line()+
  facet_wrap(vars(age_gr2),ncol = 5)+
  # scale_x_date(limits = as.Date(c("2023-01-01","2023-12-31")))+
  coord_cartesian(xlim = as.Date(c("2023-01-01","2023-12-31")))+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y")+
  theme_bw()+
  labs(x = "Admission week",y= "Log attack rate")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## sero_prevalence by age group

sero <- rbind(t1222,t1223,t423,t823)

sero$age_gr2 <- cut(sero$age+0.00000001, breaks = seq(0, 15, by = 3),
                       labels = c("<0 & ≤3 years",
                                  "<3 & ≤6 years",
                                  "<6 & ≤9 years",
                                  "<9 & ≤12 years",
                                  "<12 & ≤15 years"))

sero$col_date <- make_date(sero$col_year,sero$col_month,sero$col_day)

sero$age_gr2 <- factor(sero$age_gr2,
                         levels = c("<0 & ≤3 years",
                                    "<3 & ≤6 years",
                                    "<6 & ≤9 years",
                                    "<9 & ≤12 years",
                                    "<12 & ≤15 years"))


sp_agr <- ggplot(sero,
       aes(x = col_date, y = pos)) +
  # geom_jitter(height = 0.05)+
  geom_point(aes(x = col_date, pos),
             shape = "|")+
  facet_wrap(~age_gr2,
             ncol = 5)+
  geom_smooth(fill = "blue",alpha = 1/10,
              method = mgcv::gam,formula = y ~ s(x, bs = "bs"))+
  labs(x = "Collection date", y  = "Seroprevalence")+
  # scale_x_date(breaks = seq(as.Date("2023-01-01"),as.Date("2023-12-31")),)+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %Y")+
  coord_cartesian(ylim = c(0, 1),xlim = as.Date(c("2023-01-01","2023-12-31")))+
  theme_bw()+
  # theme(axis.text.x = element_text(vjust = 0.5,angle = 45))
  {}

atk_rate/
  sp_agr

ggsave("./fig_manuscript/fig2.svg",
       width = 14,height = 7,dpi = 500)


# fit gam model by hand to compare with geom_smooth

predict_ci <- function(model, data, var, ci = 0.95, length = 1000) {
  p <- (1 - ci) / 2
  link_inv <- model$family$linkinv
  n <- nrow(data) - length(model$coefficients)
  vals <- seq(from = min(data[[var]]), to = max(data[[var]]), length.out = length)
  df <- data.frame(col = vals)
  colnames(df) <- var

  pred <- data.frame(predict(model, df, se.fit = TRUE))
  pred[[var]] <- df[[var]]
  pred$lwr <- link_inv(pred$fit + qt(p, n) * pred$se.fit)
  pred$upr <- link_inv(pred$fit + qt(1 - p, n) * pred$se.fit)
  pred$fit <- link_inv(pred$fit)
  pred
}

data_fit <- sero %>% filter(age_gr2 == "<12 & ≤15 years")

gam_model <- gam(pos ~ s(as.numeric(col_date)),
                 family = binomial,
                      data = data_fit,
                 method = "REML")

pred1 <- predict_ci(model = gam_model, data = data_fit, var = "col_date")

pred1 %>%
  ggplot(aes(x = col_date,y = fit))+
  geom_line()+
  geom_ribbon(
    mapping = aes(
      ymin = lwr,
      ymax = upr
    ),
    data = pred1,
    alpha = 0.15
  )+
  ylim(c(0,1))


## calibration plot

## 15 age group and 15 attack rate base on that age group
## cut and calculate sp for each age group from 0-15 and fit geom_smooth


correct_lv <- c("0≤ & <1","1≤ & <2","2≤ & <3","3≤ & <4","4≤ & <5",
                "5≤ & <6","6≤ & <7","7≤ & <8","8≤ & <9","9≤ & <10",
                "10≤ & <11","11≤ & <12","12≤ & <13","13≤ & <14","14≤ & <15")

cali_sero <- sero %>% group_by(age_gr) %>%
            count(pos) %>%
            pivot_wider(names_from = pos,values_from = n) %>%
            clean_names() %>%
            mutate(sp = x1/sum(x0,x1)) %>%
            select(-c(x0,x1)) %>%
            ungroup()

cali_sero$age_gr <- factor(cali_sero$age_gr,levels = correct_lv)

## case noti
df23$age_cali <- cut(df23$age1+0.00000001, breaks = c(seq(0, 15, by = 1),82),
                    labels = c(correct_lv,
                               "16+"))

## population
hcm19$age_cali <- cut(hcm19$age2,breaks = c(seq(0, 15, by = 1),82),
                      labels = c(correct_lv,
                                 "16+"))

pop_agegr_cali <- hcm19 %>%
                  group_by(age_cali) %>%
                  summarise(n = sum(n))

cali_plot_df <- df23 %>%
                select(age_cali) %>%
                group_by(age_cali) %>%
                count() %>% na.omit() %>%
                left_join(pop_agegr_cali,by = join_by(age_cali == age_cali)) %>%
                # group_by(age_gr2) %>%
                mutate(atr = n.x/n.y) %>%
                filter(age_cali != "16+")  %>%
                left_join(cali_sero,by = join_by(age_cali == age_gr))

library(predtools)


# model_clb <- glm(atr*100000 ~ sp,data = cali_plot_df)
#
# model_clb$pred <- predict.glm(model_clb, type = 'response')

# calibration_plot(data = data.frame(y = model_clb$y,pred = model_clb$pred),
#                  obs = "y", pred = "pred")

cali_plot_df %>% ungroup() %>% select(-c(2,3)) %>%
  pivot_longer(cols = -age_cali,names_to = "type",values_to = "value") %>%
  mutate(variable = ifelse(type == "atr", value*100000, value*100)) %>%
  ggplot() +
  geom_col(aes(x  = age_cali, y = variable),position = "dodge")+
  facet_wrap(~type,scales = "free",ncol = 1)+
  # scale_y_continuous(sec.axis = sec_axis(~ ./100, name = "NDVI"))
  {}



