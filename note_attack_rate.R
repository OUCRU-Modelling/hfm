pop <- readRDS("D:/OUCRU/hfmd/data/count_dangky.rds")
pop_a <- pop %>% group_by(birth_month, birth_year) %>%
  summarise(n=sum(n)) %>% arrange(birth_year)

colnames(pop_a) <- c("m","y","n")

pop_a$dob <- str_c(pop_a$y,pop_a$m,sep = "-") %>% ym()

time1 = df1$adm_date
age1 =  df1$age1
dob = pop_a$dob
n = pop_a$n

dft <- data.frame(time1,age1) %>%
  filter(!is.na(time1) & !is.na(age1)) %>%
  arrange(time1)


dft$agr=as.factor(cut(dft$age1, c(0,0.5,
                                  1,1.5,
                                  2,2.5,
                                  3,3.5,
                                  4,4.5,5,5.5,6,100), right=TRUE ))
levels (dft$agr) = c("0-0.5", "0.5-1", "1.0-1.5",
                     "1.5-2","2-2.5","2.5-3",
                     "3-3.5","3.5-4","4-4.5",
                     "4.5-5","5-5.5","5.5-6","6+")

sus_pop <- data.frame(dob = dob, n = n)

out_total <- data.frame()

hfmd23 <- df1 %>% filter(year(adm_week) == "2023") %>%
  filter(!is.na(adm_week) ) %>%
  count(adm_week)

hfmd23$week <- 1:length(hfmd23$adm_week)
hfmd23$week <- ifelse(hfmd23$week == 53,52,hfmd23$week)

hfmd23$n2 <- ifelse(hfmd23$week == 52, sum(hfmd23$n[hfmd23$week==52]), hfmd23$n)

hfmd23 <- hfmd23[-53,]

dateaa <- hfmd23$adm_week+3

sus_pop <- data.frame(dob = dob, n = n)

for (i in 1:52){
  sus_pop$age <- interval(sus_pop$dob, dateaa[i]) / years(1)

  sus_pop$agr=as.factor(cut(sus_pop$age, c(0,0.5,
                                           1,1.5,
                                           2,2.5,
                                           3,3.5,
                                           4,4.5,5,5.5,6,100), right=TRUE ))
  levels (sus_pop$agr) = c("0-0.5", "0.5-1", "1.0-1.5",
                           "1.5-2","2-2.5","2.5-3",
                           "3-3.5","3.5-4","4-4.5",
                           "4.5-5","5-5.5","5.5-6","6+")
  outcum <- sus_pop %>% group_by(agr) %>%
    summarise(n = sum(n)) %>%
    as.data.frame()
  outcum$date <- rep(dateaa[i],nrow(outcum))
  out_total <- rbind(out_total,outcum)
}

deno <- out_total %>%
  pivot_wider(names_from = agr, values_from = n) %>% as.data.frame()

casss <- wwww$wdat

casss$agr=as.factor(cut(casss$age, c(0,0.5,1,1.5,2,2.5,
                                     3,3.5,4,4.5,5,5.5,6,100), right=TRUE ))
levels (casss$agr) = c("0-0.5", "0.5-1", "1.0-1.5",
                       "1.5-2","2-2.5","2.5-3",
                       "3-3.5","3.5-4","4-4.5",
                       "4.5-5","5-5.5","5.5-6","6+")

casss <- casss %>% group_by(date,agr) %>%
  count() %>% pivot_wider(names_from = agr, values_from = n) %>% as.data.frame()


casss <- casss[,-ncol(casss)]
casss <- replace(casss,is.na(casss), 0)
casss <- casss[,c(1:10,13,11,14,12)]

atkr <- data.frame()
atkr <- rbind(atkr,as.numeric(casss[1,-1])/as.numeric(deno[1,-1]))
for (i in 1:51){
  new <- as.numeric(casss[i+1,-1])/(as.numeric(deno[i+1,-1]) - as.numeric(casss[i,-1]))
  atkr <- rbind(atkr,new)
}
atkr <- cbind(deno$date,atkr)

colnames(atkr) <- colnames(deno)
atkr <- replace(atkr,is.na(atkr), 0)



atk_plot <- atkr %>% pivot_longer(cols=colnames(atkr)[-1],
                                  names_to= 'agr',
                                  values_to='atk') %>% as.data.frame()

atk <- ggplot(atk_plot, aes(x=as.character(date), y=agr, fill = atk)) +
  geom_raster()+
  scale_fill_paletteer_c("grDevices::Inferno",
                         breaks = c(0.01, 0.02, 0.03, 0.04, 0.05))+
  scale_y_discrete(limits=rev)+
  scale_x_discrete(name = "Admission week",labels = leb_month)+
  theme_minimal()+
  labs(y = "Age group")+
  labs(tag = "B",fill = "Attack rate")+
  theme(axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20,label.position="bottom"))

ts/
  atk


## using census data
casss <- wwww$wdat

casss$agr=as.factor(cut(casss$age, c(0,1,2,3,4,5,6,100), right=TRUE ))
levels (casss$agr) = c("0-1", "1-2","2-3",
                       "3-4","4-5","5-6","6+")

casss <- casss %>% group_by(date,agr) %>%
  count() %>% pivot_wider(names_from = agr, values_from = n) %>% as.data.frame()

casss <- casss[,-9]
casss <- replace(casss,is.na(casss), 0)




hcm19$age4 <- as.factor(cut(hcm19$age2, c(0,1,2,3,4,5,6,100), right=TRUE ))
levels(hcm19$age4) = c("0-1", "1-2","2-3",
                       "3-4","4-5","5-6","6+")

pop_s <- hcm19 %>%
  group_by(age4) %>%
  summarise(n = sum(n))

pop_s <- pop_s %>% pivot_wider(names_from = age4, values_from = n) %>% as.data.frame()

atkr <- data.frame()

atkr <- rbind(atkr,as.numeric(casss[1,-1])/as.numeric(pop_s))
for (i in 1:51){
  new <- as.numeric(casss[i+1,-1])/(as.numeric(pop_s) - as.numeric(casss[i,-1]))
  atkr <- rbind(atkr,new)
}
atkr <- cbind(casss$date,atkr)

colnames(atkr) <- colnames(casss)
atkr <- replace(atkr,is.na(atkr), 0)



atk_plot <- atkr %>% pivot_longer(cols=colnames(atkr)[-1],
                                  names_to= 'agr',
                                  values_to='atk') %>% as.data.frame()

ch2 <- data.frame(date = atk_plot$date,
                  c0 = as.numeric(co[1,]),
                  c1 = as.numeric(co[2,]),
                  c2 = as.numeric(co[3,]),
                  c3 = as.numeric(co[4,]),
                  c4 = as.numeric(co[5,]),
                  c5 = as.numeric(co[6,]))

atk <- ggplot(atk_plot, aes(x=date, y=agr, fill = atk)) +
  geom_raster()+
  scale_fill_paletteer_c("grDevices::Inferno")+
  scale_y_discrete(limits=rev)+
  theme_minimal()+
  labs(y = "Age group")+
  theme(axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 18),
      legend.text = element_text(size=18),
      legend.title = element_text(size=18))

ts/
  atk
