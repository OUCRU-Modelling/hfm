pavit<- function(pos=pos,tot=rep(1,length(pos)))
{
  gi<- pos/tot
  pai1 <- pai2 <- gi
  N <- length(pai1)
  ni<-tot
  for(i in 1:(N - 1)) {
    if(pai2[i] > pai2[i + 1]) {
      pool <- (ni[i]*pai1[i] + ni[i+1]*pai1[i + 1])/(ni[i]+ni[i+1])
      pai2[i:(i + 1)] <- pool
      k <- i + 1
      for(j in (k - 1):1) {
        if(pai2[j] > pai2[k]) {
          pool.2 <- sum(ni[j:k]*pai1[j:k])/(sum(ni[j:k]))
          pai2[j:k] <- pool.2
        }
      }
    }
  }
  return(list(pai1=pai1,pai2=pai2))
}

library(mgcv)
library(tidyverse)
gamfit2=gam(pos~s(age)+s(col_date),
           family=binomial,
           mutate(atdf, across(col_date, as.numeric)))

agrid=seq(range(unique(a))[1],range(unique(a))[2],length.out=100)
newd <- data.frame(a=agrid,x=xind,g=0)
pavit(predict.gam(gamfit,newdata=newd,type="response"))

aggre_df <- atdf %>% mutate(age2 = round(age),
                month = rep(c(1,4,8,12),each = 75)) %>%
  group_by(age2,month) %>% count(pos) %>%
  pivot_wider(names_from = pos,values_from = n) %>%
  rename(neg = `0`, pos = `1`) %>%
  replace(is.na(.), 0) %>% mutate(total = neg+pos) %>%
  select(pos,total,age2,month)

y=cbind(aggre_df$pos,aggre_df$total)
a <- aggre_df$age2
x <- aggre_df$month

gamfit=gam(y~s(a)+x,family=binomial(link="logit"))

agrid=seq(range(unique(a))[1],range(unique(a))[2],length.out=100)

xind <- sort(unique(x))[4]
newd <- data.frame(a=agrid,x=xind)

ypred <- pavit(predict.gam(gamfit,newdata=newd,type="response"))

agridfoi=foi.num(agrid,ypred$pai2)$grid
ypredfoi=foi.num(agrid,ypred$pai2)$foi

ggplot()+
  geom_line(aes(x = agrid,y = ypred$pai2))+
  geom_line(aes(x = agridfoi,y = ypredfoi))+
  ylim(0,1)

### GLM with PAVA algorithm
pavit <- function(pos=pos,tot=rep(1,length(pos))){
  gi<- pos/tot
  pai1 <- pai2 <- gi
  N <- length(pai1)
  ni<-tot
  for(i in 1:(N - 1)) {
    if(pai2[i] > pai2[i + 1]) {
      pool <- (ni[i]*pai1[i] + ni[i+1]*pai1[i + 1])/(ni[i]+ni[i+1])
      pai2[i:(i + 1)] <- pool
      k <- i + 1
      for(j in (k - 1):1) {
        if(pai2[j] > pai2[k]) {
          pool.2 <- sum(ni[j:k]*pai1[j:k])/(sum(ni[j:k]))
          pai2[j:k] <- pool.2
        }
      }
    }
  }
  return(list(pai1=pai1,pai2=pai2))
}

library(plotly)
mod1 <- glm(pos ~ age * col_date +
              I(age ^ 2) * col_date +
              I(age ^ 3) * col_date +
              I(age ^ 4) * col_date, binomial,
            mutate(atdf, across(col_date, as.numeric)))

dataset <- mod1$data
age_val <- c(.1,1:15)
collection_date_val <- seq(min(dataset$col_date),
                           max(dataset$col_date),le=512)

new_data <- expand.grid(age = age_val, col_date = collection_date_val)

prdcts <- cbind(new_data,predict(mod1, new_data,type = "link",se.fit = TRUE) |> as.data.frame()) %>%
  select(-c("residual.scale")) %>%
  mutate(fit2 = mod1$family$linkinv(fit),
         lwr = mod1$family$linkinv(fit - 1.96*se.fit),
         upr = mod1$family$linkinv(fit + 1.96*se.fit))|>
  arrange(col_date) |>
  mutate(across(col_date, as_date))

prdcts <- cbind(new_data, fit = 100 * predict(mod1, new_data, "response",se.fit = TRUE)$fit) |>
  as_tibble() |>
  arrange(col_date) |>
  mutate(across(col_date, as_date))




# unconstrain
plot_ly(prdcts, x = ~sort(unique(col_date)),
        y = ~sort(unique(age)),
        z = ~matrix(fit2*100, 16),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))

# apply pava algorithm

df_c <- data.frame()
for (i in 1:length(age_val)){
  pred <- prdcts %>% filter(age == age_val[i])
  pred$fitc <- pavit(as.numeric(pred$fit2))$pai2*100
  pred$l_fitc <- pavit(as.numeric(pred$lwr))$pai2*100
  pred$u_fitc <- pavit(as.numeric(pred$upr))$pai2*100
  df_c <- rbind(df_c,pred)
}

df_c <- df_c[order(as.Date(df_c$col_date, format="%m/%d/%Y")),]

plot_ly(df_c, x = ~sort(unique(col_date)),
        y = ~sort(unique(age)),
        z = ~matrix(fitc, 16),
        showscale = F) %>%
  add_surface()%>%
  layout(scene = list(
    xaxis = list(title = "Collection date"),
    yaxis = list(title = "Age"),
    zaxis = list(title = "Seroprevalence",range = c(0,100))
  ))


smooth_plot <- function(data,month,year){

  clo <- case_when(year == 2022 & month == 12 ~ "Dec 2022",
                   year == 2023 & month == 4 ~ "Apr 2023",
                   year == 2023 & month == 8 ~ "Aug 2023",
                   year == 2023 & month == 12 ~ "Dec 2023")

  clo2 <- case_when(year == 2022 & month == 12 ~ "#0808cf",
                    year == 2023 & month == 4 ~ "#ed097b",
                    year == 2023 & month == 8 ~ "#ed6b00",
                    year == 2023 & month == 12 ~ "#33516b")

  dtaa <- case_when(year == 2022 & month == 12 ~ t1222,
                    year == 2023 & month == 4 ~ t423,
                    year == 2023 & month == 8 ~ t823,
                    year == 2023 & month == 12 ~ t1223)

  ccc <- data %>% filter(year(col_date) == year &
                           month(col_date) == month) %>%
          select(c("age","fitc","l_fitc","u_fitc"))

  ss <- ccc %>% mutate(age2 = ksmooth(ccc$age ,ccc$fitc ,kernel="normal",bandwidth=2)$x,
                 fit2c = ksmooth(ccc$age ,ccc$fitc ,kernel="normal",bandwidth=2)$y,
                 uprc = ksmooth(ccc$age ,ccc$u_fitc ,kernel="normal",bandwidth=2)$y,
                 lwrc = ksmooth(ccc$age ,ccc$l_fitc ,kernel="normal",bandwidth=2)$y)

  ss %>%
    ggplot(aes(x = age2,y = fit2c))+
    geom_line(aes(col = clo))+
    geom_ribbon(aes(x = age2,y = fit2c,
                    ymin=lwrc, ymax=uprc),alpha = 0.5,fill = clo2)+
    ylim(0,101)+
    theme_minimal() +
    scale_color_manual(name = "Y series",
                       values = c(clo = clo2))+
    labs(y = "Seroprevalence (%)",x = "Age")+
    geom_point(data= dtaa, aes(x = age, m * pos + 1),
               shape = "|",
               col = clo2)+
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.position = "hide",
      legend.text = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15))+
    annotate("text", x = 3, y = 90, label = c(clo),size = 6)
}

p1222 <- smooth_plot(data = df_c,month = 12,year = 2022)
p423 <- smooth_plot(data = df_c,month = 4,year = 2023)
p823 <- smooth_plot(data = df_c,month = 8,year = 2023)
p1223 <- smooth_plot(data = df_c,month = 12,year = 2023)

p1222|p423|p823|p1223


