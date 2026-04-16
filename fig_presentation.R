# first run fig_manu.qmd

## ch1 admission

ts_ch1 <- df_cases_ch1_23 %>%
  group_by(adm_week, wave) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n, fill = wave)) +
  geom_col(alpha = 0.6, color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    name = "Collection date",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, le = 4)) +
  scale_fill_manual(values = c("#582C83FF", "#FFC72CFF")) +
  theme_minimal() +
  labs(y = "Cases") +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_colourbar(barwidth = 20), color = "none")

df_cases_count_ch1_23 <- df_cases_ch1_23 |>
  group_by(age, time) |>
  count() |>
  ungroup()

fit_gau_bs_ch1 <- gam(
  n ~ s(age, bs = "bs") +
    s(time, bs = "bs") +
    ti(age, time, bs = c("bs", "bs")) ,
  data = df_cases_count_ch1_23,
  family = gaussian(link = "log"),
  method = "REML"
)

age_val <- seq(0, 6, le = 365)

collection_date_val <- seq(min(df23$adm_week), max(df23$adm_week), le = 365)

new_data2 <- expand.grid(age = age_val, time = as.numeric(collection_date_val))

new_data2$pred_gau_bs_ch1 <- predict(fit_gau_bs_ch1, newdata = new_data2, type = "response")

hm_ch1 <- ggplot(new_data2) +
  geom_raster(aes(x = as.Date(time), y = age, fill = pred_gau_bs_ch1), interpolate = TRUE) +
  geom_line(
    data = cohort_lines(),
    aes(
      x = date,
      y = age,
      group = cohort,
      color = trend
    ),
    linewidth = 0.3,
    alpha = 0.4
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_viridis_c(option = "turbo", n.breaks = 10) +
  theme_minimal() +
  labs(x = "Admission week",
       y = "Age (years)",
       fill = "Number of\nadmissions",
       tag = "D") +
  theme_minimal() +
  scale_y_reverse(name = "Age (years)",
                  lim = rev(c(-0.5, 6.5)),
                  breaks = seq(-1, 7)) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_colourbar(barheight = 7), color = "none")

ch1_age <- (ts_ch1 + labs(tag = "A")) /
  (hm_ch1 + labs(tag = "B"))


ggsave("./fig_am/ch1_age.svg",plot = ch1_age,
       width = 15,height = 10,dpi = 500)

### serology to estimate force of infection

ts_ch1_a <- df_cases_ch1_23 %>%
  group_by(adm_week, wave) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n, fill = wave)) +
  geom_col(alpha = 0.6, color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    name = "Collection date",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, le = 4)) +
  scale_fill_manual(values = c("#582C83FF", "#FFC72CFF")) +
  theme_minimal() +
  labs(y = "Cases") +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_colourbar(barwidth = 20), color = "none")

ch1_3times <- ggplot() +
  geom_col(
    data = dat %>%
      bind_rows(.id = "id") %>%
      filter(age1 > 0) |>
      group_by(id, age) |>
      count(),
    aes(x = age, y = n),
    color = "white",
    fill = "black",
    alpha = 0.2
  ) +
  facet_wrap(~ factor(
    id,
    labels = c("12/2022 - 4/2023", "4/2023 - 8/2023", "8/2023 - 12/2023")
  )) +
  labs(y = "Number of cases", x = "Age (years)") +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )

sero_ch1 <- (ch1_3times /
               age_profile_sp_cm /
               ts_ch1_a) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_ch1.svg",plot = sero_ch1,
       width = 20,height = 16,dpi = 500)

###

ts_ch1_b <- df_cases_ch1_23 %>%
  group_by(adm_week, wave) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n, fill = wave)) +
  geom_col(alpha = 0.6, color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    name = "Collection date",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, le = 4)) +
  scale_fill_manual(values = c("#582C83FF", "#FFC72CFF")) +
  theme_minimal() +
  annotate(
    "rect",
    fill = "grey",
    xmin = as.Date(c(
      "2023-01-01", "2023-04-05", "2023-08-02", "2023-12-06"
    )),
    xmax = as.Date(c(
      "2023-01-15", "2023-04-26", "2023-08-30", "2023-12-27"
    )),
    ymin = 0,
    ymax = Inf,
    alpha = .5
  ) +
  labs(y = "Cases") +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_colourbar(barwidth = 20), color = "none")

age_profile_sp_cm <- outttt |>
  bind_rows() |>
  mutate(
    col_date = as.Date(collection_time),
    col_lab = format(col_date, format = "%b %Y"),
    col_lab = factor(col_lab, levels = c(
      "Dec 2022", "Apr 2023", "Aug 2023", "Dec 2023"
    ))
  ) |>
  ggplot(aes(x = cohort, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              fill = "blue",
              alpha = .3) +
  geom_point(data = df_sero |>
               mutate(col_lab = factor(
                 col_time, levels = c("Dec 2022", "Apr 2023", "Aug 2023", "Dec 2023")
               )),
             aes(x = age, y = pos),
             shape = "|") +
  facet_wrap(~ col_lab, nrow = 1) +
  labs(y = "Seroprevalence (%)", x = "Age (years)") +
  scale_y_continuous(labels = scales::label_percent(scale = 100),
                     limits = c(0, 1)) +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 18),
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text.x = element_text(size = 18)
  )


exp_in_ch1 <- expected_incidence |>
  ggplot() +
  geom_line(aes(x = cohort, y = exp_inc),linewidth = 1) +
  geom_col(
    data = dat %>%
      bind_rows(.id = "id") %>%
      filter(age1 > 0) |>
      group_by(id, age) |>
      count(),
    aes(x = age, y = n),
    color = "white",
    fill = "black",
    alpha = 0.2
  ) +
  facet_wrap( ~ factor(
    id,
    labels = c("12/2022 - 4/2023", "4/2023 - 8/2023", "8/2023 - 12/2023")
  )) +
  labs(y = "Number of cases", x = "Age (years)") +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )

exp_in_ch1 <- expected_incidence |>
  ggplot() +
  geom_line(aes(x = cohort, y = exp_inc),linewidth = 1) +
  geom_col(
    data = dat %>%
      bind_rows(.id = "id") %>%
      filter(age1 > 0) |>
      group_by(id, age) |>
      count(),
    aes(x = age, y = n),
    color = "white",
    fill = "black",
    alpha = 0.2
  ) +
  facet_wrap( ~ factor(
    id,
    labels = c("12/2022 - 4/2023", "4/2023 - 8/2023", "8/2023 - 12/2023")
  )) +
  labs(y = "Number of cases", x = "Age (years)") +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )

sero_ch1_fn <- (exp_in_ch1 /
                 age_profile_sp_cm /
                 ts_ch1_b) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_ch1_fn.svg",plot = sero_ch1_fn,
       width = 20,height = 16,dpi = 500)

###

exp_in_ch1.1 <- expected_incidence |>
  mutate(
    exp_inc = case_when(id != 1 ~ NA,.default = exp_inc)
  ) |>
  ggplot() +
  geom_line(aes(x = cohort, y = exp_inc),linewidth = 1) +
  geom_col(
    data = dat %>%
      bind_rows(.id = "id") %>%
      filter(age1 > 0) |>
      group_by(id, age) |>
      count(),
    aes(x = age, y = n),
    color = "white",
    fill = "black",
    alpha = 0.2
  ) +
  facet_wrap( ~ factor(
    id,
    labels = c("12/2022 - 4/2023", "4/2023 - 8/2023", "8/2023 - 12/2023")
  )) +
  labs(y = "Number of cases", x = "Age (years)") +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )

sero_ch1_c <- (exp_in_ch1.1 /
                 age_profile_sp_cm /
                 ts_ch1_b) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_ch1_c.svg",plot = sero_ch1_c,
       width = 20,height = 16,dpi = 500)

##

exp_in_ch1.2 <- expected_incidence |>
  mutate(
    exp_inc = case_when(id == 3 ~ NA,.default = exp_inc)
  ) |>
  ggplot() +
  geom_line(aes(x = cohort, y = exp_inc),linewidth = 1) +
  geom_col(
    data = dat %>%
      bind_rows(.id = "id") %>%
      filter(age1 > 0) |>
      group_by(id, age) |>
      count(),
    aes(x = age, y = n),
    color = "white",
    fill = "black",
    alpha = 0.2
  ) +
  facet_wrap( ~ factor(
    id,
    labels = c("12/2022 - 4/2023", "4/2023 - 8/2023", "8/2023 - 12/2023")
  )) +
  labs(y = "Number of cases", x = "Age (years)") +
  scale_x_continuous(position = "top")+
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )

sero_ch1_d <- (exp_in_ch1.2 /
                 age_profile_sp_cm /
                 ts_ch1_b) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_ch1_d.svg",plot = sero_ch1_d,
       width = 20,height = 16,dpi = 500)

###

ts_ch1_viro <- df_cases_ch1_23 %>%
  group_by(adm_week, wave) %>%
  count() %>%
  ggplot(aes(x = adm_week, y = n, fill = wave)) +
  geom_col(alpha = 0.6, color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    name = "Collection date",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31"))
  ) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, le = 4)) +
  scale_fill_manual(values = c("#582C83FF", "#FFC72CFF")) +
  theme_minimal() +
  labs(y = "Cases") +
  theme(
    axis.title.y = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_colourbar(barwidth = 20), color = "none")

# per_serotype <- df_viro %>%
#   group_by(adm_month) %>%
#   count(sero_gr) %>%
#   mutate(total = sum(n),
#          per = n/total) %>%
#   ggplot(aes(x = adm_month,y=per,fill = sero_gr))+
#   geom_bar(position="fill", stat="identity")+
#   scale_x_date(date_breaks = "1 month", date_labels = "%b",
#                limits = c(as.Date("2023-01-01"),as.Date("2023-12-31")),
#                name = "Collection month (2023)")+
#   scale_y_continuous(labels = scales::label_percent())+
#   labs(fill = "Serotype group",y = "Percentage (%)")+
#   theme_minimal()+
#   theme(axis.title.y = element_text(size = 18),
#         axis.title.x = element_text(size = 18),
#         axis.ticks.x = element_blank(),
#         legend.position = "bottom",
#         plot.tag = element_text(face = "bold", size = 18),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         legend.text = element_text(size = 15),
#         legend.title = element_text(size = 18))


sero_viro_ch1 <- (exp_in_ch1 /
                    ts_ch1_viro /
                    per_serotype) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_viro_ch1.svg",plot = sero_viro_ch1,
       width = 20,height = 16,dpi = 500)

df_viro |>
  pull(sero_gr) |>
  unique()

per_serotype <- df_viro %>%
  group_by(adm_month) %>%
  count(sero_gr) %>%
  mutate(total = sum(n), per = n / total) %>%
  ggplot(aes(
    x = adm_month,
    y = per,
    fill = factor(sero_gr, levels = c("neg", "Other", "CV-A6", "EV", "EV-A71"))
  )) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(
    aes(label = total, y = 1),
    stat = "unique",
    vjust = -0.5,
    size = 4
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = c(as.Date("2023-01-01"), as.Date("2023-12-31")),
    name = "Collection month (2023)"
  ) +
  scale_y_continuous(labels = scales::label_percent()) +  # extra space for label
  scale_fill_manual(
    values = c(
      "neg"    = "#00B0F6",
      "Other"  = "#E76BF3",
      "CV-A6"  = "#00BF7D",
      "EV"     = "#E59866",
      "EV-A71" = "#F8766D"
    )
  ) +
  labs(y = "Percentage (%)", fill = "Serotype group") +
  theme_minimal() +
  theme(
    axis.title.y  = element_text(size = 18),
    axis.title.x  = element_text(size = 18),
    axis.ticks.x  = element_blank(),
    legend.position = "bottom",
    plot.tag      = element_text(face = "bold", size = 18),
    axis.text.x   = element_text(size = 18),
    axis.text.y   = element_text(size = 18),
    legend.text   = element_text(size = 15),
    legend.title  = element_text(size = 18)
  )

per_serotype

##

# ggplot_build(per_serotype)$data[[1]] |> dplyr::distinct(fill, group)

viro_recon <- recontructed_cases_count_df |>
  ggplot(aes(x = week))+
  geom_line(aes(y = cases),linetype = "dashed")+
  # geom_ribbon(aes(ymin = cases_u,ymax = cases_l),fill = "blue",alpha = .3)+
  geom_line(aes(y = ev71_cases))+
  geom_ribbon(aes(ymin = ev71_cases_u,ymax = ev71_cases_l),fill = "red",alpha = .3)+
  geom_col(data = df_cases_count_ch1_23 |>
             mutate(day = as.Date(time),
                    week = as.Date(floor_date(day, "week")))|>
             group_by(week) |>
             summarise(cases = sum(n)),
           aes(x = week, y  = cases),alpha = .3)+
  geom_point(data = df_viro, aes(x = admission_date, y = pos*900),shape = "|",size = 2)+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b",
               name = "Collection date",
               limits = c(as.Date("2023-01-01"),as.Date("2023-12-31"))) +
  scale_y_continuous(limits = c(0,900),breaks = seq(0,900, le = 4))+
  labs(x = "Admission day (2023)",y = "Cases") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))+
  guides(fill=guide_colourbar(barwidth=20),
         color = "none")

sero_viro_ch1_r <- (exp_in_ch1 /
                     viro_recon/
                    per_serotype) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/sero_viro_ch1_r.svg",plot = sero_viro_ch1_r,
       width = 20,height = 16,dpi = 500)

###

compare_ev71 <- recontructed_df |>
  mutate(
    day = as.Date(time),
    week = as.Date(floor_date(day, "week")),
    period = case_when(
      week <= as.Date("2023-04-30") ~ "12/2022 - 4/2023",
      week > as.Date("2023-04-30") &
        week <= as.Date("2023-08-31") ~ "4/2023 - 8/2023",
      week > as.Date("2023-08-31") ~ "8/2023 - 12/2023"
    )
  ) |>
  group_by(period, age) |>
  summarise(
    ev_a71_cases = sum(ev71_c),
    raw_n = sum(n),
    .groups = "drop"
  ) |>
  ggplot(aes(x = age, y = ev_a71_cases)) +
  geom_col(alpha = 0.3) +
  geom_line(data = expected_incidence, aes(x = cohort, y = exp_inc),linewidth = 1) +
  scale_x_continuous(position = "top") +
  facet_wrap( ~ period) +
  labs(y = "Number of cases", x = "Age (years)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.tag = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    panel.grid.minor.x = element_blank()
  )


final_p <- (compare_ev71 /
                      viro_recon/
                      per_serotype) +
  plot_layout(heights = c(1, 1, 1))

ggsave("./fig_am/final_p.svg",plot = final_p,
       width = 20,height = 16,dpi = 500)
