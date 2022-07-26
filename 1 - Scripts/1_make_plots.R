#### SETUP ####
library(here)
source(here("global_options.R"))

#### DATA ####
load(here("0 - Data", "combined_data.RData"))

#### PAPER PLOTS ####

# Figure 1 
dg = df %>% 
  # start in June
  filter(ymd >= "2021-06-01" & ymd <= "2022-06-01") %>%
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday") %>%
  group_by(state) %>% arrange(date) %>%
  # define episode start (must last at least 2 weeks)
  mutate(trigger_on = cdc_flag & !lag(cdc_flag,1) & lead(cdc_flag,1),
         trigger_off = cdc_flag & !lead(cdc_flag,1) & !lead(cdc_flag,2),
         trigger_on2 = cdc_flag & !lag(cdc_flag,1),
         trigger_off2 = cdc_flag & !lead(cdc_flag,1),
         trigger_on3 = alt_flag1 & !lag(alt_flag1,1),
         trigger_off3 = alt_flag1 & !lead(alt_flag1,1),
         trigger_on4 = alt_flag2 & !lag(alt_flag2,1),
         trigger_off4 = alt_flag2 & !lead(alt_flag2,1),
         ) %>%
  # define variables for plotting episodes
  group_by(state) %>% mutate(grp = lag(cdc_flag,1)!=cdc_flag | is.na(lag(cdc_flag,1)),
                             n = n(),
                             epoch = sapply(1:n, function(a) sum(grp[1:a]))) %>%
  # calculate episode duration
  group_by(state, epoch) %>% mutate(last = max(ymd[cdc_flag]), time = as.numeric(last-ymd)/7 +1,
                                    max = max(deaths_21_lag_100k[cdc_flag], na.rm = T)*7) 

# subsets
num_ep = dg %>% filter(trigger_on | trigger_off) %>% group_by(state) %>% arrange(date) %>%
  mutate(ep = sapply(1:n(), function(a) sum(trigger_on[1:a]))) %>%
  group_by(state, ep) %>% mutate(on_date = ifelse(sum(trigger_on>0), date[trigger_on], "2021-01-01")) %>%
  filter(on_date>="2021-06-01")

table(num_ep$trigger_on)
table(num_ep$trigger_off)

# create figure 1
plot1 = ggplot(dg %>% filter(ymd <= "2022-05-15"), aes(x = ymd + 21, y = deaths_21_lag_100k*7)) +
  geom_line(col = "grey", lwd = .3) + 
  geom_line(data = dg %>% filter(cdc_flag), 
            aes(x = ymd + 21, y = deaths_21_lag_100k*7, group = paste(state, epoch)), col = "navy", lwd = .3) +
  facet_wrap(.~state, ncol = 6) + 
  geom_point(data = dg %>% filter(trigger_on2), col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7), pch = 16, size = 1) + 
  geom_point(data = dg %>% filter(trigger_off2), col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7), pch = 1, size = 1) + 
  #geom_text(data = dg %>% filter(trigger_on2 | trigger_off2), aes(x = as.Date(ifelse(trigger_on2, ymd-7 + 21, ymd-7 + 21)), y = deaths_21_lag_100k*7 + 4,
  #                                                              label = format(ifelse(deaths_21_lag_100k*7 < 10,
  #                                                                                    round(deaths_21_lag_100k*7,1),
  #                                                                                    round(deaths_21_lag_100k*7)), nsmall = 1)),
  #          size = 2) + 
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_brewer(guide = "none", name = "", palette = "Set1") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.background = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text=element_text(size = 9, vjust = -1.3)) + 
  labs(x = "", y = "Average weekly deaths per 100K population") + 
  geom_hline(yintercept = 0.9, lty = 3) 

  
ggsave(here("2 - Figures", "cdc_plot_deaths.png"), plot = plot1, width = 8, height = 11)


# Table S1
# pull CDC indicators and lagged 21 day outcomes
d_out_pre = dg %>% ungroup() %>% mutate(deaths_weekly = deaths_21_lag_100k*7,
                                    admits_weekly = admits_confirmed_100K*7,
                                    cases_weekly = round(cases_avg_per_100k*7),
                                    perc_covid_100 = perc_covid*100,
                                    deaths_weekly = ifelse(ymd>="2022-05-15", NA, deaths_weekly)) %>%
  arrange(ymd) %>% group_by(state) %>%
  mutate(zeke_time_0 = deaths_avg_per_100k*7 > .9,
         test_date = lead(ymd, 1),
         zeke_time_1 = lead(deaths_avg_per_100k*7, 1) > .9,
         zeke_time_2 = lead(deaths_avg_per_100k*7, 2) > .9,
         zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > .9,
         zeke_time_4 = lead(deaths_avg_per_100k*7, 4) > .9,
         zeke_time_5 = lead(deaths_avg_per_100k*7, 5) > .9,
         zeke_time_6 = lead(deaths_avg_per_100k*7, 6) > .9,
         zeke_time_7 = lead(deaths_avg_per_100k*7, 7) > .9,
         zeke_time_8 = lead(deaths_avg_per_100k*7, 8) > .9,
         zeke_time_9 = lead(deaths_avg_per_100k*7, 9) > .9,
         zeke_time_10 = lead(deaths_avg_per_100k*7, 10) > .9,
         zeke_time_11 = lead(deaths_avg_per_100k*7, 11) > .9,
         zeke_time_12 = lead(deaths_avg_per_100k*7, 12)> .9,
         zeke_lt_eq3 = (zeke_time_0 | zeke_time_1 | zeke_time_2 | zeke_time_3),
         zeke_lt_eq6 = (zeke_lt_eq3 + zeke_time_4 + zeke_time_5 + zeke_time_6) > 0,
         time_to_zeke = 15,
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_0, 0, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_1, 1, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_2, 2, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_3, 3, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_4, 4, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_5, 5, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_6, 6, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_7, 7, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_8, 8, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_9, 9, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_10, 10, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_11, 11, time_to_zeke),
         time_to_zeke = ifelse(time_to_zeke==15 & zeke_time_12, 12, time_to_zeke)
  )

save(d_out_pre, file = here("0 - Data", "state_time_data.RData"))

d_out = d_out_pre %>% gather(trigger, ind, trigger_on2, trigger_off2) %>%
  filter(ind==TRUE) %>% 
  mutate(type = ifelse(grepl("trigger_on", trigger), "Start", "End")) %>% dplyr::select(-ind) %>%
  dplyr::select(ymd, state, time, cases_weekly, admits_weekly, perc_covid_100, deaths_weekly, type, max, check_bound, check_bound2, type,
                zeke_time_0, zeke_lt_eq3, zeke_lt_eq6, time_to_zeke) %>%
  rename("Start date" = 1, "State" = 2, "Duration of 'high' episode (weeks)" = 3, "Weekly cases per 100K" = 4,
         "Weekly hospital admissions per 100K" = 5, "Percentage of inpatient beds occupied by COVID-19 patients" = 6,
         "Weekly deaths per 100K 21 days after start" = 7, "type" = 8)
  
d_out_alt1 = d_out_pre %>% gather(trigger, ind, trigger_on3, trigger_off3) %>%
  filter(ind==TRUE) %>% 
  mutate(type = ifelse(grepl("trigger_on", trigger), "Start", "End")) %>% dplyr::select(-ind) %>%
  dplyr::select(ymd, state, time, cases_weekly, admits_weekly, perc_covid_100, deaths_weekly, type, max, check_bound, check_bound2, type) %>%
  rename("Start date" = 1, "State" = 2, "Duration of 'high' episode (weeks)" = 3, "Weekly cases per 100K" = 4,
         "Weekly hospital admissions per 100K" = 5, "Percentage of inpatient beds occupied by COVID-19 patients" = 6,
         "Weekly deaths per 100K 21 days after start" = 7, "type" = 8)
  
d_out_alt2 = d_out_pre %>% gather(trigger, ind, trigger_on4, trigger_off4) %>%
  filter(ind==TRUE) %>%
  mutate(type = ifelse(grepl("trigger_on", trigger), "Start", "End")) %>% dplyr::select(-ind) %>%
  dplyr::select(ymd, state, time, cases_weekly, admits_weekly, perc_covid_100, deaths_weekly, type, max, check_bound, check_bound2, type) %>%
  rename("Start date" = 1, "State" = 2, "Duration of 'high' episode (weeks)" = 3, "Weekly cases per 100K" = 4,
         "Weekly hospital admissions per 100K" = 5, "Percentage of inpatient beds occupied by COVID-19 patients" = 6,
         "Weekly deaths per 100K 21 days after start" = 7, "type" = 8)
  
write.csv(d_out, file = here("2 - Figures", "table_s1.csv"))

# Figure 2
us = df %>% 
  # start in June
  filter(ymd >= "2021-06-01") %>% 
  # calculate state CFR
  group_by(ymd, state, dotw, REGION) %>% 
  summarize(
            deaths_lag.21 = sum(deaths_21_lag),
            deaths_lag.17 = sum(deaths_17_lag),
            admits_lag.14 = sum(admits_7_lag),
            admits_lag.21 = sum(admits_confirmed_avg),
            hosped_avg = sum(hosped_avg),
            hosp_case = sum(admits_confirmed_avg)/sum(cases_avg),
            cases_avg = sum(cases_avg),
            deaths_avg = sum(deaths_avg),
            cfr = deaths_lag.21/cases_avg*100,
            cfr.17 = deaths_lag.17/cases_avg*100) %>% 
  # report 1x/week to reduce noise
  #filter(dotw == "Monday") %>% 
  # calculate regional CFR
  group_by(ymd, REGION) %>% 
  mutate(num_reg = sum(deaths_lag.21),
         num_reg.17 = sum(deaths_lag.17),
         denom_reg = sum(cases_avg),
         hosped_avg = sum(hosped_avg),
         cfr_region = num_reg/denom_reg*100,
         cfr_region.17 = num_reg.17/denom_reg*100,
         hosp_case1 = admits_lag.21/cases_avg,
         hosp_case2 = admits_lag.14/cases_avg,
         hosp_case_reg = sum(admits_lag.21)/sum(cases_avg),
         region = case_when(REGION=="1"~"Northeast", REGION=="2"~"Midwest", REGION=="3"~"South", REGION=="4"~"West"),
         region = factor(region, levels = c("Northeast", "Midwest", "South", "West")))


