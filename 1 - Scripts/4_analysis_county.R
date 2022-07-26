#### SETUP ####
library(here)
source(here("global_options.R"))

#### DATA ####
load(here("0 - Data", "combined_data_county.RData"))

#### PAPER PLOTS ####

# Figure 1 
dg = df %>% 
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday") %>%
  filter(date >= "2021-01-01") 

dg = dg %>%
  
  # start in June
  filter(ymd >= "2021-06-01" & ymd <= "2022-06-01") %>%
  
  # set up order
  group_by(fips) %>%
  arrange(fips, date) %>%
  
  # define episode start (must last at least 2 weeks)
  mutate(trigger_on = cdc_flag & !lag(cdc_flag,1) & !lag(cdc_flag,2) & lead(cdc_flag,1),
         trigger_off = cdc_flag & lag(cdc_flag, 1) & !lead(cdc_flag,1) & !lead(cdc_flag,2),
         trigger_on2 = cdc_flag & !lag(cdc_flag,1),
         trigger_off2 = cdc_flag & !lead(cdc_flag,1),
         trigger_on3 = alt_flag1 & !lag(alt_flag1,1),
         trigger_off3 = alt_flag1 & !lead(alt_flag1,1),
         trigger_on4 = alt_flag2 & !lag(alt_flag2,1),
         trigger_off4 = alt_flag2 & !lead(alt_flag2,1),
         lag = lag(cdc_flag,1),
         lead = lead(cdc_flag,1)) %>%
  # define variables for plotting episodes
  group_by(fips) %>% 
  mutate(#last = max(ymd[cdc_flag]), time = as.numeric(last-ymd)/7 +1,
    #max = max(deaths_21_lag_100k[cdc_flag], na.rm = T)*7,
    label = paste(CTYNAME, ", ", ABBR, sep = "")) %>%
  mutate(epoch = case_when(ymd<"2021-10-01"~"Per 1", 
                           ymd>="2021-10-01" & ymd<"2022-02-01"~"Per 2",
                           ymd >= "2022-02-01"~"Per 3"))

num_ep = dg %>% filter(trigger_on | trigger_off) %>% group_by(label) %>% arrange(date) %>%
  mutate(ep = sapply(1:n(), function(a) sum(trigger_on[1:a]))) %>%
  group_by(label, ep) %>% mutate(on_date = ifelse(sum(trigger_on>0), date[trigger_on], "2021-01-01")) %>%
  filter(on_date>="2021-06-01") %>% filter(date <= "2022-06-01")

table(num_ep$trigger_on)
table(num_ep$trigger_off)
table(num_ep$date, num_ep$trigger_off)


d_out_pre = dg %>% group_by(label, state) %>% mutate(deaths_weekly = deaths_21_lag_100k*7,
                                        admits_weekly = admits_confirmed_100K*7,
                                        cases_weekly = round(cases_avg_per_100k*7),
                                        perc_covid_100 = perc_covid*100,
                                        deaths_weekly = ifelse(ymd>="2022-05-15", NA, deaths_weekly)) %>%
  arrange(ymd) %>% group_by(fips, label) %>%
  mutate(zeke_time_0 = deaths_avg_per_100k*7 > .9,
         zeke_time_1 = lead(deaths_avg_per_100k*7, 1) > .9,
         zeke_time_2 = lead(deaths_avg_per_100k*7, 2) > .9,
         zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > .9,
         chk = lead(deaths_avg_per_100k*7, 3),
         zeke_time_4 = lead(deaths_avg_per_100k*7, 4) > .9,
         zeke_time_5 = lead(deaths_avg_per_100k*7, 5) > .9,
         zeke_time_6 = lead(deaths_avg_per_100k*7, 6) > .9,
         zeke_time_7 = lead(deaths_avg_per_100k*7, 7) > .9,
         zeke_time_8 = lead(deaths_avg_per_100k*7, 8) > .9,
         zeke_time_9 = lead(deaths_avg_per_100k*7, 9) > .9,
         zeke_time_10 = lead(deaths_avg_per_100k*7, 10) > .9,
         zeke_time_11 = lead(deaths_avg_per_100k*7, 11) > .9,
         zeke_time_12 = lead(deaths_avg_per_100k*7, 12)> .9,
         zeke_lt_eq3 = (zeke_time_0 + zeke_time_1 + zeke_time_2 + zeke_time_3) > 0,
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

save(d_out_pre, file = here("0 - Data", "county_time_data.RData"))

# text stats
d_out = d_out_pre %>%
  gather(trigger, ind, trigger_on2, trigger_off2) %>%
  filter(ind==TRUE) %>% dplyr::select(-ind) %>%
  mutate(type = ifelse(trigger=="trigger_on2", "Start", "End")) %>%
  dplyr::select(ymd, fips, state, cases_weekly, admits_weekly, POPESTIMATE2019, perc_covid_100, deaths_weekly, type, epoch)

# table value
table(d_out$deaths_weekly[d_out$type=="Start"]>=.9)

d_out_alt1 = d_out_pre %>%
  gather(trigger, ind, trigger_on3, trigger_off3) %>%
  filter(ind==TRUE) %>% dplyr::select(-ind) %>%
  mutate(type = ifelse(trigger=="trigger_on3", "Start", "End")) %>%
  dplyr::select(ymd, fips, state, cases_weekly, admits_weekly, POPESTIMATE2019, perc_covid_100, deaths_weekly, type, epoch)

d_out_alt2 = d_out_pre %>%
  gather(trigger, ind, trigger_on4, trigger_off4) %>%
  filter(ind==TRUE) %>% dplyr::select(-ind) %>%
  mutate(type = ifelse(trigger=="trigger_on4", "Start", "End")) %>%
  dplyr::select(ymd, fips, state, cases_weekly, admits_weekly, POPESTIMATE2019, perc_covid_100, deaths_weekly, type, epoch)


# Table S1
# pull CDC indicators and lagged 21 day outcomes

summ_stats = function(d_out, filename = "table1_counties.csv"){
  
  # mortality 21-days later
  k3 = d_out %>% group_by(type) %>% 
    gather(var, value, cases_weekly, admits_weekly, perc_covid_100, deaths_weekly) %>%
    mutate(var = factor(var, levels = c("cases_weekly", "admits_weekly",
                                        "perc_covid_100",
                                        "deaths_weekly")),
           type = factor(type, levels = c("Start", "End"))) %>%
    group_by(type, var) %>%
    summarize(round_val = ifelse(var=="cases_weekly", 0, 1)[1],
              n = sum(!is.na(value)), mean = round(mean(value, na.rm = T), round_val[1]),
              median = round(median(value, na.rm = T), round_val[1]),
              q25 = round(quantile(value, .25, na.rm = T), round_val[1]),
              q75 = round(quantile(value, .75, na.rm = T), round_val[1]))
  
  # mortality 21-days later by epoch
  k4 = d_out %>% 
    group_by(epoch) %>% filter(type=="Start") %>% summarize(n = sum(!is.na(deaths_weekly)),
                                                            mean = round(mean(deaths_weekly, na.rm = T),1),
                                                            median = round(median(deaths_weekly, na.rm = T),1),
                                                            q25 = round(quantile(deaths_weekly, .25, na.rm = T),1),
                                                            q75 = round(quantile(deaths_weekly, .75, na.rm = T),1))
  
  write.csv(bind_rows(k3, k4), file = here("3 - Supplement", filename))
}

summ_stats(d_out)
summ_stats(d_out_alt1, filename = "alt1_counties.csv")
summ_stats(d_out_alt2, filename = "alt2_counties.csv")

# make plot
dg = dg %>% arrange(county_rank)
ranks = sort(unique(dg$county_rank))[1:54]
counties = unique(dg$label[dg$county_rank%in%ranks])

dh = dg %>% filter(county_rank %in% ranks) %>%
  arrange(POPESTIMATE2019) %>%
  mutate(label = factor(label, counties))

plot1 = ggplot(dh %>% filter(ymd <= "2022-05-15"), aes(x = ymd + 21, y = deaths_21_lag_100k*7)) +
  geom_line(col = "grey", lwd = .3) + 
  geom_line(data = dh %>% filter(cdc_flag), 
            aes(x = ymd + 21, y = deaths_21_lag_100k*7, group = paste(state, epoch)), col = "navy", lwd = .3) +
  facet_wrap(.~label, ncol = 6) + 
  geom_point(data = dh %>% filter(trigger_on2), pch = 16, col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7)) + 
  geom_point(data = dh %>% filter(trigger_off2), pch = 1, col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7)) + 
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_brewer(guide = "none", name = "", palette = "Set1") + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 9, vjust = -1.3)) + 
  labs(x = "", y = "Average weekly deaths per 100K population") + 
  geom_hline(yintercept = 0.9, lty = 3) 
plot1

ggsave(here("2 - Figures", "cdc_plot_deaths_county.png"), plot = plot1, width = 10, height = 11)

## sensitivity and specificty
ss = d_out_pre %>% ungroup() %>%
  gather(var, value, cdc_flag, alt_flag1, alt_flag2) %>%
  group_by(var) %>%
  summarize(sens_0 = round(sum(zeke_time_0 & value, na.rm = T)/sum(zeke_time_0 & !is.na(value), na.rm = T), 2),
            sens_3 = round(sum(zeke_time_3 & value, na.rm = T)/sum(zeke_time_3 & !is.na(value), na.rm = T), 2),
            sens_6 = round(sum(zeke_time_6 & value, na.rm = T)/sum(zeke_time_6 & !is.na(value), na.rm = T), 2),
            spec_0 = round(sum(!zeke_time_0 & !value, na.rm = T)/sum(!zeke_time_0 & !is.na(value), na.rm = T), 2),
            spec_3 = round(sum(!zeke_time_3 & !value, na.rm = T)/sum(!zeke_time_3 & !is.na(value), na.rm = T), 2),
            spec_6 = round(sum(!zeke_time_6 & !value, na.rm = T)/sum(!zeke_time_6 & !is.na(value), na.rm = T), 2),
            ppv_0 = round(sum(zeke_time_0 & value, na.rm = T)/sum(value & !is.na(zeke_time_0), na.rm = T), 2),
            ppv_3 = round(sum(zeke_time_3 & value, na.rm = T)/sum(value & !is.na(zeke_time_3), na.rm = T), 2),
            ppv_6 = round(sum(zeke_time_6 & value, na.rm = T)/sum(value & !is.na(zeke_time_6), na.rm = T), 2),
            npv_0 = round(sum(!zeke_time_0 & !value, na.rm = T)/sum(!value & !is.na(zeke_time_0), na.rm = T), 2),
            npv_3 = round(sum(!zeke_time_3 & !value, na.rm = T)/sum(!value & !is.na(zeke_time_3), na.rm = T), 2),
            npv_6 = round(sum(!zeke_time_6 & !value, na.rm = T)/sum(!value & !is.na(zeke_time_6), na.rm = T), 2))

write.csv(ss, file = here("3 - Supplement", "sens_spec_counties.csv"))
