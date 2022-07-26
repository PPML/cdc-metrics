#### SETUP ####
library(here)
source(here("global_options.R"))
source(here("1 - Scripts", "1_make_plots.R"))

#### RESULTS SECTION NUMBERS ####

# number of new episodes
dim(d_out)

# check stats
d_out_pre %>% group_by(trigger_on3) %>% summarize(mean(zeke_time_0, na.rm = T), 
                                                  mean(zeke_lt_eq3, na.rm = T), 
                                                  mean(zeke_lt_eq6, na.rm = T))

d_out_pre %>% group_by(trigger_on4) %>% summarize(mean(zeke_time_0, na.rm = T), 
                                                  mean(zeke_lt_eq3, na.rm = T), 
                                                  mean(zeke_lt_eq6, na.rm = T))

table(d_out_pre$zeke_time_0, d_out_pre$cdc_flag)


summ_stats = function(d_out, filename = "table1.csv"){
  # mortality 21-days later
  k1 = d_out %>% 
    gather(var, value, `Weekly cases per 100K`, `Weekly hospital admissions per 100K`,
           `Percentage of inpatient beds occupied by COVID-19 patients`,
           `Weekly deaths per 100K 21 days after start`) %>%
    mutate(var = factor(var, levels = c("Weekly cases per 100K", "Weekly hospital admissions per 100K",
                                        "Percentage of inpatient beds occupied by COVID-19 patients",
                                        "Weekly deaths per 100K 21 days after start")),
           type = factor(type, levels = c("Start", "End"))) %>%
    group_by(type, var) %>%
    summarize(round_val = ifelse(var=="Weekly cases per 100K", 0, 1)[1],
              n = sum(!is.na(value)), mean = round(mean(value, na.rm = T), round_val[1]),
              median = round(median(value, na.rm = T), round_val[1]),
              q25 = round(quantile(value, .25, na.rm = T), round_val[1]),
              q75 = round(quantile(value, .75, na.rm = T), round_val[1]))
  
  # mortality 21-days later by epoch
  k2 = d_out %>% mutate(epoch = case_when(`Start date`<"2021-10-01"~"Per 1", 
                                     `Start date`>="2021-10-01" & `Start date`<"2022-02-01"~"Per 2",
                                     `Start date` >= "2022-02-01"~"Per 3")) %>%
    group_by(epoch) %>% filter(type=="Start") %>% summarize(n = sum(!is.na(`Weekly deaths per 100K 21 days after start`)),
                      mean = round(mean(`Weekly deaths per 100K 21 days after start`, na.rm = T),1),
                      median = round(median(`Weekly deaths per 100K 21 days after start`, na.rm = T),1),
                      q25 = round(quantile(`Weekly deaths per 100K 21 days after start`, .25, na.rm = T),1),
                      q75 = round(quantile(`Weekly deaths per 100K 21 days after start`, .75, na.rm = T),1))

  write.csv(bind_rows(k1, k2), file = here("3 - Supplement", filename))
  
}

summ_stats(d_out)
summ_stats(d_out_alt1, filename = "alt1_states.csv")
summ_stats(d_out_alt2, filename = "alt2_states.csv")

# sensitivity and specificity
ss = d_out_pre %>% ungroup() %>%
  #filter(ymd >= "2022-04-01") %>%
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

write.csv(ss, file = here("3 - Supplement", "sens_spec_states.csv"))

# case fatality
# aggregate over US
us2 = us %>% group_by(ymd, dotw) %>% 
  filter(ymd>="2021-12-01") %>%
  summarize(
  num = sum(deaths_lag.21), denom = sum(cases_avg),
  admits = sum(admits_lag.21),
  hosps = sum(hosped_avg),
  cfr = num/denom*100,
  cfr.17 = sum(deaths_lag.17)/denom*100,
  hfr.14 = num/sum(admits_lag.14)*100,
  hfr.21 = num/sum(admits_lag.21)*100, 
  hosp_case = sum(admits_lag.21)/sum(cases_avg),
  hosp_case1 = sum(admits_lag.14)/sum(cases_avg)) %>% ungroup() %>%
  mutate(min_omi = cfr == min(cfr, na.rm = T),
         max_omi = cfr == max(cfr, na.rm = T)) %>%
  mutate(hfr.14 = lag(hfr.14, 7),
         cfr.lag = lag(cfr, 21),
         l_cases = log(denom),
         l_deaths_21 = log(num),
         l_admits = log(admits)) 

# maximum
us2 %>% filter(max_omi) %>% mutate(ymd_shift = ymd + 21)

# minimum
us2 %>% filter(min_omi) %>% mutate(ymd_shift = ymd + 21)

