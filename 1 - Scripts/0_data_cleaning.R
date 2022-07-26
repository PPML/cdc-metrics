#### SETUP ####
library(here)
source(here("global_options.R"))

#### STATE POPULATION DATA ####
data(state_census)
data(hhs_regions)

#### HOSPITALIZATIONS ####
h = read.csv(here("0 - Data", "hosps.csv")) %>% group_by(state) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  arrange(date) %>%
  mutate(admits_confirmed = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         admits_suspected = previous_day_admission_adult_covid_suspected + previous_day_admission_pediatric_covid_suspected,
         admits = admits_confirmed + admits_suspected,
         admits_avg = rollmean(admits, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_avg = ifelse(is.na(admits_avg), 0, admits_avg),
         admits_confirmed_avg = rollmean(admits_confirmed, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_suspected_avg = rollmean(admits_suspected, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         denom_factor = ifelse(total_adult_patients_hospitalized_confirmed_and_suspected_covid >= 0, total_adult_patients_hospitalized_confirmed_and_suspected_covid, NA) + 
           ifelse(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid >= 0, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid, NA),
         num_factor = ifelse(total_adult_patients_hospitalized_confirmed_covid >= 0, total_adult_patients_hospitalized_confirmed_covid, NA) + 
           ifelse(total_pediatric_patients_hospitalized_confirmed_covid >= 0, total_pediatric_patients_hospitalized_confirmed_covid, NA),
         num_factor = ifelse(is.na(denom_factor), NA, num_factor),
         denom_factor = ifelse(is.na(num_factor), NA, denom_factor),
         factor = ifelse(!is.na(num_factor) & !is.na(denom_factor), num_factor/denom_factor, 1),
         factor_count = is.na(num_factor) | is.na(denom_factor),
         hosped_avg = rollmean(total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid, k = 7, align = "right", na.pad = T, na.rm = T),
         perc_covid = rollmean(percent_of_inpatients_with_covid, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         factor_avg1 = rollmean(num_factor, k = 7, align = "right", na.pad = TRUE, na.rm = F)/rollmean(denom_factor, k = 7, align = "right", na.pad = TRUE, na.rm = F),
         factor_avg1 = ifelse(is.na(factor_avg1) | factor_avg1 > 1, 1, factor_avg1),
         factor_avg2 = rollmean(num_factor, k = 7, align = "right", na.pad = TRUE, na.rm = T)/rollmean(denom_factor, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         factor_avg2 = ifelse(is.na(factor_avg2) | factor_avg2 > 1, 1, factor_avg2),
         perc_covid = perc_covid*factor_avg2
         )

#### ANOMALOUS MD DATA ####
# dates of missing data
dates = seq(as.Date("2021-12-05"), as.Date("2021-12-19"), "days")

# pull in data from the state
m = read.csv(here("0 - Data", "MDCOVID19_CasesPer100KpopulationStatewide.csv")) %>%
  separate(ReportDate, into = c("Date", "Time"), sep = "\ ") %>%
  mutate(date = as.Date(Date, format = "%Y/%m/%d"),
         state = "Maryland") %>% filter(date %in% dates)

#### CASE DATA ####
df = read.csv(here("0 - Data", "us-states.csv")) %>% 
  
  # join to state data
  left_join(state_census, c("state"="NAME")) %>%
  filter(!is.na(POPESTIMATE2019)) %>%
  
  # group by state
  group_by(state) %>% 
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-")) %>% 
  
  arrange(ymd, .by_group = TRUE) %>%
  
  
  # filter out PR & Virgin Islands & arrange
  filter(!state%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "U.S. Virgin Islands", 
                     "United States")) %>%
  arrange(state, ymd) %>%
  
  # join to hospital data
  left_join(h %>% dplyr::select(date, state, admits_avg,
                                admits_confirmed_avg, admits_suspected_avg, perc_covid, hosped_avg, factor, factor_count, 
                                factor_avg1, factor_avg2, num_factor, denom_factor), 
            c("ABBR"="state", "ymd"="date")) %>%
  mutate(admits_confirmed_100K = admits_confirmed_avg/POPESTIMATE2019*100000,
         admits_100K = admits_avg/POPESTIMATE2019*100000) %>%
  
  # link to MD
  left_join(m %>% dplyr::select(date, state, Statewide), 
            c("state" = "state", "ymd" = "date")) %>%
  
  # estimate CDC metrics
  mutate(
    
  # fix MD as needed
  cases_avg_per_100k = ifelse(is.na(Statewide), cases_avg_per_100k, Statewide),
  cases_avg_per_100k_07d_ago = lag(cases_avg_per_100k),
  cases_avg_per_100k_chg_week = cases_avg_per_100k-cases_avg_per_100k_07d_ago,
  
  # remove NAs from bed percentages
  perc_covid = ifelse(is.na(perc_covid), 0, perc_covid),
  
  # define CDC "high"
  cdc_flag_1 = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_2 = (cases_avg_per_100k < 200/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)), # under 200/100K 7-d
  cdc_flag = cdc_flag_1 | cdc_flag_2,
  cdc_flag_alt = (admits_confirmed_100K > 5/7 | perc_covid > .05),
  alt_flag1 = admits_confirmed_100K > 5/7 | perc_covid > .05,
  alt_flag2 = (cases_avg_per_100k > 100/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)), 
  
  #cdc_flag = cdc_flag_alt,
  
  cdc_flag_inc_susp = (cases_avg_per_100k > 200/7 & (admits_100K > 10/7 | perc_covid > .1)) |
    (cases_avg_per_100k < 200/7 & (admits_100K > 20/7 | perc_covid > .15)),

  # lagged deaths
  deaths_17_lag = lead(deaths_avg, 17), 
  deaths_21_lag = lead(deaths_avg, 21), 
  deaths_07_days_ago = lag(deaths_avg, 7), 
  
  # lagged deaths per 100k
  deaths_07_lag_100k = lead(deaths_avg_per_100k, 7), 
  deaths_12_lag_100k = lead(deaths_avg_per_100k, 12), 
  deaths_13_lag_100k = lead(deaths_avg_per_100k, 13), 
  deaths_14_lag_100k = lead(deaths_avg_per_100k, 14), 
  deaths_15_lag_100k = lead(deaths_avg_per_100k, 15), 
  deaths_16_lag_100k = lead(deaths_avg_per_100k, 16), 
  deaths_17_lag_100k = lead(deaths_avg_per_100k, 17), 
  deaths_18_lag_100k = lead(deaths_avg_per_100k, 18), 
  deaths_19_lag_100k = lead(deaths_avg_per_100k, 19), 
  deaths_20_lag_100k = lead(deaths_avg_per_100k, 20), 
  deaths_21_lag_100k = lead(deaths_avg_per_100k, 21), 
  deaths_22_lag_100k = lead(deaths_avg_per_100k, 22), 
  deaths_23_lag_100k = lead(deaths_avg_per_100k, 23), 
  deaths_24_lag_100k = lead(deaths_avg_per_100k, 24), 
  deaths_25_lag_100k = lead(deaths_avg_per_100k, 25), 
  deaths_26_lag_100k = lead(deaths_avg_per_100k, 26),

  # lagged cases (there has to be a better way to tidyverse this)
  cases_lag_00 = lag(cases_avg, 00), 
  cases_lag_01 = lag(cases_avg, 01), 
  cases_lag_02 = lag(cases_avg, 02), 
  cases_lag_03 = lag(cases_avg, 03), 
  cases_lag_04 = lag(cases_avg, 04), 
  cases_lag_05 = lag(cases_avg, 05), 
  cases_lag_06 = lag(cases_avg, 06),
  cases_lag_07 = lag(cases_avg, 07), 
  cases_lag_08 = lag(cases_avg, 08), 
  cases_lag_09 = lag(cases_avg, 09), 

  cases_lag_10 = lag(cases_avg, 10), 
  cases_lag_11 = lag(cases_avg, 11), 
  cases_lag_12 = lag(cases_avg, 12), 
  cases_lag_13 = lag(cases_avg, 13), 
  cases_lag_14 = lag(cases_avg, 14), 
  cases_lag_15 = lag(cases_avg, 15), 
  cases_lag_16 = lag(cases_avg, 16),
  cases_lag_17 = lag(cases_avg, 17), 
  cases_lag_18 = lag(cases_avg, 18), 
  cases_lag_19 = lag(cases_avg, 19), 
  
  cases_lag_20 = lag(cases_avg, 20), 
  cases_lag_21 = lag(cases_avg, 21), 
  cases_lag_22 = lag(cases_avg, 22), 
  cases_lag_23 = lag(cases_avg, 23), 
  cases_lag_24 = lag(cases_avg, 24), 
  cases_lag_25 = lag(cases_avg, 25), 
  cases_lag_26 = lag(cases_avg, 26),
  cases_lag_27 = lag(cases_avg, 27), 
  cases_lag_28 = lag(cases_avg, 28), 
  cases_lag_29 = lag(cases_avg, 29), 
  
  cases_lag_30 = lag(cases_avg, 30), 
  cases_lag_31 = lag(cases_avg, 31), 
  cases_lag_32 = lag(cases_avg, 32), 
  cases_lag_33 = lag(cases_avg, 33), 
  cases_lag_34 = lag(cases_avg, 34), 
  cases_lag_35 = lag(cases_avg, 35), 
  cases_lag_36 = lag(cases_avg, 36),
  cases_lag_37 = lag(cases_avg, 37), 
  cases_lag_38 = lag(cases_avg, 38), 
  cases_lag_39 = lag(cases_avg, 39), 
  
  cases_lag_40 = lag(cases_avg, 40), 
  cases_lag_41 = lag(cases_avg, 41), 
  cases_lag_42 = lag(cases_avg, 42), 
  cases_lag_43 = lag(cases_avg, 43), 
  cases_lag_44 = lag(cases_avg, 44), 
  cases_lag_45 = lag(cases_avg, 45), 
  cases_lag_46 = lag(cases_avg, 46),
  cases_lag_47 = lag(cases_avg, 47), 
  cases_lag_48 = lag(cases_avg, 48), 
  cases_lag_49 = lag(cases_avg, 49), 
  
  cases_lag_50 = lag(cases_avg, 50), 
  cases_lag_51 = lag(cases_avg, 51), 
  
  # lagged hosps per 100K
  admits_7_lag = lead(admits_confirmed_avg, 7),
  admits_7d_ago = lag(admits_confirmed_avg, 7),
  admits_21d_ago = lag(admits_confirmed_avg, 21),
  
  # day of the week
  dotw = weekdays(ymd),
  
  # indicators yesterday
  cases_yesterday = lag(cases_avg_per_100k, 8),
  admits_yesterday = lag(admits_confirmed_100K, 8),
  perc_yesterday = lag(perc_covid, 8),
  
  check_bound = (cases_yesterday<200/7 & cases_avg_per_100k>=200/7 & 
                   (admits_yesterday>=10/7 | perc_yesterday>=.1)),
  
  check_bound2 = (cases_yesterday<200/7 & cases_avg_per_100k>=200/7),
  
  # check completeness
  chk = paste(ymd, state)) 

k = table(df$chk)
k[k > 1]

#### save
write.csv(df, here("0 - Data", "combined_data.csv"))
save(df, file = here("0 - Data", "combined_data.RData"))

