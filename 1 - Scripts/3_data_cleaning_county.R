#### SETUP ####
library(here)
source(here("global_options.R"))

#### STATE POPULATION DATA ####
data(county_census)
data(state_census)
s = state_census %>% dplyr::select(NAME, ABBR)

#### HOSPITALIZATIONS ####
c = read.csv(here("0 - Data", "United_States_COVID-19_Community_Levels_by_County.csv")) %>%
  mutate(ymd = as.Date(date_updated, "%m/%d/%y")+1, fips = as.numeric(county_fips)) %>%
  filter(ymd == "2022-05-06") %>%
  dplyr::select(county, county_fips, state, fips,
                county_population, health_service_area_number,
                health_service_area, health_service_area_population)

h = read.csv(here("0 - Data", "hosps_county.csv")) %>% 
  mutate(fips = as.numeric(fips_code)) %>%
  left_join(c, c("fips" = "fips")) %>%
  mutate(date = as.Date(collection_week, format = "%Y/%m/%d")+5,
         previous_day_admission_adult_covid_confirmed_7_day_sum = ifelse(previous_day_admission_adult_covid_confirmed_7_day_sum < 0, 2, previous_day_admission_adult_covid_confirmed_7_day_sum),
         previous_day_admission_pediatric_covid_confirmed_7_day_sum = ifelse(previous_day_admission_pediatric_covid_confirmed_7_day_sum < 0, .5, previous_day_admission_pediatric_covid_confirmed_7_day_sum),
         previous_day_admission_adult_covid_suspected_7_day_sum = ifelse(previous_day_admission_adult_covid_suspected_7_day_sum < 0, NA, previous_day_admission_adult_covid_suspected_7_day_sum),
         previous_day_admission_pediatric_covid_suspected_7_day_sum = ifelse(previous_day_admission_pediatric_covid_suspected_7_day_sum < 0, NA, previous_day_admission_pediatric_covid_suspected_7_day_sum),
         inpatient_beds_used_covid_7_day_sum = ifelse(total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum < 0, 0, total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum) + 
           ifelse(total_adult_patients_hospitalized_confirmed_covid_7_day_sum < 0, 0, total_adult_patients_hospitalized_confirmed_covid_7_day_sum),
         inpatient_beds_7_day_sum = ifelse(inpatient_beds_7_day_avg < 0, NA, inpatient_beds_7_day_avg)) %>%
  group_by(health_service_area_number, health_service_area_population, date) %>%
  summarize(admits_confirmed = sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = T) + sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum, na.rm = T),
         admits_suspected = sum(previous_day_admission_adult_covid_suspected_7_day_sum, na.rm = T) + sum(previous_day_admission_pediatric_covid_suspected_7_day_sum, na.rm = T),
         inpt_beds_covid = sum(inpatient_beds_used_covid_7_day_sum, na.rm = T),
         inpt_beds = sum(inpatient_beds_7_day_sum, na.rm = T),
         missing_kid = sum(is.na(previous_day_admission_pediatric_covid_confirmed_7_day_sum)), missing_kid_perc = mean(is.na(previous_day_admission_pediatric_covid_confirmed_7_day_sum)),
         missing_adult = sum(is.na(previous_day_admission_adult_covid_confirmed_7_day_sum)), missing_adult_perc = mean(is.na(previous_day_admission_adult_covid_confirmed_7_day_sum))) %>%
  mutate(admits = admits_confirmed + admits_suspected,
         admits_avg = admits/7,
         admits_confirmed_avg = admits_confirmed/7,
         admits_confirmed_avg = ifelse(is.na(admits_confirmed_avg), 0, admits_confirmed_avg),
         admits_suspected_avg = admits_suspected/7,
         perc_covid = inpt_beds_covid/7/inpt_beds,
         perc_covid = ifelse(perc_covid=="Inf", 0, perc_covid),
         admits_confirmed_100K = admits_confirmed_avg/health_service_area_population*100000,
         admits_100K = admits_avg/health_service_area_population*100000) %>%
  left_join(c, c("health_service_area_number" = "health_service_area_number")) %>% ungroup() %>%
  filter(!fips%in%c(36005, 36047, 36061, 36081)) %>%
  mutate(fips = ifelse(fips %in% c(36085), 36998, fips))

g = h %>% group_by(health_service_area, health_service_area_population.x) %>%
  summarize(missing_kid = sum(missing_kid),
            missing_kid_perc = mean(missing_kid_perc),
            missing_adult = sum(missing_adult),
            max_perc_missing = max(missing_adult_perc),
            missing_adult_perc = mean(missing_adult_perc))
table(g$missing_adult_perc)
table(g$max_perc_missing)

sum(g$missing_adult_perc>0.01)
sum(g$max_perc_missing>.4)

set1 = g$health_service_area[g$missing_adult_perc<0.01 & g$missing_kid_perc<0.01]
set2 = g$health_service_area[g$max_perc_missing<.15]


k = table(paste(h$fips, h$date))
k[k > 1]

chk = h %>% filter(fips==09007)
ggplot(chk, aes(x = date, y = admits_confirmed_avg)) + geom_line()
ggplot(chk, aes(x = date, y = perc_covid)) + geom_line()


# county census
county_census = county_census %>% 
  mutate(fips = as.numeric(FIPS),
         # rename for NYC
         fips = ifelse(fips %in% c(36005, 36047, 36061, 36081, 36085),
                                                            36998, fips)) %>%
  group_by(fips, STNAME) %>% summarize(POPESTIMATE2019 = sum(POPESTIMATE2019),
                                        CTYNAME = CTYNAME[1]) %>%
  mutate(CTYNAME = ifelse(fips==36998, "New York City", CTYNAME))

k = table(county_census$fips)
k[k > 1]

#### CASE DATA ####
df = read.csv(here("0 - Data", "us-counties-2021.csv")) %>% 
  bind_rows(read.csv(here("0 - Data", "us-counties-2022.csv"))) %>%
  mutate(fips = as.numeric(sub("USA-", "", geoid))) %>%
  
  # join to county data
  left_join(county_census, c("fips"="fips")) %>%
  filter(!is.na(POPESTIMATE2019)) %>%
  
  # join to state data
  left_join(s, c("state"="NAME")) %>%
  # group by state
  group_by(fips) %>% 
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-")) %>% 
  
  arrange(ymd, .by_group = TRUE) %>%
  
  # filter out PR & Virgin Islands & arrange
  filter(!STNAME%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "U.S. Virgin Islands", 
                     "United States")) %>%
  arrange(fips, ymd) %>%
  
  # join to hospital data
  left_join(h %>% dplyr::select(date, fips, admits_avg, health_service_area_population.x,
                                admits_confirmed_avg, admits_suspected_avg, perc_covid,
                                admits_confirmed_100K, admits_100K), 
            c("fips"="fips", "ymd"="date")) %>%
  
  # estimate CDC metrics
  mutate(#POPESTIMATE2019 = health_service_area_population.x,
    
  # remove NAs from bed percentages
  perc_covid = ifelse(is.na(perc_covid), 0, perc_covid),
  admits_confirmed_100K = ifelse(is.na(admits_confirmed_100K), 0, admits_confirmed_100K),
  
  # define CDC "high"
  cdc_flag_1 = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_2 = (cases_avg_per_100k < 200/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)), # under 200/100K 7-d
  cdc_flag = cdc_flag_1 | cdc_flag_2,
  
  cdc_flag_100_low = (cases_avg_per_100k > 100/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_200_low = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_500_low = (cases_avg_per_100k > 500/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_1000_low = (cases_avg_per_100k > 1000/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  
  cdc_flag_100_high = (cases_avg_per_100k > 100/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)),  # over 200/100K 7-d
  cdc_flag_200_high = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)),  # over 200/100K 7-d
  cdc_flag_500_high = (cases_avg_per_100k > 500/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)),  # over 200/100K 7-d
  cdc_flag_1000_high = (cases_avg_per_100k > 1000/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)),  # over 200/100K 7-d
  
  alt_flag1 = admits_confirmed_100K > 5/7 | perc_covid > .05,
  alt_flag2 = (cases_avg_per_100k > 100/7 | (admits_confirmed_100K > 10/7 | perc_covid > .1)), 
  
  cdc_flag_inc_susp = (cases_avg_per_100k > 200/7 & (admits_100K > 10/7 | perc_covid > .1)) |
    (cases_avg_per_100k < 200/7 & (admits_100K > 20/7 | perc_covid > .15)),

  # lagged deaths
  deaths_17_lag = lead(deaths_avg, 17), 
  deaths_21_lag = lead(deaths_avg, 21), 
  
  # lagged deaths per 100k
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
  chk = paste(ymd, fips)) %>% ungroup() %>%
  mutate(county_rank = rank(-1*POPESTIMATE2019))

k = table(df$chk)
k[k > 1]

#### save
write.csv(df, here("0 - Data", "combined_data_county.csv"))
save(df, file = here("0 - Data", "combined_data_county.RData"))




