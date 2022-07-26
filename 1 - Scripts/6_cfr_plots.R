####*************************** SETUP *******************************####
library(here)
source(here("global_options.R"))
load(here("0 - Data", "combined_data.RData"))

####*************************** DATA *******************************####

# state data frame
# subset to weekly data
d = df %>% filter(dotw == "Saturday") %>%
  summarize(ymd = ymd,
            state = state,
            REGION = REGION,
    # case and death data
         cases_week = cases_avg*7,
         cases_week_100k = cases_avg_per_100k*7,
         deaths_week = deaths_avg*7,
         deaths_week_100k = deaths_avg_per_100k*7,
         deaths_week_07_days_ago = deaths_07_days_ago*7,
         deaths_week_in_21d = deaths_21_lag*7,
         cases_week_21d_ago = cases_lag_21*7,
         cases_week_28d_ago = cases_lag_28*7,
         cases_week_07d_ago = cases_lag_07*7,
         cases_week_14d_ago = cases_lag_14*7,
         cases_change_14d_ago = cases_lag_14*7-cases_week,
         cases_change_07d_ago = cases_lag_07*7-cases_week,
    
    # log stuff 
        l_cases_week = log(cases_week),
        l_deaths_week = log(deaths_week),
        l_cases_change_14d_ago = log(cases_lag_14*7)-l_cases_week,
        l_cases_change_07d_ago = log(cases_lag_07*7)-l_cases_week,
         
    # hospitalizations
         admits_week = admits_confirmed_avg*7,
         admits_week_07d_ago = admits_7d_ago*7,
         admits_change_07d_ago = admits_7d_ago*7-admits_week,
         admits_21d_ago = admits_21d_ago*7,
         l_admits_week = log(admits_week),
         l_admits_change_07d_ago = log(admits_7d_ago*7)-log(l_admits_week),
         admits_week_100k = admits_confirmed_100K*7,
    
    # ratios
         hosp_case = admits_week/cases_week,
         cfr_observed_today = deaths_week/cases_week_21d_ago,
         cfr_change_from_last_week = deaths_week_07_days_ago/cases_week_28d_ago - cfr_observed_today,
         cfr_in_21_days = deaths_week_in_21d/cases_week,
         hfr_in_21_days = deaths_week_in_21d/admits_week,
         hfr_observed_today = deaths_week/admits_21d_ago,
      
    # additional data
        pop = POPESTIMATE2019,
    
    ) %>%
  filter(ymd >= "2021-03-01")
    
# aggregate over regions
reg = d %>% mutate(
  region = case_when(REGION=="1"~"Northeast", REGION=="2"~"Midwest", REGION=="3"~"South", REGION=="4"~"West"),
  region = factor(region, levels = c("Northeast", "Midwest", "South", "West"))
) %>% group_by(region, ymd) %>%
  summarize(
    cfr_today = sum(deaths_week)/sum(cases_week_21d_ago)*100,
    cfr_in_21_days = sum(deaths_week_in_21d)/sum(cases_week)*100, 
    hfr_in_21_days = sum(deaths_week_in_21d)/sum(admits_week)*100,
    hfr_today = sum(deaths_week)/sum(admits_21d_ago)*100,
    hosp_case = sum(admits_week)/sum(cases_week)*100
  )

# aggregate over US
us = d %>% group_by(ymd) %>% 
  
  summarize(
    cfr_today = sum(deaths_week)/sum(cases_week_21d_ago)*100,
    cfr_change_from_last_week = sum(deaths_week_07_days_ago)/sum(cases_week_28d_ago) - cfr_today,
    cfr_in_21_days = sum(deaths_week_in_21d)/sum(cases_week)*100, 
    l_cfr_in_21_days = log(cfr_in_21_days),
    hfr_in_21_days = sum(deaths_week_in_21d)/sum(admits_week)*100,
    hfr_today = sum(deaths_week)/sum(admits_21d_ago)*100,
    hosp_case = sum(admits_week)/sum(cases_week)*100,
    cases_week = sum(cases_week), deaths_week = sum(deaths_week),
    deaths_week_in_21d = sum(deaths_week_in_21d),
    cases_change_07d_ago = sum(cases_change_07d_ago), 
    cases_change_14d_ago = sum(cases_change_14d_ago),
    l_cases_week = log(cases_week), l_deaths_week = log(deaths_week),
    l_deaths_week_in_21d = log(deaths_week_in_21d),
    l_cases_change_07d_ago = log(sum(cases_week_07d_ago))-l_cases_week,
    l_cases_change_14d_ago = log(sum(cases_week_14d_ago))-l_cases_week,
    admits_week = sum(admits_week), 
    admits_change_07d_ago = sum(admits_change_07d_ago),
    l_admits_week = log(admits_week), 
    l_admits_change_07d_ago = log(sum(admits_week_07d_ago))-l_admits_week) %>%
  mutate(hosp_case_21d_ago = lag(hosp_case,3),
         hosp_case_ratio = hosp_case_21d_ago/hosp_case,
         cfr_ratio = cfr_today/cfr_in_21_days,
         hfr_ratio = hfr_today/hfr_in_21_days)

# aggregate over US
s = d %>% group_by(ymd, state) %>% filter(ymd >= "2021-05-15") %>%
  
  summarize(
    cfr_today = sum(deaths_week)/sum(cases_week_21d_ago)*100,
    cfr_change_from_last_week = sum(deaths_week_07_days_ago)/sum(cases_week_28d_ago) - cfr_today,
    cfr_in_21_days = sum(deaths_week_in_21d)/sum(cases_week)*100, 
    l_cfr_in_21_days = log(cfr_in_21_days),
    hfr_in_21_days = sum(deaths_week_in_21d)/sum(admits_week)*100,
    hfr_today = sum(deaths_week)/sum(admits_21d_ago)*100,
    hosp_case = sum(admits_week)/sum(cases_week)*100,
    cases_week = sum(cases_week), deaths_week = sum(deaths_week),
    deaths_week_in_21d = sum(deaths_week_in_21d),
    cases_change_07d_ago = sum(cases_change_07d_ago), 
    cases_change_14d_ago = sum(cases_change_14d_ago),
    l_cases_week = log(cases_week), l_deaths_week = log(deaths_week),
    l_deaths_week_in_21d = log(deaths_week_in_21d),
    l_cases_change_07d_ago = log(sum(cases_week_07d_ago))-l_cases_week,
    l_cases_change_14d_ago = log(sum(cases_week_14d_ago))-l_cases_week,
    admits_week = sum(admits_week), 
    admits_change_07d_ago = sum(admits_change_07d_ago),
    l_admits_week = log(admits_week), 
    l_admits_change_07d_ago = log(sum(admits_week_07d_ago))-l_admits_week) %>%
  mutate(hosp_case_21d_ago = lag(hosp_case,3),
         hosp_case_ratio = hosp_case_21d_ago/hosp_case,
         cfr_ratio = cfr_today/cfr_in_21_days,
         hfr_ratio = hfr_today/hfr_in_21_days)

# combine data frames
c = d %>% dplyr::select(ymd, hosp_case, cfr_in_21_days, hfr_in_21_days, state) %>% mutate(id = "state") %>%
  bind_rows(reg %>% dplyr::select(ymd, hosp_case, cfr_in_21_days, hfr_in_21_days) %>% mutate(state = region, id = "regional")) %>%
  bind_rows(us %>% dplyr::select(ymd, hosp_case, cfr_in_21_days, hfr_in_21_days) %>% mutate(state = "National", id = "national"))
    

####*************************** PLOTS *******************************####
cols = c("black", brewer.pal(4, "Set2"))
c_plot = c %>% gather(var, value, hosp_case, cfr_in_21_days, hfr_in_21_days) %>%
  mutate(var = factor(var, levels = c("hosp_case", "cfr_in_21_days", "hfr_in_21_days")),
         state = factor(state, levels = c("National", "Northeast", "Midwest", "South", "West")),
         var2 = ifelse(var=="hosp_case", "New admission:New case ratio", "21d-lagged CFR (%)"),
         var2 = ifelse(var=="hfr_in_21_days", "21d-lagged HFR (%)", var2),
         var2 = factor(var2, levels = c( "New admission:New case ratio", "21d-lagged CFR (%)",  "21d-lagged HFR (%)")),
         lty = state=="National") %>%
  filter(id!="state" & ymd >= "2021-06-01")

ggplot(c_plot,
       aes(x = ymd, y = value, group = state, col = state, alpha = lty)) + geom_line() + 
  ylim(0, NA) + 
  geom_line(data = c_plot %>% filter(state=="National"), aes(x = ymd, y = value), lwd = .5) + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        strip.background = element_blank(),
        plot.title = element_text(face = "bold")) + 
  scale_color_manual(name = "", values = cols, labels = c("National", "Northeast", "Midwest", "South", "West")) + 
  #scale_linetype_manual(name = "", values = c(1,2,2,2,2), labels = c("National", "Northeast", "Midwest", "South", "West")) + 
  scale_alpha_manual(guide = F, values = c(.6, 1)) + 
  facet_grid(var2~., scales = "free") + labs(x = "", y = "") + 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") 


ggsave(here("2 - Figures", "cfr_hfr.png"), plot = last_plot(), width = 8, height = 8)

write.csv(c_plot, file = here("2 - Figures", "cfr_file.csv"))
