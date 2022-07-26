#************************ MAKE ALTERNATIVE INDICATOR PLOTS ********************#
#*                                                                            *#
#*                                                                            *#
#*                                                                            *#
#******************************************************************************#
                                                                      
#### SETUP ####
library(here)
source(here("global_options.R"))

#### FUNCTIONS ####
format_data = function(source){
  
  load(source)
  max_ymd = max(d_out_pre$ymd)
  df2 = d_out_pre %>% 
    gather(var, value, trigger_on2, trigger_on3, trigger_on4) %>%
    filter(value==1 & !is.na(value)) %>%
    mutate(ymd = as.Date(ymd, format = "%Y-%m-%d"),
           followtime = (max_ymd-ymd)/7,
           lagzeke = ifelse(time_to_zeke<15, time_to_zeke, NA),
           event_zeke = lagzeke <= followtime,
           event_zeke = ifelse(is.na(event_zeke), 0, event_zeke),
           stime_zeke = ifelse(lagzeke >= followtime | is.na(lagzeke), followtime, lagzeke))
  return(df2)
}


make_plots = function(df2, title2 = ""){
  
  # time to Zeke
  g2 = survfit(Surv(stime_zeke, event_zeke, type = "right")~var, type = "kaplan-meier", data = df2)
  a = ggsurvplot(fit = g2, xlab = "", ylab = "",  title = title2,
                 size = .3, censor.size = 0, xlim = c(.57, 12), legend = "bottom",
                 palette = c("#E59E00", "#56B4E9", "#009E73"), fun = "event", legend.title = "", break.x.by = 2,
                 font.title = 12, 
                 legend.labs = c("CDC High", "Alternative metric 1", "Alternative metric 2"))$plot + 
    geom_vline(xintercept = 3, lty = 2, col = "darkgrey") + 
    theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1)
  a
  
  return(a)
}

# counties
df1 = format_data(source = here("0 - Data", "county_time_data.RData"))

# states
df2 = format_data(source =  here("0 - Data", "state_time_data.RData"))

# combine plots

# remake with titles for limited figure
plots1a = make_plots(df1, title2 = "Counties")
plots2a = make_plots(df2, title2 = "States")

h = ggarrange(plots2a, plots1a, common.legend = T, legend = "bottom")
h

ggsave(here("2 - Figures", "survival_zeke.png"), width = 10, height = 4, units = "in")

