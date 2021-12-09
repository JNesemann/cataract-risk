# 15 year surgery risk analysis
# John Nesemann
# November 29 2021

# packages
library(tidyverse)
library(here)

#### use dataprep.R to clean and import data ####
# factor levels are not preserved when read/writing csv's

#### table 2 -- univariable regression ####
library(survival)
library(survminer)

# base model
base <- survfit(Surv(censortimeou, cataract) ~ 1, data=data) # %>% broom::tidy(., conf.int=T)
summary(base, times = 15*365.25) 
survfit(Surv(censortimeou, cataract) ~ 1, data = data)

# iterating through variables
coxph(Surv(censortimeou, cataract)~sphere.2f, data=data) %>% 
  broom::tidy(., conf.int=T, exp=T) %>% 
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables", "table2", "coxref.csv"))
  
# p-value 
coxph(Surv(censortimeou, cataract)~sphere.2f, data=data)

#### table 3 -- assessing for confounders ####
# base model
coxph(Surv(censortimeou, cataract) ~ fuel.f, data=data) %>%
  broom::tidy(., conf.int=T, exp=T)

# iterating through the variables
coxph(Surv(censortimeou, cataract) ~ fuel.f + sphere.4f, data=data) %>% 
  broom::tidy(., conf.int=T, exp=T) %>%
  filter(term %in% c("fuel.fpropane","fuel.fwood")) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables","table3","coxref.csv"))

#### table 4 -- final model ####  
cox.final <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + # agecat
                     sex1 + ses.f + occu.f + 
                     map.3f + kitchen.f + packall.f + # smoke.2f
                     potobac.f + uv_yrs + # uv_yrs.f
                     sphere.2f +  # sphere.4f
                      nucop + cor + ps,
                     # + bmicat
                   data = filter(data, !is.na(ses.f)))

broom::tidy(cox.final, conf.int=T, exp=T) %>% view()

# saving the model
broom::tidy(cox.final, conf.int=T, exp=T) %>% 
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables","table4", "coxfinal.csv"))

#### NOTE TO SELF NEED TO TEST FOR DEPARTURE FROM LINEARITY

# obtaining p-values through LRT
cox.without <-  coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + # agecat
                        #  
                        sex1 + occu.f + ses.f +
                        map.3f + kitchen.f + packall.f + # smoke.2f
                        potobac.f +  + # uv_yrs.f
                        sphere.2f +  # sphere.4f
                        nucop + cor + ps,
                      # + bmicat
                      data = filter(data, !is.na(ses)))

lmtest::lrtest(cox.without, cox.final)

#### sensitivity analyses -- only those with complete follow up ####
m.sens <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + # agecat
                  sex1 + ses.f + occu.f + 
                  map.3f + kitchen.f + packall.f + potobac.f + uv_yrs + #  uv_yrs.f
                  sphere.2f +
                  nucop + cor + ps,
                data = filter(data, status==1))
summary(m.sens)

# saving the model
m.sens %>% broom::tidy(., conf.int=T, exp=T) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables","coxsens", "coxsens.csv"))

# p-values
m.senswo <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + # agecat
                    # uv_yrs + #  uv_yrs.f
                    sex1 + ses.f + occu.f + 
                    map.3f + kitchen.f + packall.f + potobac.f + 
                    sphere.2f +
                    nucop + cor + ps,
                  data = filter(data, status==1))

lmtest::lrtest(m.senswo, m.sens)

  
#### figure 2 -- results from cox regression ####

ref <- data.frame(parameter = c(#"35-39","35-39","35-39",
                                "Agriculture","Agriculture","Agriculture",
                                "Optimal","Optimal","Optimal",
                                "Kerosene","Kerosene","Kerosene",
                                "No","No","No",
                                "Never","Never","Never",
                                "None","None","None",
                                "Lowest","Lowest","Lowest",
                                "Female","Female","Female",
                                "No error","No error","No error"
                                #"1st quantile","1st quantile","1st quantile"
                                ),
                  estimate = rep(c(1,1,1), times = 9),
                  conf.low = rep(c(1,1,1), times = 9),
                  conf.high = rep(c(1,1,1), times = 9),
                  var = c(# "Age","Age","Age",
                          "Occupation","Occupation","Occupation",
                          "MAP","MAP","MAP",
                          "Fuel","Fuel","Fuel",
                          "Kitchen","Kitchen","Kitchen",
                          "Smoking","Smoking","Smoking",
                          "Snuff / betel use","Snuff / betel use","Snuff / betel use",
                          "SES","SES","SES",
                          "Sex","Sex","Sex",
                          "Refractive error","Refractive error","Refractive error"
                          # "Sun exposure","Sun exposure","Sun exposure"
                          ))

figdata <- broom::tidy(cox.final, conf.int=T, exp=T) %>%
  select(term, estimate, conf.low, conf.high) %>%
  rename(parameter = term) %>%
  mutate(var=factor(case_when(grepl("age_house", parameter)~"Age",
                       grepl("occu.f", parameter)~"Occupation",
                       grepl("map.3f", parameter)~"MAP",
                       grepl("fuel.f", parameter)~"Fuel",
                       grepl("kitchen.f", parameter)~"Kitchen",
                       grepl("packall.f", parameter)~"Smoking",
                       grepl("potobac", parameter)~"Snuff / betel use",
                       grepl("ses.f", parameter)~"SES",
                       grepl("sex", parameter)~"Sex",
                       grepl("sphere.2f", parameter)~"Refractive error",
                       grepl("uv_yrs", parameter)~"Sun exposure",
                       grepl("nucop", parameter) ~ "Cataract severity",
                       grepl("cor", parameter) ~ "Cataract severity",
                       grepl("ps", parameter) ~ "Cataract severity"),
                    levels = c("Fuel", "Age","Sex","SES","Occupation","Kitchen","Smoking","Snuff / betel use",
                               "MAP","Sun exposure","Refractive error","Cataract severity")),
         parameter=factor(case_when(parameter == "age_house" ~ "Age",
                                    # parameter == "agecat45-49" ~ "45-49",
                                    # parameter == "agecat50+" ~ "50+",
                                    parameter == "occu.funemployed" ~ "Unemployed",
                                    parameter == "occu.fother" ~ "Other",
                                    parameter == "map.3fnormal" ~ "Normal",
                                    parameter == "map.3fhigh" ~ "High",
                                    parameter == "fuel.fpropane" ~ "Propane",
                                    parameter == "fuel.fwood" ~ "Wood",
                                    parameter == "kitchen.fYes" ~ "Yes",
                                    parameter == "packall.fHeavy" ~ "Heavy",
                                    parameter == "packall.fLight" ~ "Light",
                                    parameter == "potobac.fHigh" ~ "Highest",
                                    parameter == "potobac.fLow" ~ "Low",
                                    parameter == "potobac.fMed" ~ "Moderate",
                                    parameter == "ses.f2" ~ "Lower-middle",
                                    parameter == "ses.f3" ~ "Middle",
                                    parameter == "ses.f4" ~ "Upper",
                                    parameter == "sex1Male" ~ "Male",
                                    parameter == "sphere.2fhyperopia" ~ "Hyperopia",
                                    parameter == "sphere.2fmyopia" ~ "Myopia",
                                    # parameter == "sphere.2fmod-high myopia" ~ "Moderate-\nhigh myopia",
                                    parameter == "uv_yrs" ~ "Sun exposure",
                                    # parameter == "uv_yrs.fHigh-Med" ~ "4th quantile",
                                    # parameter == "uv_yrs.fLow-Med" ~ "2nd quantile",
                                    # parameter == "uv_yrs.fMed" ~ "3rd quantile"
                                    parameter == "nucop" ~ "Nuclear",
                                    parameter == "cor" ~ "Cortical",
                                    parameter == "ps" ~ "Subcapsular"), 
                          levels = c("Kerosene","Propane", "Wood", # fuel
                                     "Age", 
                                     "Female", "Male", # sex
                                     "Agriculture","Unemployed","Other", # Occupation
                                     "Optimal", "Normal", "High", # MAP
                                     "No", "Yes", # kitchen
                                     "Never","Light", "Heavy", # smoking
                                     "None","Low", "Moderate","Highest", # potobac
                                     "Lowest","Lower-middle", "Middle", "Upper", # SES
                                     "Sun exposure", # sun
                                     "No error","Hyperopia", "Myopia", # ref error
                                     "Nuclear", "Cortical", "Subcapsular"))) %>% # baseline cat severity 
  rbind(., ref)

# actual figure
fig2 <- figdata %>%
  ggplot(aes(x=parameter, y=estimate, label = parameter)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  alpha = 0.8,
                  position = position_dodge2(width = 0.5,  
                                             padding = 0.4)) +
  geom_hline(yintercept = 1, linetype=3) + 
  facet_grid(cols = vars(var), scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45, 
                                   hjust = 0.8),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        panel.border = element_rect(color = "black", 
                                    size = 0.5,
                                    linetype = 1),
        panel.spacing.x = unit(0, "line"),
        panel.grid.major.x = element_blank()) +
  # changing the facet titles to be black and white
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 9)) +
  labs(y = "Relative Rate (95% CI)", x = "Variables")
fig2

# saving
ggsave(here("figures","fig2","fig2.eps"),
       fig2,
       device = cairo_ps,
       height = 4.5, width = 14, units = "in",
       dpi = 300)










