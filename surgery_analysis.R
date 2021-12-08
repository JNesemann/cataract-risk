# 15 year surgery risk analysis
# John Nesemann
# November 29 2021

# packages
library(tidyverse)
library(here)

#### importing and cleaning data ####
data <- read_csv(here("data","data_clean.csv"))

#### table 2 -- univariable regression ####
library(survival)
library(survminer)

# base model
base <- survfit(Surv(censortimeou, cataract) ~ 1, data=data) # %>% broom::tidy(., conf.int=T)
summary(base, times = 15*365.25) 
survfit(Surv(censortimeou, cataract) ~ 1, data = data)

# iterating through variables
coxph(Surv(censortimeou, cataract)~fuel.f, data=data) %>% 
  broom::tidy(., conf.int=T, exp=T) %>% 
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables", "table2", "coxunadj.csv"))
  
# p-value 
coxph(Surv(censortimeou, cataract)~fuel.f, data=data)

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
                   data = data)

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

# obtaining p-values through LRT
cox.without <- coxph(Surv(censortimeou, cataract) ~ fuel.f + agecat + sex1 + ses.f + 
                       occu.f + kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.4f,
                     # + bmicat     map.3f + 
                     data = filter(data, !is.na(map.3f)))

lmtest::lrtest(cox.without, cox.final)

#### sensitivity analyses -- only those with complete follow up ####
m.sens <- coxph(Surv(censortimeou, cataract) ~ fuel.f + agecat + sex1 + ses.f + occu.f + 
                  map.3f + kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.4f,
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
m.senswo <- coxph(Surv(censortimeou, cataract) ~ fuel.f + agecat + sex1 + ses.f + occu.f + 
                    map.3f + kitchen.f + packall.f + potobac.f + uv_yrs.f,
                  data = filter(data, status==1 & !is.na(sphere.4f))) #   + sphere.4f

lmtest::lrtest(m.senswo, m.sens)

  
#### figure 2 -- results from cox regression ####

ref <- data.frame(parameter = c("35-39","35-39","35-39",
                                "Agriculture","Agriculture","Agriculture",
                                "Optimal","Optimal","Optimal",
                                "Kerosene","Kerosene","Kerosene",
                                "No","No","No",
                                "Never","Never","Never",
                                "None","None","None",
                                "Lowest","Lowest","Lowest",
                                "Female","Female","Female",
                                "No error","No error","No error",
                                "1st quantile","1st quantile","1st quantile"),
                  estimate = rep(c(1,1,1), times = 11),
                  conf.low = rep(c(1,1,1), times = 11),
                  conf.high = rep(c(1,1,1), times = 11),
                  var = c("Age","Age","Age",
                          "Occupation","Occupation","Occupation",
                          "MAP","MAP","MAP",
                          "Fuel","Fuel","Fuel",
                          "Kitchen","Kitchen","Kitchen",
                          "Smoking","Smoking","Smoking",
                          "Snuff / betel use","Snuff / betel use","Snuff / betel use",
                          "SES","SES","SES",
                          "Sex","Sex","Sex",
                          "Refractive error","Refractive error","Refractive error",
                          "Sun exposure","Sun exposure","Sun exposure"))

figdata <- broom::tidy(cox.final, conf.int=T, exp=T) %>%
  select(term, estimate, conf.low, conf.high) %>%
  rename(parameter = term) %>%
  mutate(var=case_when(grepl("agecat", parameter)~"Age",
                       grepl("occu.f", parameter)~"Occupation",
                       grepl("map.3f", parameter)~"MAP",
                       grepl("fuel.f", parameter)~"Fuel",
                       grepl("kitchen.f", parameter)~"Kitchen",
                       grepl("packall.f", parameter)~"Smoking",
                       grepl("potobac", parameter)~"Snuff / betel use",
                       grepl("ses.f", parameter)~"SES",
                       grepl("sex", parameter)~"Sex",
                       grepl("sphere.4f", parameter)~"Refractive error",
                       grepl("uv_yrs", parameter)~"Sun exposure"),
         parameter=factor(case_when(parameter == "agecat40-44" ~ "40-44",
                                    parameter == "agecat45-49" ~ "45-49",
                                    parameter == "agecat50+" ~ "50+",
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
                                    parameter == "sphere.4fhyperopia" ~ "Hyperopia",
                                    parameter == "sphere.4flow myopia" ~ "Low myopia",
                                    parameter == "sphere.4fmod-high myopia" ~ "Moderate-\nhigh myopia",
                                    parameter == "uv_yrs.fHigh" ~ "5th quantile",
                                    parameter == "uv_yrs.fHigh-Med" ~ "4th quantile",
                                    parameter == "uv_yrs.fLow-Med" ~ "2nd quantile",
                                    parameter == "uv_yrs.fMed" ~ "3rd quantile"), 
                          levels = c("35-39","40-44", "45-49", "50+", "Female", "Male", # age and sex
                                     "Agriculture","Unemployed","Other", # Occupation
                                     "Optimal", "Normal", "High", # MAP
                                     "Kerosene","Propane", "Wood", # fuel
                                     "No", "Yes", # kitchen
                                     "Never","Light", "Heavy", # smoking
                                     "None","Low", "Moderate","Highest", # potobac
                                     "Lowest","Lower-middle", "Middle", "Upper", # SES
                                     "1st quantile","2nd quantile", "3rd quantile", "4th quantile","5th quantile", # sun
                                     "No error","Hyperopia", "Low myopia", "Moderate-\nhigh myopia"))) %>% # ref error
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
  labs(y = "Relative Risk (95% CI)", x = "Variables")
fig2

# saving
ggsave(here("figures","fig2","fig2.eps"),
       fig2,
       device = cairo_ps,
       height = 4.5, width = 14, units = "in",
       dpi = 300)










