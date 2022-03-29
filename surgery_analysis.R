# 15 year surgery risk analysis

# packages
library(tidyverse)
library(here)

#### use dataprep.R to clean and import data ####
# factor levels are not preserved when read/writing csv's

#### results paragraph 1 -- time at risk and 3 events ####
data %>% filter(status == 1) %>%
  summarise(n=sum(!is.na(studyno)),
            n_cat=sum(cataract==1),
            yrs=sum(censortimeou)/365,
            rate=n_cat/yrs *100)

# poisson model with time at risk to get rate + 95%CIs
ggplot(data=data, aes(x=cataract)) + geom_histogram()
rate.m <- glm(cataract ~ offset(log(censortimeou/365)), family = "poisson", data = data)
rate.m %>% broom::tidy(., conf.int=T, exp = T)
0.0108*100 # estimate
0.00872*100 # low
0.0132*100 # high


#### table 2 -- univariable regression ####
library(survival)
library(survminer)

# base model
base <- survfit(Surv(censortimeou, cataract) ~ 1, data=data) # 
summary(base, times = 15*365.25) 

# iterating through variables
coxph(Surv(censortimeou, cataract)~sex1, data=data) %>% 
  broom::tidy(., conf.int=T, exp=T) %>% 
  select(term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits = 1),
         conf.low=round(conf.low, digits=1),
         conf.high=round(conf.high, digits = 1)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(estci, estimate, ci, sep = " ") %>%
  write_csv(., here("tables", "table2", "coxsex.csv"))
  
# p-value 
coxph(Surv(censortimeou, cataract)~sex1, data=data)

# #### table 3 -- assessing for confounders ####
# # base model
# coxph(Surv(censortimeou, cataract) ~ fuel.f, data=data) %>%
#   broom::tidy(., conf.int=T, exp=T)
# 
# # iterating through the variables
# coxph(Surv(censortimeou, cataract) ~ fuel.f + sphere.4f, data=data) %>% 
#   broom::tidy(., conf.int=T, exp=T) %>%
#   filter(term %in% c("fuel.fpropane","fuel.fwood")) %>%
#   select(term, estimate, conf.low, conf.high) %>%
#   mutate(estimate=round(estimate, digits = 1),
#          conf.low=round(conf.low, digits=1),
#          conf.high=round(conf.high, digits = 1)) %>%
#   unite(ci, conf.low, conf.high, sep = " to ") %>%
#   mutate(ci=paste0("(",ci,")")) %>%
#   unite(estci, estimate, ci, sep = " ") %>%
#   write_csv(., here("tables","table3","coxref.csv"))

#### table 4 -- final model ####  
cox.final <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + # agecat
                     sex1 + ses.f + occu.f + 
                     kitchen.f + packall.f + # smoke.2f
                     potobac.f + uv_yrs.f + # uv_yrs.f
                     sphere.2f + bcva.3f,  # sphere.4f
                     # nucop + cor + ps,
                     # + bmicat
                   data = filter(data, !is.na(ses.f)))

# testing for departure from linearity for age
class(data$uv_yrs.of)
class(data$agecat.of)

coxph(Surv(censortimeou, cataract) ~ fuel.f + agecat + sex1 + ses.f + occu.f +  kitchen.f + packall.f + 
        potobac.f + uv_yrs.of + sphere.2f + bcva.3f, 
      data = filter(data, !is.na(ses.f))) %>%
  broom::tidy(., conf.int=T) # %>% view(.)

# # JK try at orthogonal polynomial; first create the ordered factor
# imputed.35.clean.jk <- imputed.35.clean %>%
#   mutate(ses5.of=factor(ses5.f, ordered=TRUE, levels=c("low", "medlow", "med", "medhigh", "high")),
#          ses3.of=factor(ses3.f, ordered = T, levels=c("low","medium","high")))
# # Then simply use the ordered factor instead
# moh.vi.ses5f.imp.adj.jk <- glmer(moh.vi ~ ses5.of + age + sex.bin + (1|community), family = binomial(link = logit), data = imputed.35.clean.jk)
# summary(moh.vi.ses5f.imp.adj.jk) # The L/Q/C refer to linear, quadratic, cubic polynomials. Generally just look at linear
# # # Another way:
# # library(emmeans) # vague memory is that it’s an easy way to get effects at different time points when you have a time by treatment interaction, but I also used once to do the orthogonal polynomials
# # moh.vi.ses5f.imp.adj.emmeans <- emmeans(moh.vi.ses5f.imp.adj,"ses5.f")
# # moh.vi.ses5f.imp.adj.emmeans.contrast <- contrast(moh.vi.ses5f.imp.adj.emmeans, "poly")
# # # Note the two ways give the same answer:
# # moh.vi.ses5f.imp.adj.emmeans.contrast # this second way
# # summary(moh.vi.ses5f.imp.adj.jk) # the way we did above. I find the ordered factor a little easier.
# # # I forget why I did it with this whole emmeans thing before
# 
# # What about just looking at whether there is any difference between categories (as opposed to a linear effect)
# # JN note I think this is an omnibus test
# # Easiest way I know is to make 2 models, one with, one without the categorical variable of interest
# # Then do a likelihood ratio test comparing the 2 models
# moh.vi.ses5f.imp.adj.jk.withses <- glmer(moh.vi ~ ses5.f + age + sex.bin + (1|community), family = binomial(link = logit), data = imputed.35.clean.jk)
# moh.vi.ses5f.imp.adj.jk.withoutses <- glmer(moh.vi ~ age + sex.bin + (1|community), family = binomial(link = logit), data = imputed.35.clean.jk)
# # Make sure the same number of observations are in each model—because the anova command below assumes the models are nested, meaning they are exactly the same with exactly the same number of observations, except one of the models doesn’t have the SES variable. (Note that if SES had a couple missing values, then the number of observations would be different in the 2 models and they would no longer be nested)
# summary(moh.vi.ses5f.imp.adj.jk.withses)
# summary(moh.vi.ses5f.imp.adj.jk.withoutses)
# # Anova for the likelihood ratio test
# anova(moh.vi.ses5f.imp.adj.jk.withses, moh.vi.ses5f.imp.adj.jk.withoutses)
# # Note that P=0.18. So the orthogonal polynomial is different.

# broom::tidy(cox.final, conf.int=T, exp=T) %>% view()

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
cox.without <- coxph(Surv(censortimeou, cataract) ~ age_house + sex1 + ses.f + occu.f +  # fuel.f +
                       kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f + bcva.3f, 
                     data = filter(data, !is.na(ses.f)))

lmtest::lrtest(cox.without, cox.final)

#### testing PH assumption ####
ph_data <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + 
          sex1 + ses.f + occu.f + 
          kitchen.f + packall.f + 
          potobac.f + uv_yrs.f + 
          sphere.2f + bcva.3f,
        data = filter(data, !is.na(ses.f))) %>% 
  cox.zph(.)
ph_data

ph.plot <- ggcoxzph(ph_data)
ph.plot

#### final model with sex interaction term ####
cox.sex <- coxph(Surv(censortimeou, cataract) ~ fuel.f*sex1 + age_house + ses.f + occu.f +  # 
        kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f + bcva.3f, 
      data = filter(data, !is.na(ses.f))) 

summary(cox.sex) 
# Concordance= 0.799  (se = 0.021 )
# Likelihood ratio test= 103.3  on 25 df,   p=2e-11
# Wald test            = 94.71  on 25 df,   p=5e-10
# Score (logrank) test = 116.5  on 25 df,   p=9e-14
summary(cox.final)
# Concordance= 0.797  (se = 0.021 )
# Likelihood ratio test= 102.1  on 23 df,   p=6e-12
# Wald test            = 95.16  on 23 df,   p=1e-10
# Score (logrank) test = 114.6  on 23 df,   p=4e-14

cox.sex # LRT 103.3
cox.final # LRT 102.1

# AIC
extractAIC(cox.sex) # 25.000 1004.759
extractAIC(cox.final) # 23.000 1001.954

# interpretation: https://www.scribbr.com/statistics/akaike-information-criterion/#:~:text=The%20AIC%20function%20is%202K,it%20is%20being%20compared%20to.
# final model has smaller AIC so better fit
# also the difference is >2 so fit is significantly better

# lrt comparing
lmtest::lrtest(cox.final, cox.sex) # p-value 0.5504


#### final model stratified by sex ####
# cox.f <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + ses.f + occu.f +  # 
#                        kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f + bcva.3f, 
#                      data = filter(data, !is.na(ses.f) & sex1 == "Female"))
# 
# broom::tidy(cox.f, conf.int=T, exp=T) %>% 
#   select(term, estimate, conf.low, conf.high) %>%
#   filter(term %in% c("fuel.fkerosene", "fuel.fpropane")) %>%
#   mutate(estimate=round(estimate, digits = 1),
#          conf.low=round(conf.low, digits=1),
#          conf.high=round(conf.high, digits = 1)) %>%
#   unite(ci, conf.low, conf.high, sep = " to ") %>%
#   mutate(ci=paste0("(",ci,")")) %>%
#   unite(estci, estimate, ci, sep = " ") %>%
#   write_csv(., here("tables","sex_stratified", "coxfemale.csv"))
# 
# cox.m <- coxph(Surv(censortimeou, cataract) ~ fuel.f + age_house + ses.f + occu.f +  # 
#                  kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f + bcva.3f, 
#                data = filter(data, !is.na(ses.f) & sex1 == "Male"))
# 
# broom::tidy(cox.m, conf.int=T, exp=T) %>% 
#   select(term, estimate, conf.low, conf.high) %>%
#   filter(term %in% c("fuel.fkerosene", "fuel.fpropane")) %>%
#   mutate(estimate=round(estimate, digits = 1),
#          conf.low=round(conf.low, digits=1),
#          conf.high=round(conf.high, digits = 1)) %>%
#   unite(ci, conf.low, conf.high, sep = " to ") %>%
#   mutate(ci=paste0("(",ci,")")) %>%
#   unite(estci, estimate, ci, sep = " ") %>%
#   write_csv(., here("tables","sex_stratified", "coxmale.csv"))

#### sensitivity analyses -- only those with complete follow up ####
m.sens <- coxph(Surv(censortimeou, cataract) ~ fuel.f + sex1 + age_house + ses.f + occu.f +  # 
                  kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f + bcva.3f, 
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
m.senswo <- coxph(Surv(censortimeou, cataract) ~ fuel.f + sex1 + age_house + occu.f + ses.f +  # + bcva.3f
                    kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f , 
                  data = filter(data, status==1))

lmtest::lrtest(m.senswo, m.sens)

  
# #### figure 2 -- results from cox regression ####
# 
# ref <- data.frame(parameter = c("Wood",  # cooking fuel
#                                 "Female", # sex
#                                 "Lowest", # SES
#                                 "No", # kitchen
#                                 "Agriculture", # Occupation
#                                 "Never", # Smoking status
#                                 "None", # Snuff / betel
#                                 "1st quantile", # sun exposure
#                                 "No error", # ref error
#                                 "20/20"),
#                   estimate = rep(1, times = 10),
#                   conf.low = rep(1, times = 10),
#                   conf.high = rep(1, times = 10),
#                   var = c("Cooking fuel",
#                           "Sex",
#                           "SES",
#                           "Kitchen",
#                           "Occupation",
#                           "Smoking",
#                           "Snuff / betel use",
#                           "Sun exposure",
#                           "Refractive error",
#                           "BCVA"))
# 
# figdata <- broom::tidy(cox.final, conf.int=T, exp=T) %>% # START HERE TOMORROW
#   select(term, estimate, conf.low, conf.high) %>%
#   rename(parameter = term) %>%
#   mutate(var=factor(case_when(grepl("age_house", parameter)~"Age",
#                        grepl("occu.f", parameter)~"Occupation",
#                        grepl("fuel.f", parameter)~"Cooking fuel",
#                        grepl("kitchen.f", parameter)~"Kitchen",
#                        grepl("packall.f", parameter)~"Smoking",
#                        grepl("potobac", parameter)~"Snuff / betel use",
#                        grepl("ses.f", parameter)~"SES",
#                        grepl("sex", parameter)~"Sex",
#                        grepl("sphere.2f", parameter)~"Refractive error",
#                        grepl("uv_yrs.f", parameter)~"Sun exposure",
#                        grepl("bcva.3f", parameter) ~ "BCVA"),
#                     levels = c("Cooking fuel","Age","Sex","SES","Occupation","Kitchen","Smoking","Snuff / betel use",
#                                "Sun exposure","Refractive error","BCVA")),
#          parameter=factor(case_when(parameter == "age_house" ~ "Age",
#                                     parameter == "occu.funemployed" ~ "Unemployed",
#                                     parameter == "occu.fother" ~ "Other",
#                                     parameter == "fuel.fpropane" ~ "Propane",
#                                     parameter == "fuel.fkerosene" ~ "Kerosene",
#                                     parameter == "kitchen.fYes" ~ "Yes",
#                                     parameter == "packall.fHeavy" ~ "Heavy",
#                                     parameter == "packall.fLight" ~ "Light",
#                                     parameter == "potobac.fHigh" ~ "Highest",
#                                     parameter == "potobac.fLow" ~ "Low",
#                                     parameter == "potobac.fMed" ~ "Moderate",
#                                     parameter == "ses.f2" ~ "Lower-middle",
#                                     parameter == "ses.f3" ~ "Middle",
#                                     parameter == "ses.f4" ~ "Upper",
#                                     parameter == "sex1Male" ~ "Male",
#                                     parameter == "sphere.2fhyperopia" ~ "Hyperopia",
#                                     parameter == "sphere.2fmyopia" ~ "Myopia",
#                                     parameter == "bcva.3fbt20/20" ~ "Better than \n20/20",
#                                     parameter == "bcva.3fwt20/20" ~ "Worse than \n20/20",
#                                     parameter == "uv_yrs.fHigh" ~ "5th quantile",
#                                     parameter == "uv_yrs.fHigh-Med" ~ "4th quantile",
#                                     parameter == "uv_yrs.fLow-Med" ~ "2nd quantile",
#                                     parameter == "uv_yrs.fMed" ~ "3rd quantile"), 
#                           levels = c("Wood","Kerosene","Propane", # fuel
#                                      "Age", 
#                                      "Female", "Male", # sex
#                                      "Agriculture","Unemployed","Other", # Occupation
#                                      "No", "Yes", # kitchen
#                                      "Never","Light", "Heavy", # smoking
#                                      "None","Low", "Moderate","Highest", # potobac
#                                      "Lowest","Lower-middle", "Middle", "Upper", # SES
#                                      "1st quantile","2nd quantile","3rd quantile","4th quantile","5th quantile", # sun
#                                      "No error","Hyperopia", "Myopia", # ref error
#                                      "20/20", "Better than \n20/20", "Worse than \n20/20"))) %>% # visual acuity
#   rbind(., ref)
# 
# # actual figure
# fig2 <- figdata %>%
#   ggplot(aes(x=parameter, y=estimate, label = parameter)) +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
#                   alpha = 0.8,
#                   position = position_dodge2(width = 0.5,  
#                                              padding = 0.4)) +
#   geom_hline(yintercept = 1, linetype=3) + 
#   facet_grid(cols = vars(var), scales = "free_x") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 9, angle = 45, 
#                                    hjust = 0.8),
#         axis.text.y = element_text(size = 9),
#         axis.title = element_text(size = 9),
#         legend.text = element_text(size = 9),
#         legend.title = element_text(size = 9),
#         panel.border = element_rect(color = "black", 
#                                     size = 0.5,
#                                     linetype = 1),
#         panel.spacing.x = unit(0, "line"),
#         panel.grid.major.x = element_blank()) +
#   # changing the facet titles to be black and white
#   theme(strip.background = element_rect(fill = "white"),
#         strip.text.x = element_text(size = 9)) +
#   labs(y = "Relative Rate (95% CI)", x = "Variables")
# fig2
# 
# # saving
# ggsave(here("figures","fig2","fig2.eps"),
#        fig2,
#        device = cairo_ps,
#        height = 4.5, width = 14, units = "in",
#        dpi = 300)










