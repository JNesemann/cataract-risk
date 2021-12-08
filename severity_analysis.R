# Risk factors for baseline cataract severity
# John Nesemann

# packages
library(here)
library(tidyverse)

# telling R not to write in scientific notation
options(scipen = 999)

#### use dataprep.R to clean and import data ####
# factor levels are not preserved when read/writing csv's

#### calculating power ####
library(pwr)

# baseline analysis
data %>% 
  group_by(fuel.bin.f) %>%
  summarise(n=sum(!is.na(studyno)),
            mean=mean(mean_nucop_od),
            sd=sd(mean_nucop_od))

pwr.t2n.test(n1=677, n2=60, sig.level = 0.05, power = 0.8)

# 15 year analysis
data %>% filter(status==1) %>%
  group_by(fuel.bin.f) %>%
  summarize(n=sum(!is.na(studyno)))

pwr.2p2n.test(n1=515, n2=41, sig.level = 0.05, power = 0.8)

#### table 1 ####

# creating functions
per <- function(x) {paste0("(",x,"%)")}
p <- function(x,total) {round(x/total*100,digits = 2)}
par <- function(x) {paste0("(",x,")")}
par(5)
round2 <- function(x) {round(x,digits=2)}
round2(0.3555)

# baseline
table1.bl <- data %>%
  summarise(total=sum(!is.na(age_house)),
            n_age_35.39=sum(agecat=="35-39"), p_age_35.39=p(n_age_35.39, total),
            n_age_40.44=sum(agecat=="40-44"), p_age_40.44=p(n_age_40.44, total),
            n_age_45.49=sum(agecat=="45-49"), p_age_45.49=p(n_age_45.49, total),
            n_age_50plus=sum(agecat=="50+"), p_age_50plus=p(n_age_50plus, total),
            n_sex_Male=sum(sex1=="Male"), p_sex_Male=p(n_sex_Male,total),
            n_sex_Female=sum(sex1=="Female"), p_sex_Female=p(n_sex_Female,total),
            n_ses_Poor=sum(ses.f=="1", na.rm=T), p_ses_Poor=p(n_ses_Poor,total),
            n_ses_lowmid=sum(ses.f=="2",na.rm = T), p_ses_lowmid=p(n_ses_lowmid,total),
            n_ses_Middle=sum(ses.f=="3", na.rm=T), p_ses_Middle=p(n_ses_Middle,total),
            n_ses_Upper=sum(ses.f=="4", na.rm = T), p_ses_Upper=p(n_ses_Upper,total),
            n_ses_NA=sum(is.na(ses.f)), p_ses_NA=p(n_ses_NA,total),
            n_educ_Illiterate=sum(educ.f=="illiterate", na.rm = T), p_educ_Illiterate=p(n_educ_Illiterate,total),
            n_educ_Primary=sum(educ.f=="primary", na.rm = T), p_educ_Primary=p(n_educ_Primary,total),
            n_educ_Middle=sum(educ.f=="middle",na.rm=T), p_educ_Middle=p(n_educ_Middle,total),
            n_educ_Secondary=sum(educ.f=="secondary", na.rm=T), p_educ_Secondary=p(n_educ_Secondary,total),
            n_educ_NA=sum(is.na(educ.f)), p_educ_NA=p(n_educ_NA,total),
            n_work_Agriculture=sum(occu.f=="agriculture", na.rm=T), p_work_Agriculture=p(n_work_Agriculture,total),
            n_work_Unemployed=sum(occu.f=="unemployed", na.rm=T), p_work_Unemployed=p(n_work_Unemployed,total),
            n_work_Other=sum(occu.f=="other",na.rm=T), p_work_Other=p(n_work_Other,total),
            n_work_NA=sum(is.na(occu.f)), p_work_NA=p(n_work_NA,total),
            # xtabs(data=data,~bmicat,addNA=T)
            n_bmi_under=sum(bmicat=="underweight"), p_bmi_under=p(n_bmi_under,total),
            n_bmi_norm=sum(bmicat=="normal"), p_bmi_norm=p(n_bmi_norm,total),
            n_bmi_over=sum(bmicat=="overweight"), p_bmi_over=p(n_bmi_over,total),
            n_bmi_obese=sum(bmicat=="obese"), p_bmi_obese=p(n_bmi_obese,total),
            # xtabs(data=data,~map.3f,addNA=T)
            n_map_optimal=sum(map.3f=="optimal"), p_map_optimal=p(n_map_optimal,total),
            n_map_norm=sum(map.3f=="normal"), p_map_norm=p(n_map_norm,total),
            n_map_hnorm=sum(map.3f=="high"), p_map_hnorm=p(n_map_hnorm,total),
            # n_map_htn=sum(map.f=="hypertension"), p_map_htn=p(n_map_htn,total),
            # n_worksun_Yes=sum(worksun==1),p_worksun_Yes=p(n_worksun_Yes,total),
            # n_worksun_No=sum(worksun==8), p_worksun_No=p(n_worksun_No,total),
            n_kitchen_Yes=sum(kitchen.f=="Yes", na.rm=T), p_kitchen_Yes=p(n_kitchen_Yes,total),
            n_kitchen_No=sum(kitchen.f=="No", na.rm=T), p_kitchen_No=p(n_kitchen_No,total),
            n_kitchen_NA=sum(is.na(kitchen.f)), p_kitchen_NA=p(n_kitchen_NA,total),
            # xtabs(data=data, ~fuel.f, addNA=T)
            n_fuel_kerosene=sum(fuel.f=="kerosene", na.rm=T), p_fuel_kerosene=p(n_fuel_kerosene,total),
            n_fuel_propane=sum(fuel.f=="propane", na.rm=T), p_fuel_propane=p(n_fuel_propane, total),
            n_fuel_wood=sum(fuel.f=="wood", na.rm=T), p_fuel_wood=p(n_fuel_wood,total),
            n_fuel_NA=sum(is.na(fuel.f)), p_fuel_NA=p(n_fuel_NA,total),
            # n_fuelexp_High=sum(exp.fuel.f=="High", na.rm=T), p_fuelexp_High=p(n_fuelexp_High,total),
            # n_fuelexp_Med=sum(exp.fuel.f=="Med", na.rm=T), p_fuelexp_Med=p(n_fuelexp_Med,total),
            # n_fuelexp_Low=sum(exp.fuel.f=="Low", na.rm=T), p_fuelexp_Low=p(n_fuelexp_Low,total),
            # n_fuelexp_NA=sum(is.na(exp.fuel.f)), p_fuelexp_NA=p(n_fuelexp_NA,total),
            # xtabs(data=data,~packall.f,addNA=T)
            n_smoke_None=sum(packall.f=="None", na.rm=T), p_smoke_None=p(n_smoke_None, total),
            n_smoke_Light=sum(packall.f=="Light", na.rm=T), p_smoke_Light=p(n_smoke_Light,total),
            n_smoke_Heavy=sum(packall.f=="Heavy", na.rm=T), p_smoke_Heavy=p(n_smoke_Heavy,total),
            n_smoke_NA=sum(is.na(packall.f)), p_smoke_NA=p(n_smoke_NA,total),
            # xtabs(data=data,~etoh3f,addNA=T)
            n_etoh_Never=sum(etoh3f=="never", na.rm=T), p_etoh_Never=p(n_etoh_Never,total),
            n_etoh_Past=sum(etoh3f=="past", na.rm=T), p_etoh_Past=p(n_etoh_Past,total),
            n_etoh_Current=sum(etoh3f=="current", na.rm=T), p_etoh_Current=p(n_etoh_Current,total),
            n_etoh_NA=sum(is.na(etoh3f)), p_etoh_NA=p(n_etoh_NA,total),
            # n_smoke_NA=sum(is.na(smoke.3f)), p_smoke_NA=p(n_smoke_NA,total),
            # n_snuff_Yes=sum(snuff.f=="yes",na.rm=T), p_snuff_Yes=p(n_snuff_Yes,total),
            # n_snuff_No=sum(snuff.f=="no",na.rm=T), p_snuff_No=p(n_snuff_No,total),
            # n_snuff_NA=sum(is.na(snuff.f)), p_snuff_NA=p(n_snuff_NA,total),
            # n_betel_Yes=sum(betel.f=="yes",na.rm=T), p_betel_Yes=p(n_betel_Yes,total),
            # n_betel_No=sum(betel.f=="no", na.rm=T), p_betel_No=p(n_betel_No,total),
            # n_betel_NA=sum(is.na(betel.f)), p_betel_NA=p(n_betel_NA,total),
            n_po_None=sum(potobac.f=="None"), p_po_None=p(n_po_None,total),
            n_po_Low=sum(potobac.f=="Low"), p_po_Low=p(n_po_Low,total),
            n_po_Med=sum(potobac.f=="Med"), p_po_Med=p(n_po_Med,total),
            n_po_High=sum(potobac.f=="High"), p_po_High=p(n_po_High,total),
            # xtabs(data=data,~uv_yrs.f, addNA=T)
            n_uv_Low=sum(uv_yrs.f=="Low", na.rm=T), p_uv_Low=p(n_uv_Low,total),
            n_uv_Lowmed=sum(uv_yrs.f=="Low-Med", na.rm=T), p_uv_Lowmed=p(n_uv_Lowmed,total),
            n_uv_Med=sum(uv_yrs.f=="Med", na.rm=T), p_uv_Med=p(n_uv_Med,total),
            n_uv_Highmed=sum(uv_yrs.f=="High-Med", na.rm=T), p_uv_Highmed=p(n_uv_Highmed,total),
            n_uv_High=sum(uv_yrs.f=="High", na.rm=T), p_uv_High=p(n_uv_High,total),
            n_uv_NA=sum(is.na(uv_yrs.f)), p_uv_NA=p(n_uv_NA,total)) %>%
  # n_drink_None=sum(etoh_exp.f=="None", na.rm=T), p_drink_None=p(n_drink_None,total),
  # n_drink_Low=sum(etoh_exp.f=="Low", na.rm=T), p_drink_Low=p(n_drink_Low,total),
  # n_drink_Med=sum(etoh_exp.f=="Med", na.rm = T), p_drink_Med=p(n_drink_Med,total),
  # n_drink_High=sum(etoh_exp.f=="High", na.rm = T), p_drink_High=p(n_drink_High,total),
  # n_drink_NA=sum(is.na(etoh_exp.f)), p_drink_NA=p(n_drink_NA,total)) %>%
  # CONSIDER ADDING FOLLOW UP DATA, I.E., HOW MANY HAD FOLLOW UP AND REPORTED TRAUMA
  pivot_longer(n_age_35.39:p_uv_NA, names_to = c("np","var","levels"), names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         n=round(n, digits=0)) %>%
  mutate(p=per(p)) %>%
  unite(col = "Number (%)", n, p, sep = " ") %>% dplyr::rename(Variable=var, Levels=levels) %>%
  mutate(Levels=case_when(Levels=="35.39"~"35-39",
                          Levels=="40.44"~"40-44",
                          Levels=="45.49"~"45-49",
                          Levels=="50plus"~"50+",
                          Levels=="lowmid"~"Lower middle",
                          Levels=="NA"~"Missing",
                          TRUE ~ Levels),
         Variable=case_when(Variable=="age"~"Age category",
                            Variable=="sex"~"Sex",
                            Variable=="ses"~"Socioeconomic status",
                            Variable=="educ"~"Education",
                            Variable=="work"~"Employment",
                            # Variable=="worksun"~"Work in sun",
                            # Variable=="fuel"~"Cook fuel",
                            Variable=="fuel"~"Cook-fuel exposure",
                            Variable=="smoke"~"Smoking status",
                            Variable=="po"~"Snuff / betel use",
                            Variable=="uv"~"Sunlight exposure",
                            # Variable=="snuff"~"Snuff use",
                            # Variable=="betel"~"Betel use",
                            Variable=="drink"~"Drinking status", 
                            TRUE ~ Variable)) %>% dplyr::select(-total)
table1.bl
# view(table1.bl)
write_csv(table1.bl, here("tables","table1","table1bl.csv"))

# baseline eye variables (i.e., refraction)
table1.blref <- data.eye %>% ungroup() %>%
  # xtabs(data=data.eye,~sphere.2f,addNA=T)
  summarise(total=sum(!is.na(eye)),
            n_none=sum(sphere.2f=="none"), p_none=p(n_none, total),
            n_hyperopia=sum(sphere.2f=="hyperopia"), p_hyperopia=p(n_hyperopia, total),
            n_myopia=sum(sphere.2f=="myopia"), p_myopia=p(n_myopia, total)) %>%
  pivot_longer(n_none:p_myopia, 
               names_to = c("np","severity"), 
               names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         p=per(p)) %>%
  unite("N (%)", n, p , sep = " ") %>%
  mutate(severity=case_when(severity=="none"~"None",
                            severity=="hyperopia"~"Hyperopia",
                            severity=="myopia"~"Myopia")) %>%
  rename("Refractive Error"=severity) %>% 
  dplyr::select(-total)

table1.blref
write_csv(table1.blref, here("tables", "table1", "table1_blref.csv"))

# 15 year follow up data
table1.fu <- data %>% 
  mutate(status.f=case_when(status==1~"followed",
                            status %in% c(2,3)~"dead or lost")) %>% group_by(status.f) %>%
  summarise(total=sum(!is.na(age_house)),
            n_age_35.39=sum(agecat=="35-39"), p_age_35.39=p(n_age_35.39, total),
            n_age_40.44=sum(agecat=="40-44"), p_age_40.44=p(n_age_40.44, total),
            n_age_45.49=sum(agecat=="45-49"), p_age_45.49=p(n_age_45.49, total),
            n_age_50plus=sum(agecat=="50+"), p_age_50plus=p(n_age_50plus, total),
            n_sex_Male=sum(sex1=="Male"), p_sex_Male=p(n_sex_Male,total),
            n_sex_Female=sum(sex1=="Female"), p_sex_Female=p(n_sex_Female,total),
            n_ses_Poor=sum(ses.f=="1", na.rm=T), p_ses_Poor=p(n_ses_Poor,total),
            n_ses_lowmid=sum(ses.f=="2",na.rm = T), p_ses_lowmid=p(n_ses_lowmid,total),
            n_ses_Middle=sum(ses.f=="3", na.rm=T), p_ses_Middle=p(n_ses_Middle,total),
            n_ses_Upper=sum(ses.f=="4", na.rm = T), p_ses_Upper=p(n_ses_Upper,total),
            n_ses_NA=sum(is.na(ses.f)), p_ses_NA=p(n_ses_NA,total),
            n_educ_Illiterate=sum(educ.f=="illiterate", na.rm = T), p_educ_Illiterate=p(n_educ_Illiterate,total),
            n_educ_Primary=sum(educ.f=="primary", na.rm = T), p_educ_Primary=p(n_educ_Primary,total),
            n_educ_Middle=sum(educ.f=="middle",na.rm=T), p_educ_Middle=p(n_educ_Middle,total),
            n_educ_Secondary=sum(educ.f=="secondary", na.rm=T), p_educ_Secondary=p(n_educ_Secondary,total),
            n_educ_NA=sum(is.na(educ.f)), p_educ_NA=p(n_educ_NA,total),
            n_work_Agriculture=sum(occu.f=="agriculture", na.rm=T), p_work_Agriculture=p(n_work_Agriculture,total),
            n_work_Unemployed=sum(occu.f=="unemployed", na.rm=T), p_work_Unemployed=p(n_work_Unemployed,total),
            n_work_Other=sum(occu.f=="other",na.rm=T), p_work_Other=p(n_work_Other,total),
            n_work_NA=sum(is.na(occu.f)), p_work_NA=p(n_work_NA,total),
            # xtabs(data=data,~bmicat,addNA=T)
            n_bmi_under=sum(bmicat=="underweight"), p_bmi_under=p(n_bmi_under,total),
            n_bmi_norm=sum(bmicat=="normal"), p_bmi_norm=p(n_bmi_norm,total),
            n_bmi_over=sum(bmicat=="overweight"), p_bmi_over=p(n_bmi_over,total),
            n_bmi_obese=sum(bmicat=="obese"), p_bmi_obese=p(n_bmi_obese,total),
            # xtabs(data=data,~map.3f,addNA=T)
            n_map_optimal=sum(map.3f=="optimal"), p_map_optimal=p(n_map_optimal,total),
            n_map_norm=sum(map.3f=="normal"), p_map_norm=p(n_map_norm,total),
            n_map_hnorm=sum(map.3f=="high"), p_map_hnorm=p(n_map_hnorm,total),
            n_kitchen_Yes=sum(kitchen.f=="Yes", na.rm=T), p_kitchen_Yes=p(n_kitchen_Yes,total),
            n_kitchen_No=sum(kitchen.f=="No", na.rm=T), p_kitchen_No=p(n_kitchen_No,total),
            n_kitchen_NA=sum(is.na(kitchen.f)), p_kitchen_NA=p(n_kitchen_NA,total),
            # xtabs(data=data, ~fuel.f, addNA=T)
            n_fuel_kerosene=sum(fuel.f=="kerosene", na.rm=T), p_fuel_kerosene=p(n_fuel_kerosene,total),
            n_fuel_propane=sum(fuel.f=="propane", na.rm=T), p_fuel_propane=p(n_fuel_propane, total),
            n_fuel_wood=sum(fuel.f=="wood", na.rm=T), p_fuel_wood=p(n_fuel_wood,total),
            n_fuel_NA=sum(is.na(fuel.f)), p_fuel_NA=p(n_fuel_NA,total),
            # xtabs(data=data,~packall.f,addNA=T)
            n_smoke_None=sum(packall.f=="None", na.rm=T), p_smoke_None=p(n_smoke_None, total),
            n_smoke_Light=sum(packall.f=="Light", na.rm=T), p_smoke_Light=p(n_smoke_Light,total),
            n_smoke_Heavy=sum(packall.f=="Heavy", na.rm=T), p_smoke_Heavy=p(n_smoke_Heavy,total),
            n_smoke_NA=sum(is.na(packall.f)), p_smoke_NA=p(n_smoke_NA,total),
            # xtabs(data=data,~etoh3f,addNA=T)
            n_etoh_Never=sum(etoh3f=="never", na.rm=T), p_etoh_Never=p(n_etoh_Never,total),
            n_etoh_Past=sum(etoh3f=="past", na.rm=T), p_etoh_Past=p(n_etoh_Past,total),
            n_etoh_Current=sum(etoh3f=="current", na.rm=T), p_etoh_Current=p(n_etoh_Current,total),
            n_etoh_NA=sum(is.na(etoh3f)), p_etoh_NA=p(n_etoh_NA,total),
            n_po_None=sum(potobac.f=="None"), p_po_None=p(n_po_None,total),
            n_po_Low=sum(potobac.f=="Low"), p_po_Low=p(n_po_Low,total),
            n_po_Med=sum(potobac.f=="Med"), p_po_Med=p(n_po_Med,total),
            n_po_High=sum(potobac.f=="High"), p_po_High=p(n_po_High,total),
            # xtabs(data=data,~uv_yrs.f, addNA=T)
            n_uv_Low=sum(uv_yrs.f=="Low", na.rm=T), p_uv_Low=p(n_uv_Low,total),
            n_uv_Lowmed=sum(uv_yrs.f=="Low-Med", na.rm=T), p_uv_Lowmed=p(n_uv_Lowmed,total),
            n_uv_Med=sum(uv_yrs.f=="Med", na.rm=T), p_uv_Med=p(n_uv_Med,total),
            n_uv_Highmed=sum(uv_yrs.f=="High-Med", na.rm=T), p_uv_Highmed=p(n_uv_Highmed,total),
            n_uv_High=sum(uv_yrs.f=="High", na.rm=T), p_uv_High=p(n_uv_High,total),
            n_uv_NA=sum(is.na(uv_yrs.f)), p_uv_NA=p(n_uv_NA,total)) %>%
  pivot_longer(n_age_35.39:p_uv_NA, names_to = c("np","var","levels"), names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         n=round(n, digits=0)) %>%
  mutate(p=per(p)) %>%
  unite(col = "Number (%)", n, p, sep = " ") %>% dplyr::rename(Variable=var, Levels=levels) %>%
  mutate(Levels=case_when(Levels=="35.39"~"35-39",
                          Levels=="40.44"~"40-44",
                          Levels=="45.49"~"45-49",
                          Levels=="50plus"~"50+",
                          Levels=="lowmid"~"Lower middle",
                          Levels=="NA"~"Missing",
                          TRUE ~ Levels),
         Variable=case_when(Variable=="age"~"Age category",
                            Variable=="sex"~"Sex",
                            Variable=="ses"~"Socioeconomic status",
                            Variable=="educ"~"Education",
                            Variable=="work"~"Employment",
                            Variable=="fuel"~"Cook-fuel exposure",
                            Variable=="smoke"~"Smoking status",
                            Variable=="po"~"Snuff / betel use",
                            Variable=="uv"~"Sunlight exposure",
                            Variable=="drink"~"Drinking status", 
                            TRUE ~ Variable)) %>% dplyr::select(-total)
table1.fu
# view(table1.fu)
write_csv(table1.fu, here("tables","table1","table1_fu.csv"))

# follow up eye refraction
table1.furef <- data.eye %>% ungroup() %>%
  mutate(status.f=case_when(status==1~"followed",
                            status %in% c(2,3)~"dead or lost")) %>% group_by(status.f) %>%
  # xtabs(data=data.eye,~sphere.2f,addNA=T)
  summarise(total=sum(!is.na(eye)),
            n_none=sum(sphere.2f=="none"), p_none=p(n_none, total),
            n_hyperopia=sum(sphere.2f=="hyperopia"), p_hyperopia=p(n_hyperopia, total),
            n_myopia=sum(sphere.2f=="myopia"), p_myopia=p(n_myopia, total)) %>%
  pivot_longer(n_none:p_myopia, 
               names_to = c("np","severity"), 
               names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         p=per(p)) %>%
  unite("N (%)", n, p , sep = " ") %>%
  mutate(severity=case_when(severity=="none"~"None",
                            severity=="hyperopia"~"Hyperopia",
                            severity=="myopia"~"Myopia")) %>%
  rename("Refractive Error"=severity) %>% 
  dplyr::select(-total)

table1.furef
write_csv(table1.furef, here("tables","table1","table1_furef.csv"))

#### table 2 - univariable association ####

# trying a feasible generalized least squares model per UCLA IDREE
library(nlme)

# first step is to convert my data to long format with three outomes stacked in a single column
data.long <- data.eye %>%
  # first selecting the relevant variables
  select(studyno, sex1, agecat, ses.f, ses.of, educ.f, occu.f, bmicat, map.3f, kitchen.f,
         fuel.f, packall.f, etoh3f, potobac.f, uv_yrs.f, eye, sphereeq.4f, sphere.2f,
         mean.nucop, mean.cor, mean.ps) %>%
  # now pivoting longer
  pivot_longer(mean.nucop:mean.ps, names_to = "cattype", values_to = "score")

data.long

# base model
m.base <- gls(score ~ 0 + cattype,
              weights = varIdent(form=~1|cattype),
              correlation = corSymm(form=~1 | studyno),
              data = data.long)
summary(m.base)

# modeling the univariable associations
uni <- gls(score ~ 0 + cattype + cattype:sphere.2f, # iterating through all variables here
               weights = varIdent(form=~1|cattype),
               correlation = corSymm(form=~1 | studyno),
               data = filter(data.long, !is.na(sphere.2f))) # iterating

# essentially using this code below from UCLA IDRE
# m <- gls(score ~ 0 + test + test:prog,
#          weights = varIdent(form=~1|test),
#          correlation=corSymm(form=~ 1 | id), data=hsb_long)

# estimates
est <- summary(uni)$tTable %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname != "cattypemean.cor" & rowname != "cattypemean.nucop" & rowname != "cattypemean.ps") %>%
  select(rowname, Value) %>%
  mutate(Value=round(Value, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  rename(est=Value) %>%
  arrange(cattype) %>% as_tibble()
est

# confidence intervals
ci <- confint(uni)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname != "cattypemean.cor" & rowname != "cattypemean.nucop" & rowname != "cattypemean.ps") %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype) %>% as_tibble()

# putting estimates and cis together
uni.table <- full_join(est, ci) %>%
  unite(estci, est, ci, sep = " ")
uni.table

# saving the csv 
write_csv(uni.table, here("tables","table2","uniref.csv"))

# omnibus p-value
anova(uni)

# alternatively can do a likelihood ratio test
# lmtest::lrtest(uni, m.base)


#### table 3 -- confounders ####
# unadjusted model
m.unadj <- gls(score ~ 0 + cattype + cattype:fuel.f,
              weights = varIdent(form=~1|cattype),
              correlation = corSymm(form=~1 | studyno),
              data = filter(data.long, !is.na(fuel.f)))

est <- summary(m.unadj)$tTable %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(p=`p-value`, est=Value) %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  select(rowname, est) %>%
  mutate(est=round(est, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

ci <- confint(m.unadj)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

full_join(est, ci) %>%
  unite(estci, est, ci, sep = " ") %>%
  write_csv(., here("tables","table3","unadj.csv"))

# iterating through confounders
m.conf <- gls(score ~ 0 + cattype + cattype:fuel.f + cattype:sphere.2f,
           weights = varIdent(form=~1|cattype),
           correlation = corSymm(form=~1 | studyno),
           data = filter(data.long, !is.na(fuel.f) & !is.na(sphere.2f)))

est <- summary(m.conf)$tTable %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(p=`p-value`, est=Value) %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  select(rowname, est) %>%
  mutate(est=round(est, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

ci <- confint(m.conf)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

full_join(est, ci) %>%
  unite(estci, est, ci, sep = " ") %>%
  write_csv(., here("tables","table3","ref.csv"))

#### table 4 -- final model ####
# for now just adding all RFs in univariable analysis + a-priori RFs

m.final <- gls(score ~ 0 + cattype + cattype:fuel.f + cattype:agecat + cattype:sex1 + 
                 cattype:ses.f + cattype:educ.f + cattype:bmicat + cattype:kitchen.f +
                 cattype:packall.f + cattype:potobac.f + cattype:uv_yrs.f + cattype:sphere.2f,
               weights = varIdent(form=~1|cattype),
               correlation = corSymm(form=~1 | studyno),
               data = filter(data.long, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
                             & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
                             & !is.na(potobac.f) & !is.na(uv_yrs.f)))

est <- summary(m.final)$tTable %>%
  as.data.frame() %>% rownames_to_column() %>%
  select(rowname, Value) %>%
  filter(rowname != "cattypemean.cor" & rowname != "cattypemean.nucop" & rowname != "cattypemean.ps") %>%
  rename(est=Value) %>%
  mutate(est=round(est, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype, parameter)

ci <- confint(m.final)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  filter(rowname != "cattypemean.cor" & rowname != "cattypemean.nucop" & rowname != "cattypemean.ps") %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

full_join(est, ci) %>% 
  unite(estci, est, ci, sep = " ") %>%
  write_csv(., here("tables","table4","finalmodel.csv"))

# obtaining p-values via LRT comparing final model with and without variable of interest
# mwithout <- gls(score ~ 0 + cattype + cattype:sex1 + cattype:fuel.f + # cattype:agecat +
#                   cattype:ses.f + cattype:educ.f + cattype:bmicat + cattype:kitchen.f +
#                   cattype:potobac.f + cattype:packall.f + cattype:uv_yrs.f + cattype:sphere.2f,
#                 weights = varIdent(form=~1|cattype),
#                 correlation = corSymm(form=~1 | studyno),
#                 data = filter(data.long, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
#                               & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
#                               & !is.na(potobac.f) & !is.na(uv_yrs.f)))
# 
# # lrt
# lmtest::lrtest(mwithout, m.final)
# anova(mwithout, m.final)
# anova(mwithout, m.final)
anova(m.final) # can I use this? looks like the p-values correspond better with the 95%CIs

# sex1, agecat, ses.f, ses.of, educ.f, occu.f, bmicat, map.3f, kitchen.f,
# fuel.f, packall.f, etoh3f, potobac.f, uv_yrs.f, eye, sphereeq.4f, 
# mean.nucop, mean.cor, mean.ps

#### final model stratified by sex ####

# female
m.finalf <- gls(score ~ 0 + cattype + cattype:fuel.f + cattype:agecat + 
                 cattype:ses.f + cattype:educ.f + cattype:bmicat + cattype:kitchen.f +
                 cattype:packall.f + cattype:potobac.f + cattype:uv_yrs.f + cattype:sphere.2f,
               weights = varIdent(form=~1|cattype),
               correlation = corSymm(form=~1 | studyno),
               data = filter(data.long, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
                             & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
                             & !is.na(potobac.f) & !is.na(uv_yrs.f) & sex1 == "Female"))

est.f <- summary(m.finalf)$tTable %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(p=`p-value`, est=Value) %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  select(rowname, est) %>%
  mutate(est=round(est, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

ci.f <- confint(m.finalf)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

full_join(est.f, ci.f) %>%
  unite(estci, est, ci, sep = " ") %>%
  write_csv(., here("tables","sex_stratified","finalmodelf.csv"))

# male
m.finalm <- gls(score ~ 0 + cattype + cattype:fuel.f + cattype:agecat + 
                  cattype:ses.f + cattype:educ.f + cattype:bmicat + cattype:kitchen.f +
                  cattype:packall.f + cattype:potobac.f + cattype:uv_yrs.f + cattype:sphere.2f,
                weights = varIdent(form=~1|cattype),
                correlation = corSymm(form=~1 | studyno),
                data = filter(data.long, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
                              & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
                              & !is.na(potobac.f) & !is.na(uv_yrs.f) & sex1 == "Male"))

est.m <- summary(m.finalm)$tTable %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(p=`p-value`, est=Value) %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  select(rowname, est) %>%
  mutate(est=round(est, digits = 3)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

ci.m <- confint(m.finalm)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname %in% c("cattypemean.cor:fuel.fpropane","cattypemean.nucop:fuel.fpropane",
                        "cattypemean.ps:fuel.fpropane", "cattypemean.cor:fuel.fwood",
                        "cattypemean.nucop:fuel.fwood", "cattypemean.ps:fuel.fwood")) %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  mutate(low=round(low, digits = 3), 
         high=round(high, digits=3)) %>%
  unite(ci, low, high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

full_join(est.f, ci.f) %>%
  unite(estci, est, ci, sep = " ") %>%
  write_csv(., here("tables","sex_stratified","finalmodelm.csv"))


#### figure ####

ci <- confint(m.final)[,1:2] %>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(low = '2.5 %', high = '97.5 %') %>%
  filter(rowname != "cattypemean.cor" & rowname != "cattypemean.nucop" & rowname != "cattypemean.ps") %>%
  mutate(low=round(low, digits = 5), 
         high=round(high, digits=5)) %>%
  separate(rowname, into = c("cattype", "parameter"), sep = ":") %>%
  separate(cattype, into = c("trash", "cattype", sep = ".")) %>%
  select(-trash, -.) %>%
  arrange(cattype)

# creating reference values for each variable
ref <- data.frame(cattype = c("Cortical","Nuclear","Subcapsular"),
                     parameter = c("35-39","35-39","35-39",
                                   "Underweight","Underweight","Underweight",
                                   "Primary","Primary","Primary",
                                   "Kerosene","Kerosene","Kerosene",
                                   "No","No","No",
                                   "Never","Never","Never",
                                   "None","None","None",
                                   "Lowest","Lowest","Lowest",
                                   "Female","Female","Female",
                                   "No error","No error","No error",
                                   "1st quantile","1st quantile","1st quantile"),
                     est = rep(c(0.00,0.00,0.00), times = 11),
                     low = rep(c(0.00,0.00,0.00), times = 11),
                     high = rep(c(0.00,0.00,0.00), times = 11),
                     var = c("Age","Age","Age",
                             "BMI","BMI","BMI",
                             "Education","Education","Education",
                             "Fuel","Fuel","Fuel",
                             "Kitchen","Kitchen","Kitchen",
                             "Smoking","Smoking","Smoking",
                             "Snuff / betel use","Snuff / betel use","Snuff / betel use",
                             "SES","SES","SES",
                             "Sex","Sex","Sex",
                             "Refractive error","Refractive error","Refractive error",
                             "Sun exposure","Sun exposure","Sun exposure"))
  
figdata <- full_join(est, ci) %>%
  mutate(var=case_when(grepl("agecat", parameter)~"Age",
                       grepl("bmicat", parameter)~"BMI",
                       grepl("educ.f", parameter)~"Education",
                       grepl("fuel.f", parameter)~"Fuel",
                       grepl("kitchen.f", parameter)~"Kitchen",
                       grepl("packall.f", parameter)~"Smoking",
                       grepl("potobac", parameter)~"Snuff / betel use",
                       grepl("ses.f", parameter)~"SES",
                       grepl("sex", parameter)~"Sex",
                       grepl("sphere.2f", parameter)~"Refractive error",
                       grepl("uv_yrs", parameter)~"Sun exposure"),
         parameter=factor(case_when(parameter == "agecat40-44" ~ "40-44",
                             parameter == "agecat45-49" ~ "45-49",
                             parameter == "agecat50+" ~ "50+",
                             parameter == "bmicatnormal" ~ "Normal",
                             parameter == "bmicatobese" ~ "Obese",
                             parameter == "bmicatoverweight" ~ "Overweight",
                             parameter == "educ.filliterate" ~ "Illiterate",
                             parameter == "educ.fmiddle" ~ "Middle school",
                             parameter == "educ.fsecondary" ~ "Secondary",
                             parameter == "fuel.fpropane" ~ "Propane",
                             parameter == "fuel.fwood" ~ "Wood",
                             parameter == "kitchen.fYes" ~ "Yes",
                             parameter == "packall.fHeavy" ~ "Heavy",
                             parameter == "packall.fLight" ~ "Light",
                             parameter == "potobac.fHigh" ~ "High",
                             parameter == "potobac.fLow" ~ "Low",
                             parameter == "potobac.fMed" ~ "Moderate",
                             parameter == "ses.f2" ~ "Lower-middle",
                             parameter == "ses.f3" ~ "Middle",
                             parameter == "ses.f4" ~ "Upper",
                             parameter == "sex1Male" ~ "Male",
                             parameter == "sphere.2fhyperopia" ~ "Hyperopia",
                             parameter == "sphere.2fmyopia" ~ "Myopia",
                             # parameter == "sphereeq.4fmod-high myopia" ~ "Moderate-\nhigh myopia",
                             parameter == "uv_yrs.fHigh" ~ "5th quantile",
                             parameter == "uv_yrs.fHigh-Med" ~ "4th quantile",
                             parameter == "uv_yrs.fLow-Med" ~ "2nd quantile",
                             parameter == "uv_yrs.fMed" ~ "3rd quantile"), 
                             levels = c("35-39","40-44", "45-49", "50+", "Female", "Male", # age and sex
                                        "Underweight","Normal","Overweight", "Obese", # BMI
                                        "Primary","Illiterate", "Middle school", "Secondary", # educ
                                        "Kerosene","Propane", "Wood", # fuel
                                        "No", "Yes", # kitchen
                                        "Never","Light", "Heavy", # smoking
                                        "None","Low", "Moderate","High", # potobac
                                        "Lowest","Lower-middle", "Middle", "Upper", # SES
                                        "1st quantile","2nd quantile", "3rd quantile", "4th quantile","5th quantile", # sun
                                        "No error","Hyperopia", "Myopia")), # ref error "Moderate-\nhigh myopia"
         cattype=case_when(cattype == "cor" ~ "Cortical",
                           cattype == "nucop" ~ "Nuclear",
                           cattype == "ps" ~ "Subcapsular")) %>%
  rbind(., ref)

# trying to group parameters by the variable they correspond to
fig <- figdata %>%
  ggplot(aes(x=parameter, y=est, label = parameter, color = cattype)) +
  geom_pointrange(aes(ymin = low, ymax = high),
                  alpha = 0.8,
                  position = position_dodge2(width = 0.5,  
                                             padding = 0.4)) +
  geom_hline(yintercept = 0, linetype=3) + 
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
  labs(color = "Cataract type", y = "Beta (95% CI)", x = "Variables")
fig

# saving
ggsave(here("figures","fig1","fig1.eps"),
       fig,
       device = cairo_ps,
       height = 4.5, width = 14, units = "in",
       dpi = 300)

#### sensitivity analyses using one eye per participant ####

# using the values from the worst eye for each participant: nucop, cor, ps, sphere.2f
m.sens <- lm(cbind(nucop, cor, ps) ~ fuel.f + agecat + sex1 + ses.f + educ.f + bmicat +
     kitchen.f + packall.f + potobac.f + uv_yrs.f + sphere.2f,
   data = filter(data, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
                 & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
                 & !is.na(potobac.f) & !is.na(uv_yrs.f)))

broom::tidy(m.sens, conf.int=T) %>%
  filter(term != "(Intercept)") %>%
  select(response, term, estimate, conf.low, conf.high) %>%
  mutate(estimate=round(estimate, digits=3),
         conf.low=round(conf.low, digits = 3), 
         conf.high=round(conf.high, digits=3)) %>%
  unite(ci, conf.low, conf.high, sep = " to ") %>%
  mutate(ci=paste0("(",ci,")")) %>%
  unite(ciest, estimate, ci, sep = " ") %>%
  write_csv(., here("tables", "mvsens", "mvsens.csv"))
  
# omnibus p-values
anova(m.sens)






