# Risk factors for baseline cataract severity
# John Nesemann

# packages
library(here)
library(tidyverse)

# telling R not to write in scientific notation
options(scipen = 999)

#### importing & cleaning data ####
data <- read_csv(here("data","cataract_data.csv")) %>%
  # first creating binary vars for the LOCSIII grades -- nuc opa and color ≥2 is positive
  mutate(nucopod.bin=case_when(mean_nucopod>=2~1,
                               mean_nucopod<2~0,
                               is.na(mean_nucopod)~NA_real_), 
         # xtabs(data=data,~mean_nucopod+nucopod.bin,addNA=T)
         nucopos.bin=case_when(mean_nucopos>=2 ~ 1,
                               mean_nucopos<2 ~ 0,
                               is.na(mean_nucopos) ~ NA_real_), 
         # xtabs(data=data,~mean_nucopos+nucopos.bin,addNA=T)
         nuccolod.bin=case_when(mean_nucolod>=2 ~ 1,
                                mean_nucolod<2~0,
                                is.na(mean_nucolod)~NA_real_), 
         # xtabs(data=data,~mean_nucolod+nuccolod.bin,addNA=T)
         nuccolos.bin=case_when(mean_nucolos>=2 ~ 1,
                                mean_nucolos<2~0,
                                is.na(mean_nucolos)~NA_real_), 
         # xtabs(data=data,~mean_nucolos+nuccolos.bin,addNA=T)
         # ≥1 is classified as positive for cortical and posterior subcapsular change 
         corod.bin=case_when(mean_corod>=1 ~ 1,
                             mean_corod<1 ~ 0,
                             is.na(mean_corod)~NA_real_), 
         # xtabs(data=data,~mean_corod+corod.bin,addNA=T)
         coros.bin=case_when(mean_coros>=1 ~ 1,
                             mean_coros<1 ~ 0,
                             is.na(mean_coros)~NA_real_), 
         # xtabs(data=data,~mean_coros+coros.bin,addNA=T)
         psod.bin=case_when(mean_psod>=1 ~ 1,
                            mean_psod<1 ~ 0,
                            is.na(mean_psod)~NA_real_), 
         # xtabs(data=data,~mean_psod+psod.bin,addNA=T)
         psos.bin=case_when(mean_psos>=1 ~ 1,
                            mean_psos<1 ~ 0,
                            is.na(mean_psos)~NA_real_), 
         # xtabs(data=data,~mean_psos+psos.bin,addNA=T)
         # now creating a mixed cataract var -- when any 2+ are positive
         # possible combos: nucop + cor, nucop + ps, cor + ps. Q: do I add nuclear color
         mixedod.bin=case_when(nucopod.bin==1 & corod.bin==1 ~ 1,
                               nucopod.bin==1 & psod.bin==1 ~ 1,
                               corod.bin==1 &  psod.bin==1 ~ 1,
                               TRUE ~ 0), 
         # xtabs(data=data,~mixedod.bin+corod.bin+nucopod.bin+psod.bin, addNA=T)
         mixedos.bin=case_when(nucopos.bin==1 & coros.bin==1 ~ 1,
                               nucopos.bin==1 & psos.bin==1 ~ 1,
                               coros.bin==1 &  psos.bin==1 ~ 1,
                               TRUE ~ 0), 
         # xtabs(data=data,~mixedos.bin+coros.bin+nucopos.bin+psos.bin, addNA=T)
         # regrouping cataract vars into integers
         # ggplot(data=data,aes(x=mean_nucopod)) + geom_histogram()
         nucopod.int=factor(case_when(mean_nucopod<=1~"1",
                                      mean_nucopod<=2~"2",
                                      mean_nucopod<=3~"3",
                                      mean_nucopod<=4~"4",
                                      is.na(mean_nucopod) ~ NA_character_),
                            levels = c("1","2","3","4")), 
         # xtabs(data=data,~nucopod.int+nucopod.bin,addNA=T)
         # ggplot(data=data,aes(x=mean_nucopos)) + geom_histogram()
         nucopos.int=factor(case_when(mean_nucopos<=1~"1",
                                      mean_nucopos<=2~"2",
                                      mean_nucopos<=3~"3",
                                      mean_nucopos<=4~"4",
                                      is.na(mean_nucopos) ~ NA_character_),
                            levels = c("1","2","3","4")), 
         # xtabs(data=data,~nucopos.int+nucopos.bin,addNA=T)
         # ggplot(data=data,aes(x=mean_nucolod))+geom_histogram()
         nucolod.int=factor(case_when(mean_nucolod<=1~"1",
                                      mean_nucolod<=2~"2",
                                      mean_nucolod<=3~"3",
                                      mean_nucolod<=4~"4",
                                      mean_nucolod<=5~"5",
                                      is.na(mean_nucolod)~NA_character_),
                            levels = c("1","2","3","4","5")), 
         # xtabs(data=data,~nucolod.int+nuccolod.bin,addNA=T)
         # ggplot(data=data,aes(x=mean_nucolos))+geom_histogram()
         nucolos.int=factor(case_when(mean_nucolos<=1~"1",
                                      mean_nucolos<=2~"2",
                                      mean_nucolos<=3~"3",
                                      mean_nucolos<=4~"4",
                                      mean_nucolos<=5~"5",
                                      is.na(mean_nucolos)~NA_character_),
                            levels = c("1","2","3","4","5")), 
         # xtabs(data=data,~nucolos.int+nuccolos.bin,addNA=T)
         # ggplot(data, aes(x=mean_corod))+geom_histogram()
         corod.int=factor(case_when(mean_corod<=1~"1",
                                    mean_corod<=2~"2",
                                    mean_corod<=3~"3",
                                    mean_corod<=4~"4",
                                    mean_corod<=5~"5",
                                    is.na(mean_corod)~NA_character_),
                          levels = c("1","2","3","4","5")), 
         # xtabs(data=data,~corod.int+corod.bin,addNA=T)
         # ggplot(data, aes(x=mean_coros))+geom_histogram()
         coros.int=factor(case_when(mean_coros<=1~"1",
                                    mean_coros<=2~"2",
                                    mean_coros<=3~"3",
                                    mean_coros<=4~"4",
                                    mean_coros<=5~"5",
                                    is.na(mean_coros)~NA_character_),
                          levels = c("1","2","3","4","5")), 
         # xtabs(data=data,~coros.int+coros.bin,addNA=T)
         # ggplot(data, aes(x=mean_psod))+geom_histogram()
         psod.int=factor(case_when(mean_psod<=1~"1",
                                   mean_psod<=2~"2",
                                   mean_psod<=3~"3",
                                   mean_psod<=4~"4",
                                   is.na(mean_psod)~NA_character_),
                         levels = c("1","2","3","4")), 
         # xtabs(data=data,~psod.int+psod.bin,addNA=T)
         # ggplot(data, aes(x=mean_psos))+geom_histogram()
         psos.int=factor(case_when(mean_psos<=1~"1",
                                   mean_psos<=2~"2",
                                   mean_psos<=3~"3",
                                   mean_psos<=4~"4",
                                   is.na(mean_psos)~NA_character_),
                         levels = c("1","2","3","4")), 
         # xtabs(data=data,~psos.int+psos.bin,addNA=T)
         # NEED TO REOCODE VA VARIABLES AS ORDERED FACTORS OR NUMERICS 
         # regrouping the upper two groups of SES
         ses=case_when(asses %in% c(4,5) ~ 4,
                       asses==3 ~ 3,
                       asses==2 ~ 2,
                       asses==1 ~ 1,
                       TRUE ~ NA_real_), 
         # xtabs(data=data,~ses+asses,addNA=T)
         ses.f=factor(as.character(ses), levels = c("1","2","3","4")),
         ses.of=factor(ses.f, ordered = T, levels = c("1","2","3","4")),
         # generating a factor education var
         educ.f=factor(case_when(educ_simple==1~"illiterate",
                                 educ_simple==2~"primary",
                                 educ_simple==3~"middle",
                                 educ_simple==4 ~"secondary",
                                 educ_simple==5 ~ NA_character_),
                       levels = c("primary","illiterate","middle","secondary")), 
         # xtabs(data=data,~educ_simple+educ.f,addNA=T)
         occu.f=factor(case_when(occucode_simple==3~"unemployed",
                                 occucode_simple==2~"other",
                                 occucode_simple==1~"agriculture",
                                 occucode_simple==4~NA_character_),
                       levels = c("agriculture","unemployed","other")), 
         # xtabs(data=data,~occu.f,addNA=T)
         height.m=height/100, # convering height from cm to m
         # generating BMI
         bmi=weight/height.m^2, # xtabs(data=data,~bmi,addNA=T)
         # generating mean arterial pressure
         map=(bpsys+(2*bpdios))/3, # xtabs(data=data,~map,addNA=T)
         # creating spherical equivalent variable
         sphereeqod=sphod+(0.5*cylod), # xtabs(data=data,~sphereeqod,addNA=T)
         sphereeqos=sphos+(0.5*cylos), # xtabs(data=data,~sphereeqos,addNA=T)
         exp.fuel.f=factor(exp.fuel.f, levels = c("Med","Low","High")), 
         # xtabs(data=data,~exp.fuel.f,addNA=T)
         exp.fuel.of=factor(exp.fuel.f, ordered = T, levels = c("Med","Low","High")),
         # age categories xtabs(data=data,~age_house,addNA=T)
         agecat=case_when(age_house<40~"35-39",
                          age_house<45~"40-44",
                          age_house<50~"45-49",
                          age_house>=50~"50+",
                          is.na(age_house)~NA_character_), 
         # xtabs(data=data,~agecat+age_house,addNA=T)
         agecat=factor(agecat, levels = c("35-39","40-44","45-49","50+")),
         agecat.int=case_when(agecat=="35-39"~1,
                              agecat=="40-44"~2,
                              agecat=="45-49"~3,
                              agecat=="50+"~4,
                              is.na(agecat)~NA_real_),
         # xtabs(data=data,~agecat+agecat.int, addNA=T)
         bmicat=factor(case_when(bmi < 18.5 ~ "underweight",
                                 bmi < 25 ~ "normal",
                                 bmi >= 25 & bmi < 30 ~ "overweight",
                                 bmi > 30 ~ "obese",
                                 TRUE ~ NA_character_), # xtabs(data=data,~bmicat,addNA=T)
                       levels = c("underweight","normal","overweight" ,"obese")), 
         # xtabs(data=data,~bmicat,addNA=T)
         map.f=factor(case_when(map < 93.33 ~ "optimal",
                                map >= 93.33 & map <= 99 ~ "normal",
                                map > 99 & map <= 105.67 ~ "high normal",
                                map > 105.67 ~ "hypertension", 
                                TRUE ~ NA_character_), # xtabs(data=data,~map.f,addNA=T)
                      levels = c("optimal","normal","high normal","hypertension")), 
         # xtabs(data=data,~map.f,addNA=T)
         map.3f=factor(if_else(map.f %in% c("high normal","hypertension"), "high", as.character(map.f)),
                       levels = c("optimal","normal","high")), # xtabs(data=data,~map.f+map.3f,addNA=T)
         pulse.f=factor(case_when(pulse > 100 ~ "tachy",
                                  pulse < 60 ~ "brady",
                                  pulse <= 100 & pulse >= 60 ~ "normal",
                                  T ~ NA_character_),
                        levels = c("normal","brady","tachy")), 
         # xtabs(data=data,~pulse+pulse.f,addNA=T)
         gluc.f=factor(case_when(rbg<140~"normal", 
                                 # xtabs(data=data,~rbg,addNA=T)
                                 rbg>=140~"high",
                                 TRUE ~ NA_character_),
                       levels = c("normal","high")), # xtabs(data=data,~gluc.f,addNA=T)
         # xtabs(data=data,~resprate,addNA=T)
         resp.f=factor(case_when(resprate<12 ~ "brady",
                                 resprate>20 ~ "tachy",
                                 resprate>=12 & resprate<=20 ~ "normal",
                                 T ~ NA_character_),
                       levels = c("brady","normal","tachy")), 
         # xtabs(data=data,~resprate+resp.f,addNA=T)
         worksun.bin=factor(if_else(worksun==1,"Yes","No"),
                            levels = c("No","Yes")), 
         # xtabs(data=data,~worksun+worksun.bin,addNA=T)
         # ggplot(filter(data, uv_yrs !=0), aes(x=uv_yrs)) + geom_histogram()
         uv_yrs.5=factor(ntile(uv_yrs, 5)), 
         # addmargins(xtabs(data=data,~uv_yrs+uv_yrs.5,addNA=T))
         uv_yrs.f=factor(case_when(uv_yrs.5==1 ~ "Low",
                                   uv_yrs.5==2 ~ "Low-Med",
                                   uv_yrs.5==3 ~ "Med",
                                   uv_yrs.5==4 ~ "High-Med",
                                   uv_yrs.5==5 ~ "High",
                                   is.na(uv_yrs.5) ~ NA_character_),
                         levels = c("Low","Low-Med","Med","High-Med","High")), 
         # xtabs(data=data,~uv_yrs.5+uv_yrs.f,addNA=T)
         uv_yrs.of=factor(uv_yrs.f, ordered=T, levels = c("None","Low","Med","High")),
         # ggplot(filter(data, peakuv !=0), aes(x=peakuv)) + geom_histogram()
         # peakuv.f=factor(case_when(peakuv == 0 ~ "None",
         #                           peakuv > 0 & peakuv <=60 ~ "Low", 
         # # quantile(filter(data, peakuv !=0)$peakuv, 0.33, na.rm=T)
         #                           peakuv > 60 & peakuv <= 90 ~ "Med", 
         # # quantile(filter(data, peakuv !=0)$peakuv, 0.66, na.rm=T)
         #                           peakuv > 90 ~ "High",
         #                           is.na(peakuv) ~ NA_character_),
         #                 levels = c("None","Low","Med","High")), # xtabs(data=data,~peakuv.f,addNA=T)
         # NEED TO CREATE VARS FOR SMOKING, ETOH, AND PO TOBACCO
         smoke.3f=factor(smoke.3f, levels = c("never","past","current")),
         smoke.2f=factor(case_when(smoke.3f=="never"~"never",
                                   smoke.3f %in% c("past","current")~"past or current",
                                   is.na(smoke.3f) ~ NA_character_),
                         levels = c("never","past or current")),
         etoh3f=factor(etoh3f, levels = c("never","past","current")), 
         # xtabs(data=data,~etoh3f,addNA=T)
         etoh2f=factor(case_when(etoh3f=="never"~"never",
                                 etoh3f %in% c("past","currect")~"past or current",
                                 is.na(etoh3f)~NA_character_),
                       levels = c("never","past or current")),
         # ggplot(filter(data, exp_potobac !=0), aes(x=exp_potobac)) + geom_histogram()
         potobac.f=factor(case_when(is.na(exp_potobac)~"None",
                                    exp_potobac <= 3 ~ "Low", 
                                    # quantile(data$exp_potobac, 0.33, na.rm=T)
                                    exp_potobac > 3 & exp_potobac < 24.98 ~ "Med", 
                                    # quantile(data$exp_potobac, 0.66, na.rm=T)
                                    exp_potobac >= 24.98 ~ "High"),
                          levels = c("None","Low","Med","High")),
         # xtabs(data=data,~potobac.f,addNA=T)
         trauma_past3mon.f=factor(case_when(trauma_past3mon==1~"Yes",
                                            trauma_past3mon==0~"No",
                                            is.na(trauma_past3mon)~NA_character_),
                                  levels = c("No","Yes")),
         trauma_past12mon.f=factor(case_when(trauma_past12mon==1~"Yes",
                                             trauma_past12mon==0~"No",
                                             is.na(trauma_past12mon)~NA_character_),
                                   levels = c("No","Yes")),
         trauma_15yrs.f=factor(case_when(trauma_15yrs==1~"Yes",
                                         trauma_15yrs==0~"No",
                                         is.na(trauma_15yrs)~NA_character_),
                               levels = c("No","Yes")),
         # ggplot(filter(data, packall !=0), aes(x=packall)) + geom_histogram()
         # quantile(filter(data, packall!=0)$packall, 0.66)
         packall.f=factor(case_when(packall==0~"None",
                                    packall <15 ~"Light",
                                    packall >= 15 ~ "Heavy",
                                    is.na(packall)~NA_character_),
                          levels = c("None","Light","Heavy")), 
         # xtabs(data=data,~packall.f,addNA=T)
         # ggplot(data, aes(x=etoh_exp)) + geom_histogram()
         # quantile(filter(data, etoh_exp!=0)$packall, 0.66)
         etoh_exp.f=factor(case_when(etoh_exp==0~"None",
                                     etoh_exp <= 1.3955~"Low",
                                     etoh_exp > 1.3955 & etoh_exp <=17.455 ~ "Med",
                                     etoh_exp > 17.455 ~ "High",
                                     is.na(etoh_exp)~NA_character_),
                           levels = c("None","Low","Med","High")), 
         # xtabs(data=data,~etoh_exp.f,addNA=T)
         # visual impairment and blindness
         # xtabs(data=data,~bcvaod,addNA=T) 
         bcvaod.f=factor(case_when(bcvaod == "4/2" ~ "20/10",
                                   bcvaod == "4/2.5" ~ "20/12.5",
                                   bcvaod == "4/3" ~ "20/15",
                                   bcvaod == "4/4" ~ "20/20",
                                   bcvaod == "4/5" ~ "20/25",
                                   bcvaod == "4/6" ~ "20/30",
                                   bcvaod == "4/8" ~ "20/40",
                                   bcvaod == "4/10" ~ "20/50"), 
                         levels = c("20/10","20/12.5","20/15","20/20","20/25","20/30","20/40","20/50")), 
         # addmargins(xtabs(data=data,~bcvaod+bcvaod.f,addNA=T))
         # recoding as ordinal variable to fit into the lm
         bcvaod.n=case_when(bcvaod.f=="20/10"~1,
                            bcvaod.f=="20/12.5"~2,
                            bcvaod.f=="20/15"~3,
                            bcvaod.f=="20/20"~4,
                            bcvaod.f=="20/25"~5,
                            bcvaod.f=="20/30"~6,
                            bcvaod.f=="20/40"~7,
                            bcvaod.f=="20/50"~8),
         # xtabs(data=data,~bcvaod.f+bcvaod.n,addNA=T)
         # xtabs(data=data,~bcvaos,addNA=T)
         bcvaos.f=factor(case_when(bcvaos == "4/2" ~ "20/10",
                                   bcvaos == "4/2.5" ~ "20/12.5",
                                   bcvaos == "4/3" ~ "20/15",
                                   bcvaos == "4/4" ~ "20/20",
                                   bcvaos == "4/5" ~ "20/25",
                                   bcvaos == "4/6" ~ "20/30",
                                   bcvaos == "4/8" ~ "20/40",
                                   bcvaos == "4/10" ~ "20/50"), 
                         levels = c("20/10","20/12.5","20/15","20/20","20/25","20/30","20/40","20/50")), 
         # addmargins(xtabs(data=data,~bcvaos+bcvaos.f,addNA=T))
         # and creating a numeric variable
         bcvaos.n=case_when(bcvaos.f=="20/10"~1,
                            bcvaos.f=="20/12.5"~2,
                            bcvaos.f=="20/15"~3,
                            bcvaos.f=="20/20"~4,
                            bcvaos.f=="20/25"~5,
                            bcvaos.f=="20/30"~6,
                            bcvaos.f=="20/40"~7,
                            bcvaos.f=="20/50"~8), 
         # xtabs(data=data,~bcvaos.f+bcvaos.n,addNA=T)
         # recoding sex1 as factor
         sex1=factor(sex1, levels= c("Female","Male"))) # %>%
  # mutate(sphereeq_od.f=case_when(sphod <= -6 ~ "high myopia",
  #                                sphod > -6 & sphod <= -3 ~ "mod myopia",
  #                                sphod > -3 & sphod <= -0.5 ~ "low myopia",
  #                                sphod > -0.5 & sphod < 0.5 ~ "none",
  #                                sphod < 3 & sphod >= 0.5 ~ "low-mod hyperopia",
  #                                sphod >= 3 ~ "high hyperopia",
  #                             TRUE ~ NA_character_), 
  #        # xtabs(data=data,~sphod+sphereeq_od.f,addNA=T)
  #        sphereeq_od.4f=factor(case_when(sphereeq_od.f %in% c("high myopia","mod myopia")~"mod-high myopia",
  #                                        sphereeq_od.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
  #                                     TRUE ~ sphereeq_od.f),
  #                           levels = c("none","low myopia","mod-high myopia","hyperopia")),
  #        sphereeq_os.f=case_when(sphos <= -6 ~ "high myopia",
  #                                sphos > -6 & sphos <= -3 ~ "mod myopia",
  #                                sphos > -3 & sphos <= -0.5 ~ "low myopia",
  #                                sphos > -0.5 & sphos < 0.5 ~ "none",
  #                                sphos < 3 & sphos >= 0.5 ~ "low-mod hyperopia",
  #                                sphos >= 3 ~ "high hyperopia",
  #                                TRUE ~ NA_character_), 
  #        # xtabs(data=data,~sphos+sphereeq_os.f,addNA=T)
  #        sphereeq_os.4f=factor(case_when(sphereeq_os.f %in% c("high myopia","mod myopia")~"mod-high myopia",
  #                                        sphereeq_os.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
  #                                        TRUE ~ sphereeq_os.f),
  #                              levels = c("none","low myopia","mod-high myopia","hyperopia")))

# creating a dataset for IDRE
# ts <- data %>%
#   select(studyno, mean_nucopod, mean_nucopos, mean_corod, mean_coros, mean_psod, mean_psos,
#          agecat, sex1, kitchen.f, fuel.f, ses.f, educ.f, educ.f, potobac.f, packall.f, uv_yrs.f, sphereeq_od.4f,
#          sphereeq_os.4f)
# 
# write_csv(ts, here("data","idre_data.csv"))

#### data checks ####
# VA AND CATARACT
# nucopod.int nucopos.int nucolod.int nucolos.int corod.int coros.int psod.int psos.int
ggplot(data=data, aes(x=mean_nucopod, y=bcvaod.f)) + geom_boxplot()
ggplot(data=data, aes(x=nucopos.int, y=bcvaos.f)) + geom_boxplot()
ggplot(data=data, aes(x=corod.int, y=bcvaod.f)) + geom_boxplot()
ggplot(data=data, aes(x=coros.int, y=bcvaod.f)) + geom_boxplot()
ggplot(data=data, aes(x=psod.int, y=bcvaod.f)) + geom_boxplot()
ggplot(data=data, aes(x=psos.int, y=bcvaod.f)) + geom_boxplot() 
# these all seem to track with worse VA with worsening cataract severity

# DUPLICATES
data %>% group_by(studyno) %>% mutate(dups=n()) %>% ungroup() %>% summarise(dups=sum(dups>1)) # no dups

#### CREATING A PER EYE DATASET ####
# inserting a _ before each os/od to make separation easier
colnames(data) <- gsub("od","_od",colnames(data),fixed = T)
colnames(data) <- gsub("os","_os",colnames(data),fixed = T)
# view(data)

data.eye <- data %>%
  # renaming vars so od/os is at the end of each name
  dplyr::rename(mean.nucop_od=mean_nucop_od, mean.nucop_os=mean_nucop_os, 
                mean.nucol_od=mean_nucol_od,
                mean.nucol_os=mean_nucol_os, mean.cor_od=mean_cor_od, 
                mean.cor_os=mean_cor_os, mean.ps_od=mean_ps_od,
                mean.ps_os=mean_ps_os, nucop.bin_od=nucop_od.bin, 
                nucop.bin_os=nucop_os.bin, nuccol.bin_od=nuccol_od.bin,
                nuccol.bin_os=nuccol_os.bin, cor.bin_od=cor_od.bin, 
                cor.bin_os=cor_os.bin, ps.bin_od=ps_od.bin, ps.bin_os=ps_os.bin,
                mixed.bin_od=mixed_od.bin, mixed.bin_os=mixed_os.bin, 
                nucop.int_od=nucop_od.int, nucop.int_os=nucop_os.int, 
                nucol.int_od=nucol_od.int, nucol.int_os=nucol_os.int, 
                cor.int_od=cor_od.int, cor.int_os=cor_os.int,
                ps.int_od=ps_od.int, ps.int_os=ps_os.int,
                # renaming the things that were accidentally split by the above code
                vcode=vc_ode, hcode=hc_ode, wkcode=wkc_ode, 
                occucode=occuc_ode, occucodesimple=occuc_ode_simple, 
                radexpos=radexp_os, bpdios=bpdi_os, glucose=gluc_ose, 
                bcva.f_od=bcva_od.f, bcva.f_os=bcva_os.f,
                bcva.n_os=bcva_os.n, bcva.n_od=bcva_od.n) %>%
  # selecting all eye vars
  dplyr::select(dplyr::contains("_od"), dplyr::contains("_os"), everything()) %>%
  # mutating everything to a character so it combines into a single column in the pivot_longer
  mutate_at(vars(dplyr::contains("_od")), ~as.character(.)) %>%
  mutate_at(vars(dplyr::contains("_os")), ~as.character(.)) %>%
  pivot_longer(vasc_od:bcva.n_os, 
               names_to = c("var","eye"), 
               names_sep = "_", 
               values_to = "value") %>%
  # pivoting wider - should get 798 * 2 = 1596
  pivot_wider(names_from = "var", values_from = "value") %>%
  mutate(nuccol.bin=as.numeric(nuccol.bin),
         cor.bin=as.numeric(cor.bin),
         ps.bin=as.numeric(ps.bin),
         mixed.bin=as.numeric(mixed.bin),
         mean.nucop=as.numeric(mean.nucop),
         mean.cor=as.numeric(mean.cor),
         mean.ps=as.numeric(mean.ps),
         mean.nucol=as.numeric(mean.nucol),
         sphereeq=as.numeric(sphereeq),
         sphereeq.f=case_when(sphereeq <= -6 ~ "high myopia",
                              sphereeq > -6 & sphereeq <= -3 ~ "mod myopia",
                              sphereeq > -3 & sphereeq <= -0.5 ~ "low myopia",
                              sphereeq > -0.5 & sphereeq < 0.5 ~ "none",
                              sphereeq < 3 & sphereeq >= 0.5 ~ "low-mod hyperopia",
                              sphereeq >= 3 ~ "high hyperopia",
                              TRUE ~ NA_character_), 
         # xtabs(data=data.eye,~sphereeq+sphereeq.f,addNA=T)
         sphereeq.4f=factor(case_when(sphereeq.f %in% c("high myopia","mod myopia")~"mod-high myopia",
                                      sphereeq.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
                                      TRUE ~ sphereeq.f),
                            levels = c("none","low myopia","mod-high myopia","hyperopia")),
         bcva.n=as.numeric(bcva.n))
data.eye

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
  # xtabs(data=data.eye,~sphereeq.4f,addNA=T)
  summarise(total=sum(!is.na(eye)),
            n_modhighMyopia=sum(sphereeq.4f=="mod-high myopia"), p_modhighMyopia=p(n_modhighMyopia,total),
            # n_Myopia_Moderate=sum(sphereeq.f=="mod myopia"), p_Myopia_Moderate=p(n_Myopia_Moderate,total),
            n_LowMyopia=sum(sphereeq.4f=="low myopia"), p_LowMyopia=p(n_LowMyopia,total),
            n_Normal=sum(sphereeq.4f=="none"), p_Normal=p(n_Normal,total),
            n_Hyperopia=sum(sphereeq.4f=="hyperopia"), p_Hyperopia=p(n_Hyperopia,total)) %>%
  # n_Hyperopia_High=sum(sphereeq.f=="high hyperopia"), p_Hyperopia_High=p(n_Hyperopia_High,total)) %>%
  pivot_longer(n_modhighMyopia:p_Hyperopia, 
               names_to = c("np","severity"), 
               names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         p=per(p)) %>%
  unite("N (%)", n, p , sep = " ") %>%
  mutate(severity=case_when(severity=="modhighMyopia"~"Moderate to high myopia",
                            severity=="LowMyopia"~"Low myopia",
                            severity=="Normal"~"None",
                            severity=="Hyperopia"~"Hyperopia")) %>%
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
  # xtabs(data=data.eye,~sphereeq.4f,addNA=T)
  summarise(total=sum(!is.na(eye)),
            n_modhighMyopia=sum(sphereeq.4f=="mod-high myopia"), p_modhighMyopia=p(n_modhighMyopia,total),
            # n_Myopia_Moderate=sum(sphereeq.f=="mod myopia"), p_Myopia_Moderate=p(n_Myopia_Moderate,total),
            n_LowMyopia=sum(sphereeq.4f=="low myopia"), p_LowMyopia=p(n_LowMyopia,total),
            n_Normal=sum(sphereeq.4f=="none"), p_Normal=p(n_Normal,total),
            n_Hyperopia=sum(sphereeq.4f=="hyperopia"), p_Hyperopia=p(n_Hyperopia,total)) %>%
  # n_Hyperopia_High=sum(sphereeq.f=="high hyperopia"), p_Hyperopia_High=p(n_Hyperopia_High,total)) %>%
  pivot_longer(n_modhighMyopia:p_Hyperopia, 
               names_to = c("np","severity"), 
               names_sep = "_", values_to = "values") %>%
  pivot_wider(names_from = np, values_from = values) %>%
  mutate(p=round(p, digits=1),
         p=per(p)) %>%
  unite("N (%)", n, p , sep = " ") %>%
  mutate(severity=case_when(severity=="modhighMyopia"~"Moderate to high myopia",
                            severity=="LowMyopia"~"Low myopia",
                            severity=="Normal"~"None",
                            severity=="Hyperopia"~"Hyperopia")) %>%
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
         fuel.f, packall.f, etoh3f, potobac.f, uv_yrs.f, eye, sphereeq.4f, 
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

# modeling the univariables associations
uni <- gls(score ~ 0 + cattype + cattype:sphereeq.4f, # iterating through all variables here
               weights = varIdent(form=~1|cattype),
               correlation = corSymm(form=~1 | studyno),
               data = filter(data.long, !is.na(sphereeq.4f))) # iterating

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
# est

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
# uni.table

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
m.conf <- gls(score ~ 0 + cattype + cattype:fuel.f + cattype:sphereeq.4f,
           weights = varIdent(form=~1|cattype),
           correlation = corSymm(form=~1 | studyno),
           data = filter(data.long, !is.na(fuel.f) & !is.na(sphereeq.4f)))

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
                 cattype:packall.f + cattype:potobac.f + cattype:uv_yrs.f + cattype:sphereeq.4f,
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
mwithout <- gls(score ~ 0 + cattype + cattype:agecat + cattype:sex1 + cattype:kitchen.f + # cattype:fuel.f + 
                  cattype:ses.f + cattype:educ.f + cattype:bmicat + 
                  cattype:potobac.f + cattype:packall.f + cattype:uv_yrs.f + cattype:sphereeq.4f,
                weights = varIdent(form=~1|cattype),
                correlation = corSymm(form=~1 | studyno),
                data = filter(data.long, !is.na(fuel.f) & !is.na(ses.f) & !is.na(educ.f)
                              & !is.na(bmicat) & !is.na(kitchen.f) & !is.na(packall.f)
                              & !is.na(potobac.f) & !is.na(uv_yrs.f)))

# lrt
lmtest::lrtest(mwithout, m.final)
anova(mwithout, m.final)
anova(mwithout, m.final)
anova(m.final, )

# sex1, agecat, ses.f, ses.of, educ.f, occu.f, bmicat, map.3f, kitchen.f,
# fuel.f, packall.f, etoh3f, potobac.f, uv_yrs.f, eye, sphereeq.4f, 
# mean.nucop, mean.cor, mean.ps

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

#### figure ####
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
                       grepl("sphereeq.4f", parameter)~"Refractive error",
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
                             parameter == "sphereeq.4fhyperopia" ~ "Hyperopia",
                             parameter == "sphereeq.4flow myopia" ~ "Low myopia",
                             parameter == "sphereeq.4fmod-high myopia" ~ "Moderate-\nhigh myopia",
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
                                        "No error","Hyperopia", "Low myopia", "Moderate-\nhigh myopia")), # ref error
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

#### trying a multivariate model ####
mv.model <- lm(cbind(mean_nucop_od, mean_nucop_os, mean_cor_od, mean_cor_os, mean_ps_od, mean_ps_os) ~ 
                  fuel.f + uv_yrs.f + agecat + sex1 + ses.f + educ.f + bmicat + kitchen.f + packall.f + potobac.f,
               data = data)  
summary(mv.model)

car::linearHypothesis(mv.model, 
                      c("fuel.fpropane", "fuel.fwood"))

