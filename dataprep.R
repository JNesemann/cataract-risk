# Data cleaning and prep
# John Nesemann

# packages
library(here)
library(tidyverse)

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
         fuel.f=factor(fuel.f, levels = c("wood","kerosene", "propane")),
         # creating clean vs unclean fuel
         fuel.2f = factor(case_when(fuel.f %in% c("kerosene", "wood") ~ "unclean",
                                    fuel.f == "propane" ~ "clean",
                                    is.na(fuel.f) ~ NA_character_),
                          levels = c("unclean", "clean")),
         # xtabs(data=data, ~fuel.f + fuel.2f, addNA=T)
         # age categories xtabs(data=data,~age_house,addNA=T)
         agecat=case_when(age_house<40~"35-39",
                          age_house<45~"40-44",
                          age_house<50~"45-49",
                          age_house>=50~"50+",
                          is.na(age_house)~NA_character_), 
         # xtabs(data=data,~agecat+age_house,addNA=T)
         agecat.of=factor(agecat, ordered = T, levels = c("35-39","40-44","45-49","50+")),
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
         # visual acuity
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
         bcvaod.3f=factor(case_when(bcvaod.f %in% c("20/10", "20/12.5", "20/15") ~ "bt20/20",
                                    bcvaod.f == "20/20" ~ "20/20",
                                    bcvaod.f %in% c("20/25", "20/30", "20/40", "20/50") ~ "wt20/20",
                                    is.na(bcvaod.f) ~ NA_character_),
                          levels = c("20/20", "wt20/20", "bt20/20")),
         # xtabs(data=data, ~bcvaod.f+bcvaod.3f, addNA=T)
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
         bcvaos.3f=factor(case_when(bcvaos.f %in% c("20/10", "20/12.5", "20/15") ~ "bt20/20",
                                    bcvaos.f == "20/20" ~ "20/20",
                                    bcvaos.f %in% c("20/25", "20/30", "20/40", "20/50") ~ "wt20/20",
                                    is.na(bcvaos.f) ~ NA_character_),
                          levels = c("20/20", "wt20/20", "bt20/20")),
         # xtabs(data=data, ~bcvaos.f+bcvaos.3f, addNA=T)
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
         # generating a BCVA at the person level
         bcva.n = case_when(bcvaod.n < bcvaos.n ~ bcvaod.n,
                            bcvaos.n < bcvaod.n ~ bcvaos.n,
                            bcvaos.n == bcvaod.n ~ bcvaod.n),
         bcva.3f = case_when(bcva.n %in% c(1, 2, 3) ~ "bt20/20",
                             bcva.n == 4 ~ "20/20",
                             bcva.n %in% c(5, 6, 7, 8) ~ "wt20/20"),
         # xtabs(data=data, ~bcva.3f, addNA=T)
         # recoding sex1 as factor
         sex1=factor(sex1, levels= c("Female","Male")),
         # selecting worse cataract severity and refractive error per person
         # xtabs(data=data,~nucop.od)
         nucop=case_when(mean_nucopod > mean_nucopos ~ mean_nucopod,
                         mean_nucopos > mean_nucopod ~ mean_nucopos,
                         mean_nucopod == mean_nucopos ~ mean_nucopod,
                         is.na(mean_nucopod) ~ mean_nucolos,
                         is.na(mean_nucopos) ~ mean_nucolod,
                         is.na(mean_nucopod) & is.na(mean_nucopos) ~ NA_real_), # xtabs(dat=data,~nucop,addNA=T)
         cor=case_when(mean_corod > mean_coros ~ mean_corod,
                       mean_coros > mean_corod ~ mean_coros,
                       mean_corod == mean_coros ~ mean_corod,
                       is.na(mean_corod) ~ mean_coros,
                       is.na(mean_coros) ~ mean_corod,
                       is.na(mean_corod) & is.na(mean_coros) ~ NA_real_), # xtabs(dat=data,~cor,addNA=T)
         ps=case_when(mean_psod > mean_psos ~ mean_psod,
                      mean_psos > mean_psod ~ mean_psos,
                      mean_psod == mean_psos ~ mean_psod,
                      is.na(mean_psod) ~ mean_psos,
                      is.na(mean_psos) ~ mean_psod,
                      is.na(mean_psod) & is.na(mean_psos) ~ NA_real_), # xtabs(dat=data,~ps,addNA=T)
         # xtabs(data=data,~sphereeqod,addNA=T)
         sphere=case_when(sphereeqod>sphereeqos ~ sphereeqod,
                          sphereeqos>sphereeqod ~ sphereeqos,
                          sphereeqod==sphereeqos ~ sphereeqod,
                          is.na(sphereeqod) ~ sphereeqos,
                          is.na(sphereeqos) ~ sphereeqod,
                          is.na(sphereeqod) & is.na(sphereeqos) ~ NA_real_), # xtabs(data=data,~sphere,addNA=T)
         sphere.f=case_when(sphere <= -6 ~ "high myopia",
                            sphere > -6 & sphere <= -3 ~ "mod myopia",
                            sphere > -3 & sphere <= -0.5 ~ "low myopia",
                            sphere > -0.5 & sphere < 0.5 ~ "none",
                            sphere < 3 & sphere >= 0.5 ~ "low-mod hyperopia",
                            sphere >= 3 ~ "high hyperopia",
                            TRUE ~ NA_character_), # xtabs(data=data,~sphereeq+sphereeq.f,addNA=T)
         sphere.4f=factor(case_when(sphere.f %in% c("high myopia","mod myopia")~"mod-high myopia",
                                    sphere.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
                                    TRUE ~ sphere.f),
                          levels = c("none","low myopia","mod-high myopia","hyperopia")),
         sphere.2f=factor(case_when(sphere.f %in% c("high myopia","mod myopia","low myopia")~"myopia",
                                    sphere.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
                                    TRUE ~ sphere.f),
                          levels = c("none","hyperopia","myopia"))) # xtabs(data=data,~sphere.f+sphere.2f, addNA=T)

#### data checks ####
# VA AND CATARACT
# nucopod.int nucopos.int nucolod.int nucolos.int corod.int coros.int psod.int psos.int
# ggplot(data=data, aes(x=mean_nucopod, y=bcvaod.f)) + geom_boxplot()
# ggplot(data=data, aes(x=nucopos.int, y=bcvaos.f)) + geom_boxplot()
# ggplot(data=data, aes(x=corod.int, y=bcvaod.f)) + geom_boxplot()
# ggplot(data=data, aes(x=coros.int, y=bcvaod.f)) + geom_boxplot()
# ggplot(data=data, aes(x=psod.int, y=bcvaod.f)) + geom_boxplot()
# ggplot(data=data, aes(x=psos.int, y=bcvaod.f)) + geom_boxplot() 
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
                bcva.3f_od = bcva_od.3f, bcva.3f_os = bcva_os.3f,
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
  # unselecting the person level bcva as it results in duplicate column names
  select(-bcva.3f, -bcva.n) %>%
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
         sphere.2f=factor(case_when(sphereeq.f %in% c("high myopia","mod myopia","low myopia")~"myopia",
                                    sphereeq.f %in% c("low-mod hyperopia","high hyperopia")~"hyperopia",
                                    TRUE ~ sphereeq.f),
                          levels = c("none","hyperopia","myopia")),
         # xtabs(data=data,~sphereeq.4f+sphere.2f, addNA=T)
         bcva.n=as.numeric(bcva.n))
data.eye

# checking for duplicates
data.eye %>% group_by(studyno) %>% mutate(dups=n()) %>% ungroup() %>% summarise(dups=sum(dups>2)) # no dups
