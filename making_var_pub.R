source("~/postdocHC/load.R")


tu <- readRDS("~/postdocHC/tu_original.rds")
#colnames to lower case 
colnames(tu) <- tolower(colnames(tu))
head(tu)

#create id
tu$id <- 1:nrow(tu)
tu$education_t4 <- factor(tu$education_t4)

# month of visit 2 in T4
tu$Month_Yr_t42 <- format(as.Date(tu$date_t42), "%Y-%m")
## Follow up time ----
tu$Date_T42 <- as.Date(tu$date_t42, format= "%Y/%m/%d")
tu$Date_T4 <- as.Date(tu$date_t4, format= "%Y/%m/%d")
tu$Dato_Dod <- as.Date(tu$dato_dod, format= "%Y/%m/%d")
tu$Dato_Emigrert <- as.Date(tu$dato_emigrert_em, format= "%Y/%m/%d")


endOfFollowUp <-  pmin(as.Date("2020-12-31"), 
                       tu$Dato_Dod,
                       tu$Dato_Emigrert,
                       na.rm = TRUE)
tu$follow_up_time_t42 <- as.numeric(endOfFollowUp - tu$Date_T42)
tu$follow_up_time_t4 <- as.numeric(endOfFollowUp - tu$Date_T4)

tu$Date_T6 <- as.Date(tu$attendance_date_t62, format= "%Y/%m/%d")
tu$follow_up_time_t6 <- as.numeric(endOfFollowUp - tu$Date_T6)

tu$Date_T7 <- as.Date(tu$attendance_date_d2_t72, format= "%Y/%m/%d")
tu$follow_up_time_t7 <- as.numeric(endOfFollowUp - tu$Date_T7)

#Event
tu$death <- 0
tu$death <- ifelse(!is.na(tu$dato_dod),1,0)

#### Time between visits ####
tu$Date_T41 <- as.Date(tu$date_t4, format = "%Y/%m/%d")
tu$betweenT4visits <- as.numeric(tu$Date_T42-tu$Date_T41)
hist(tu$betweenT4visits, breaks = 120, main = paste("Histogram of time between visit 1 and 2 in Tromsø4"),
     xlab = "Time in days")


tu$Date_T6 <- as.Date(tu$attendance_date_t6, format= "%Y/%m/%d")
tu$betweenT6visits <- as.numeric(tu$attendance_date_t62-tu$attendance_date_t6)
hist(tu$betweenT6visits, breaks = 100)

# FOR THIS ANALYSIS ALL HAVING LONGER THAN 120 DAYS BETWEEN T4 VISITS ARE EXCLUDED
tu <- subset(tu, betweenT4visits<=120)
#tu <- subset(tu, age_t4>70)


## agegroups ----
tu <- tu %>%  mutate(agegroups_t4= case_when(age_t4 <30 ~ 0,
                                             age_t4<35 & age_t4 >=30 ~ 1,
                                             age_t4<40 & age_t4 >=35 ~ 2,
                                             age_t4<45 & age_t4 >=40 ~ 3,
                                             age_t4<50 & age_t4 >=45 ~ 4,
                                             age_t4<55 & age_t4 >=50 ~ 5,
                                             age_t4<60 & age_t4 >=55 ~ 6,
                                             age_t4<65 & age_t4 >=60 ~ 7,
                                             age_t4<70 & age_t4 >=65 ~ 8,
                                             age_t4<75 & age_t4 >=70 ~ 9,
                                             age_t4<80 & age_t4 >=75 ~ 10,
                                             age_t4 >=80 ~ 11
)) 

tu <- tu %>%  mutate(agegroups_t6= case_when(age_t6<40 ~ 0,
                                             age_t6<45 & age_t6 >=40 ~ 1,
                                             age_t6<50 & age_t6 >=45 ~ 2,
                                             age_t6<55 & age_t6 >=50 ~ 3,
                                             age_t6<60 & age_t6 >=55 ~ 4,
                                             age_t6<65 & age_t6 >=60 ~ 5,
                                             age_t6<70 & age_t6 >=65 ~ 6,
                                             age_t6<75 & age_t6 >=70 ~ 7,
                                             age_t6<80 & age_t6 >=75 ~ 8,
                                             age_t6 >=80 ~ 9
)) 
tu <- tu %>%  mutate(agegroups_t7= case_when(age_t7<40 ~ 0,
                                             age_t7<45 & age_t7 >=40 ~ 1,
                                             age_t7<50 & age_t7 >=45 ~ 2,
                                             age_t7<55 & age_t7 >=50 ~ 3,
                                             age_t7<60 & age_t7 >=55 ~ 4,
                                             age_t7<65 & age_t7 >=60 ~ 5,
                                             age_t7<70 & age_t7 >=65 ~ 6,
                                             age_t7<75 & age_t7 >=70 ~ 7,
                                             age_t7<80 & age_t7 >=75 ~ 8,
                                             age_t7 >=80 ~ 9
)) 

#create a mean for visit 2
tu <- tu %>% 
  group_by(id )%>% 
  mutate(mean_diabp_t42 = mean(c( diabp2_t42, diabp3_t42)))
tu <- tu %>% 
  group_by(id )%>% 
  mutate(mean_sysbp_t42 = mean(c( sysbp2_t42, sysbp3_t42)))
tu$hr_t3 <- (tu$pulse1_t3 +tu$pulse2_t3 +tu$pulse3_t3)/3
# tu$hr_t41 <- (tu$pulse1_t4 +tu$pulse2_t4 +tu$pulse3_t4)/3
# tu$hr_t42 <- (tu$pulse1_t42 +tu$pulse2_t42 +tu$pulse3_t42)/3
#Alternative 
tu <- tu %>% 
  group_by(id )%>% 
  mutate(hr_t41 = mean(c(pulse2_t4, pulse3_t4)))
tu <- tu %>% 
  group_by(id )%>% 
  mutate(hr_t42 = mean(c(pulse2_t42, pulse3_t42)))
#Calculating number of diseases and medications in Tromso4
tu <- tu %>% group_by(id) %>% mutate(number_diseases = sum(
  angina_t4, 
  heart_attack_t4, 
  asthma_t4, 
  diabetes_t4, 
  stroke_t4))
tu <- tu %>% group_by(id) %>% mutate(number_med = sum(
  bplow_q1_atc_t4,
  lipidlow_q1_atc_t4
))
#Smoking 
tu <- tu %>%  group_by(id) %>% mutate(smoking_t4 = case_when(cigarettes_t4==1 ~ 1,
                                                             cigar_t4==1 ~ 1,
                                                             pipe_t4==1 ~1,
                                                             cigarettes_t4==0 & cigar_t4==0 & pipe_t4==0 ~0))
#Calculating LDL in T4 and T42 using
#Friedewald’s formula: LDL cholesterol = total cholesterol – high-density lipoprotein cholesterol – (0.45 × triglycerides).

tu <- tu %>% group_by(id) %>% mutate(ldl_t4 = cholesterol_t4 - hdl_t4 - (0.45*triglycerides_t4))
tu <- tu %>% group_by(id) %>% mutate(ldl_t42 = cholesterol_t42 - hdl_t42 - (0.45*triglycerides_t42))

#Making a dichotomous variable for education and childhood economy
tu <- tu %>%  ungroup() %>% mutate(edu_t4_dic = case_when(education_t4==1 ~ 0,
                                                          education_t4==2 ~0,
                                                          education_t4==3~0,
                                                          education_t4==4~1,
                                                          education_t4==5~1))

table(tu$education_t4, useNA = "always")  
table(tu$edu_t4_dic, useNA = "always")  
tu <- tu %>% ungroup() %>% mutate(edu_t4 = case_when(education_t4==1 ~1,
                                                     education_t4==2~2,
                                                     education_t4==3~2,
                                                     education_t4==4~3,
                                                     education_t4==5~3))
tu <- tu %>% ungroup() %>% mutate(child_eco_t4_dic = case_when(childhood_economy_t4<=2 ~1,
                                                               childhood_economy_t4==3~0,
                                                               childhood_economy_t4==4~0))

table(tu$edu_t4, useNA = "always")  

#### cut-offs
tu <- tu %>% mutate(dic_betweenvisits = case_when(betweenT4visits<41 ~ 0,
                                                  betweenT4visits>=41 ~1))

tu <- tu %>% mutate(dic_betweenvisits_60 = case_when(betweenT4visits<60 ~ 0,
                                                     betweenT4visits>=60 ~1))
tu <- tu %>% mutate(dic_betweenvisits_20 = case_when(betweenT4visits<20 ~ 0,
                                                     betweenT4visits>=20 ~1))

### Creating an alcohol variable
tu <- tu %>% ungroup() %>% mutate(alcohol_t4 = case_when(teetotaller_t4==1 ~ 0,
                                                         teetotaller_t4==0 ~ beer_glasses_t4 + wine_glasses_t4 + spirits_glasses_t4))

tu <- tu %>% ungroup() %>% mutate(alco_quart = ntile(alcohol_t4, 4))
tu$alco_quart <- factor(tu$alco_quart)

# creating one dataset for short and long between visits and female/male
long_between_visits <- subset(tu, betweenT4visits>=41)
short_between_visits <- subset(tu, betweenT4visits<41)
male <- subset(tu, sex_t4==1)
female <- subset(tu, sex_t4==0)


create_quartiles <- function(df, name, sex, visit1, visit2, tromsostudy, agegroups){
  visit1 <-  enquo(visit1)
  visit2 <-  enquo(visit2)
  name <-  enquo(name)
  sex <- enquo(sex)
  tromsostudy <-  enquo(tromsostudy)
  
  df <- df %>% 
    group_by(id) %>%
    mutate( d = (({{visit2}})) - ({{visit1}}))
  
  df <- df %>% 
    group_by(id) %>%
    mutate( d2 =  (({{visit2}})) - ({{visit1}}))
  
  df <- df %>% ungroup() %>% mutate(sd2 = d2- mean(c(d2), na.rm=T))
  df <- df %>% 
    group_by(id) %>%
    mutate(s_abs_d = abs(sd2))
  df <- df %>% 
    ungroup() %>%
    mutate(s_abs_d2 = abs(scale(c(d2))))
  
  df <- df %>% 
    group_by(id) %>%
    mutate( stdv = sd(c(({{visit1}}),  ({{visit2}}))))
  
  df <- df %>% 
    group_by(id) %>%
    mutate( Mean = mean(c(({{visit1}}),  ({{visit2}}))))
  
  df <- df %>% 
    group_by(id) %>%
    mutate( cv = sd(c(({{visit1}}),  ({{visit2}})))/ mean(c(({{visit1}}),  ({{visit2}}))))
  #sex-specific quartiles for cv 
  df <- df %>%
    group_by(({{sex}})) %>% 
    group_by(({{agegroups}})) %>% 
    mutate(quart_cv = ntile(cv, 8))
  
  # sex-specific quartiles of mean
  df <- df %>%
    group_by(({{sex}})) %>% 
    group_by(({{agegroups}})) %>% 
    mutate(quart_mean = ntile(Mean, 8))
  
  # sex-specific quartiles of difference
  df <- df %>%
    group_by(({{sex}})) %>% 
    group_by(({{agegroups}})) %>% 
    mutate(percentiles_d = ntile(d, 8))
  
  df <- df %>%
    group_by(({{sex}})) %>% 
    group_by(({{agegroups}})) %>% 
    mutate(quart_d = ntile(d, 5))
  
  # dummy for highest quartile for cv
  df <- df %>%
    mutate(quart4_cv = case_when(
      quart_cv == 8 ~ 1,
      quart_cv == 7 ~ 1,
      quart_cv == 6 ~ 0,
      quart_cv == 5 ~ 0,
      quart_cv == 4 ~ 0,
      quart_cv == 3 ~ 0,
      quart_cv == 2 ~ 0,
      quart_cv == 1 ~ 0)) 
  
  # dummy for highest quartile for mean
  df <- df %>%
    mutate(quart4_mean = case_when(
      quart_mean == 1 ~ 0,
      quart_mean == 2 ~ 0,
      quart_mean == 3 ~ 0,
      quart_mean == 4 ~ 0,
      quart_mean == 5 ~ 0,
      quart_mean == 6 ~ 0,
      quart_mean == 7 ~ 1,
      quart_mean == 8 ~ 1)) 
  
  # dummy for highest quartile of difference
  df <- df %>%
    mutate(quart4_d = case_when(
      percentiles_d == 1 ~ 1,
      percentiles_d == 2 ~ 0,
      percentiles_d == 3 ~ 0,
      percentiles_d == 4 ~ 0,
      percentiles_d == 5 ~ 0,
      percentiles_d == 6 ~ 0,
      percentiles_d == 7 ~ 0,
      percentiles_d == 8 ~ 1)) 
  
  #composite of mean and cv
  df <- df %>% 
    mutate(composite_mean_cv = case_when(
      quart4_mean==0 & quart4_cv==0 ~ 0, 
      quart4_mean==1 & quart4_cv==0 ~ 1,
      quart4_mean==0 & quart4_cv==1 ~ 2,
      quart4_mean==1 & quart4_cv==1 ~ 3, 
    ))
  
  #composite of mean and d
  df <- df %>% 
    mutate(composite_mean_d = case_when(
      quart4_mean==0 & quart4_d==0 ~ 0, 
      quart4_mean==1 & quart4_d==0 ~ 1,
      quart4_mean==0 & quart4_d==1 ~ 2,
      quart4_mean==1 & quart4_d==1 ~ 3, 
    ))
  df <- df %>% mutate(perc  = abs(d)/pmax((({{visit2}})), ({{visit1}})))
  #renaming variables
  df <- df %>% 
    rename("d_{{name}}_{{tromsostudy}}":= d)
  df <- df %>% 
    rename("abs_d_{{name}}_{{tromsostudy}}":= s_abs_d)
  df <- df %>% 
    rename("scaled_d_{{name}}_{{tromsostudy}}":= s_abs_d2)
  df <- df %>% 
    rename("sd_{{name}}_{{tromsostudy}}":= stdv)
  df <- df %>% 
    rename("Mean_{{name}}_{{tromsostudy}}":= Mean)
  df <- df %>% 
    rename("cv_{{name}}_{{tromsostudy}}":= cv)
  df <- df %>% 
    rename("quart_cv_{{name}}_{{tromsostudy}}":= quart_cv)
  df <- df %>% 
    rename("quart4_cv_{{name}}_{{tromsostudy}}":= quart4_cv)
  df <- df %>% 
    rename("quart_mean_{{name}}_{{tromsostudy}}":= quart_mean)
  df <- df %>% 
    rename("quart4_mean_{{name}}_{{tromsostudy}}":= quart4_mean)
  df <- df %>% 
    rename("quart_d_{{name}}_{{tromsostudy}}":= quart_d)
  df <- df %>% 
    rename("percentiles_d_{{name}}_{{tromsostudy}}":= percentiles_d)
  df <- df %>% 
    rename("quart4_d_{{name}}_{{tromsostudy}}":= quart4_d)
  df <- df %>% 
    rename("composite_mean_cv_{{name}}_{{tromsostudy}}":= composite_mean_cv)
  df <- df %>% 
    rename("composite_mean_d_{{name}}_{{tromsostudy}}":= composite_mean_d)
  df <- df %>% 
    rename("perc_d_{{name}}_{{tromsostudy}}":= perc)
}
tu <- tu %>% ungroup()
tu <- create_quartiles(tu, diabp, sex_t4, mean_diabp_t4, mean_diabp_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, sysbp, sex_t4, mean_sysbp_t4, mean_sysbp_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, hr, sex_t4, hr_t41, hr_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, chol, sex_t4, cholesterol_t4, cholesterol_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, trig, sex_t4, triglycerides_t4, triglycerides_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, hdl, sex_t4, hdl_t4, hdl_t42, t4, agegroups_t4)
#tu <- create_quartiles(tu, ldl, sex_t4, ldl_t4, ldl_t42, t4, agegroups_t4)
tu <- create_quartiles(tu, ggt, sex_t4, ggt_t4, ggt_t42, t4, agegroups_t4)

tu <- tu %>% group_by(id) %>% mutate(sum_abs_d=sum(c(scaled_d_diabp_t4
                                                     ,scaled_d_sysbp_t4
                                                     ,scaled_d_trig_t4
                                                     , scaled_d_chol_t4
                                                     ,scaled_d_ggt_t4
                                                     ,scaled_d_hdl_t4
                                                     , scaled_d_hr_t4
)))




tu <- tu %>% mutate(time_quart2 = case_when(betweenT4visits<20 ~ 1,
                                            betweenT4visits>=20 &  betweenT4visits<60 ~ 2,
                                            betweenT4visits>=60 ~ 3,))

tu <- tu %>% ungroup() %>% mutate(time_quart = ntile(betweenT4visits, 4))
tu$time_quart <- factor(tu$time_quart)
tu$time_quart2 <- factor(tu$time_quart2)
table(tu$time_quart, tu$time_quart2)

tu <- tu %>% group_by(sex_t4) %>% 
  group_by(agegroups_t4) %>% 
  mutate(quint_sum_abs_d = ntile(sum_abs_d, 5))

saveRDS(tu,file="tu.rds")



