tu <- readRDS("~/postdocHC/tu.rds")


#### Interaction with self-defined categories of time between visits ####
sum <- tu %>% dplyr::select(sum_abs_d, mean_diabp_t4, death, follow_up_time_t42, age_t4,  sex_t4, 
                            ggt_t4, ggt_t42, mean_sysbp_t4, triglycerides_t4, cholesterol_t4, hr_t41
                            ,hdl_t4, childhood_economy_t4, education_t4, health_t4, cigarettes_t4, bmi_t4, smoking_t4,
                            phys_activity_leisure_hard_t4, number_diseases , number_med,betweenT4visits, 
                            angina_t4, heart_attack_t4, asthma_t4, diabetes_t4, dic_betweenvisits, dic_betweenvisits_20, stroke_t4, time_quart, time_quart2
)
sum <- sum %>% drop_na(sum_abs_d, mean_diabp_t4, death, follow_up_time_t42, age_t4,  sex_t4, 
                       ggt_t4, ggt_t42,  mean_sysbp_t4, triglycerides_t4, cholesterol_t4, hr_t41
                       ,hdl_t4, childhood_economy_t4, education_t4, health_t4, cigarettes_t4, bmi_t4, smoking_t4,
                       phys_activity_leisure_hard_t4, number_diseases , number_med,betweenT4visits, 
                       angina_t4, heart_attack_t4, asthma_t4, diabetes_t4, dic_betweenvisits, dic_betweenvisits_20, time_quart, time_quart2)
sum <- subset(sum, betweenT4visits<121)

dtimes <- sort(unique(with(sum, ceiling(follow_up_time_t42/365.25)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/365.25), death) ~., sum, cut=dtimes)
tdata$tstart <- tdata$tstart#/10
tdata$tstop <- tdata$tstop#/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
options(datadist = "dd")


model1 <- cph(Surv(tstart, tstop, death) ~  
                sum_abs_d*time_quart2
              + sum_abs_d*rcs(c.age, 3)
              + rcs(c.age, 3)
              + strat(sex_t4)
              , data = tdata)
anova(model1)

model2 <- cph(Surv(tstart, tstop, death) ~  
                sum_abs_d*time_quart2
              + sum_abs_d*rcs(c.age, 3)
              + rcs(c.age, 3)
              + phys_activity_leisure_hard_t4
              + bmi_t4
              + smoking_t4#*rcs(c.age, 3)
              + number_diseases
              + number_med
              + education_t4
              + strat(sex_t4)
              , data = tdata)
anova(model2)

model3 <- cph(Surv(tstart, tstop, death) ~  
                sum_abs_d*time_quart2
              + sum_abs_d*rcs(c.age, 3)
              + rcs(c.age, 3)
              + mean_diabp_t4#*rcs(c.age, 3)
              + mean_sysbp_t4#*rcs(c.age, 3)
              + hr_t41*rcs(c.age, 3)
              + cholesterol_t4
              + hdl_t4
              + triglycerides_t4
              + ggt_t4
              + phys_activity_leisure_hard_t4
              + bmi_t4
              + smoking_t4#*rcs(c.age, 3)
              + number_diseases
              + number_med
              + education_t4
              + strat(sex_t4)
              , data = tdata)
anova(model3)
summary(model3)

sd_sum <- sd(tu$sum_abs_d, na.rm = T)
m_sum = mean(tu$sum_abs_d, na.rm = T)
quintile <- quantile(tu$sum_abs_d, c(.1, .3, .5, .7, .9), na.rm = T)
#### Crude model ####
cont_fit_sum <- contrast(model1, 
                         a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='1'),
                         b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='1'),
)                    

print(cont_fit_sum, fun=exp)
cont_fit_sum <- as.data.frame(cont_fit_sum[c('Contrast','Lower','Upper')])
quart_time <- rep(1, 31)
age <- c(55:85)
cont_fit_sum <- cbind(cont_fit_sum, quart_time, age )
cont_fit_sum2 <- contrast(model1, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='2'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='2')
)                      
print(cont_fit_sum2, fun=exp)
cont_fit_sum2 <- as.data.frame(cont_fit_sum2[c('Contrast','Lower','Upper')])

colnames(cont_fit_sum2) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(2, 31)
cont_fit_sum2 <- cbind(cont_fit_sum2, quart_time, age  )
cont_fit_sum4 <- contrast(model1, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='3'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='3')
)                      
print(cont_fit_sum4, fun=exp)
cont_fit_sum4 <- as.data.frame(cont_fit_sum4[c('Contrast','Lower','Upper')])
colnames(cont_fit_sum4) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(3, 31)
cont_fit_sum4 <- cbind(cont_fit_sum4, quart_time, age  )


cont_fit_sum <- rbind(cont_fit_sum, cont_fit_sum2, cont_fit_sum4)

cont_fit_sum$quart_time <- factor(cont_fit_sum$quart_time)

HR_sum_crude <-   ggplot(cont_fit_sum, aes(x=age, y=exp(Contrast), group=quart_time, colour=quart_time)) + geom_line() 
HR_sum_crude <- HR_sum_crude +  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper), fill=quart_time),
                                            alpha=0.1, linetype=0) +   scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.5,2,3), limits = c(0.5, 3)) +
  xlab('Age')  + ggtitle("Model 1") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8),  legend.position = "none") #+ 
# scale_colour_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days")) + 
#  scale_fill_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days"))

HR_sum_crude

#### Adjusted for behaviours and BMI ####
cont_fit_sum <- contrast(model2, 
                         a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='1'),
                         b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='1'),
)                    

print(cont_fit_sum, fun=exp)
cont_fit_sum <- as.data.frame(cont_fit_sum[c('Contrast','Lower','Upper')])
quart_time <- rep(1, 31)
age <- c(55:85)
cont_fit_sum <- cbind(cont_fit_sum, quart_time, age )
cont_fit_sum2 <- contrast(model2, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='2'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='2')
)                      
print(cont_fit_sum2, fun=exp)
cont_fit_sum2 <- as.data.frame(cont_fit_sum2[c('Contrast','Lower','Upper')])

colnames(cont_fit_sum2) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(2, 31)
cont_fit_sum2 <- cbind(cont_fit_sum2, quart_time, age  )
cont_fit_sum4 <- contrast(model2, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='3'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='3')
)                      
print(cont_fit_sum4, fun=exp)
cont_fit_sum4 <- as.data.frame(cont_fit_sum4[c('Contrast','Lower','Upper')])
colnames(cont_fit_sum4) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(3, 31)
cont_fit_sum4 <- cbind(cont_fit_sum4, quart_time, age  )


cont_fit_sum <- rbind(cont_fit_sum, cont_fit_sum2, cont_fit_sum4)

cont_fit_sum$quart_time <- factor(cont_fit_sum$quart_time)

HR_sum_behav_bmi <-   ggplot(cont_fit_sum, aes(x=age, y=exp(Contrast), group=quart_time, colour=quart_time)) + geom_line() 
HR_sum_behav_bmi <- HR_sum_behav_bmi +  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper), fill=quart_time),
                                                    alpha=0.1, linetype=0) +   scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.5,2,3), limits = c(0.5, 3)) +
  xlab('Age')  + ggtitle("Model 2") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8), legend.position = "none") #+ 
#scale_colour_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days")) + 
#scale_fill_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days"))

HR_sum_behav_bmi

#### FULL MODEL ####

cont_fit_sum <- contrast(model3, 
                         a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='1'),
                         b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='1'),
)                    

print(cont_fit_sum, fun=exp)
cont_fit_sum <- as.data.frame(cont_fit_sum[c('Contrast','Lower','Upper')])
quart_time <- rep(1, 31)
age <- c(55:85)
cont_fit_sum <- cbind(cont_fit_sum, quart_time, age )
cont_fit_sum2 <- contrast(model3, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='2'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='2')
)                      
print(cont_fit_sum2, fun=exp)
cont_fit_sum2 <- as.data.frame(cont_fit_sum2[c('Contrast','Lower','Upper')])

colnames(cont_fit_sum2) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(2, 31)
cont_fit_sum2 <- cbind(cont_fit_sum2, quart_time, age  )
cont_fit_sum4 <- contrast(model3, 
                          a = list(c.age=55:85, sum_abs_d= quintile[5], time_quart2='3'),
                          b = list(c.age=55:85, sum_abs_d = quintile[1], time_quart2='3')
)                      
print(cont_fit_sum4, fun=exp)
cont_fit_sum4 <- as.data.frame(cont_fit_sum4[c('Contrast','Lower','Upper')])
colnames(cont_fit_sum4) <- c("Contrast", "Lower", "Upper")
quart_time <- rep(3, 31)
cont_fit_sum4 <- cbind(cont_fit_sum4, quart_time, age  )


cont_fit_sum <- rbind(cont_fit_sum, cont_fit_sum2, cont_fit_sum4)

cont_fit_sum$quart_time <- factor(cont_fit_sum$quart_time)

HR_sum_full <-   ggplot(cont_fit_sum, aes(x=age, y=exp(Contrast), group=quart_time, colour=quart_time)) + geom_line() 
HR_sum_full <- HR_sum_full +  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper), fill=quart_time),
                                          alpha=0.1, linetype=0) +   scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.5,2,3), limits = c(0.5, 3)) +
  xlab('Age')  + ggtitle("Model 3") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8),  legend.position = "none") #+ 
# scale_colour_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days")) + 
# scale_fill_discrete(name= "Categories of time interval between visits", labels = c("<20 days", "20-60 days", ">60 days"))

HR_sum_full
HR_sum_full_legend <-   ggplot(cont_fit_sum, aes(x=age, y=exp(Contrast), group=quart_time, colour=quart_time)) + geom_line() 
HR_sum_full_legend <- HR_sum_full_legend +  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper), fill=quart_time),
                                                        alpha=0.1, linetype=0) +   scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.5,2,3)) +
  xlab('Age')  + ggtitle("Full model") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 12)) +
  scale_colour_discrete(name= "Categories of time interval \n between visits", labels = c("<20 days", "20-60 days", ">60 days")) +
  scale_fill_discrete(name= "Categories of time interval \n between visits", labels = c("<20 days", "20-60 days", ">60 days")) 

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

shared_legend <- extract_legend(HR_sum_full_legend)


figure_interaction_time <- grid.arrange( 
  HR_sum_crude, HR_sum_behav_bmi, HR_sum_full,shared_legend,
  ncol = 4, nrow = 1, left = "Hazard ratio", bottom = "") 
figure_interaction_time
