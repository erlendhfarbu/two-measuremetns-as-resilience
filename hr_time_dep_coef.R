tu <- readRDS("~/postdocHC/tu.rds")

tu2 <- tu %>% dplyr::select(Mean_hr_t4, d_hr_t4, hr_t41, death, follow_up_time_t42, age_t4, abs_d_hr_t4, sex_t4, 
                        betweenT4visits, dic_betweenvisits, dic_betweenvisits_20, dic_betweenvisits_60)
#tu2 <- subset(tu2, age_t4<65)
tu2 <- subset(tu2, betweenT4visits<121)
#tu2 <- subset(tu2, d_hr_t4>=0)
#tu2 <- subset(tu2, d_hr_t4<=0)

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
head(tdata, n=50L)
tdata$year <- round(tdata$tstop, digits=0)
tdata <- tdata %>% mutate(int_hr_time = hr_t41*year)

dd <- datadist(tdata)

options(datadist = "dd")
# fit <- cph(Surv(tstart, tstop, death) ~  
#              + abs_d_hr_t4
#             + rcs(c.age, 3)
#            + hr_t41*rcs(c.age, 3)#interaction with age. No indication of nonlinear. calculating a normal interactionterm
#            + strat(sex_t4)#*ns(c.age, 3)
#            ,data = tdata)
# fit1 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hr_t4
#             + abs_d_hr_t4#*rcs(c.age, 3) #No improvement of model with interaction with age. PH-assumption ok in fit 
#             + rcs(c.age, 3)
#             + hr_t41
#             +int_hr_age
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)
# fit2 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_hr_t4, 3)
#             + rcs(abs_d_hr_t4, 3)*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + hr_t41*rcs(c.age, 3)
#             + strat(sex_t4)
#             ,data = tdata)
# fit3 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hr_t4*dic_betweenvisits_60
#             + abs_d_hr_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + hr_t41*rcs(c.age, 3)
#             + strat(sex_t4)
#             ,data = tdata)
# 
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hr_t4*dic_betweenvisits
#             + abs_d_hr_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + hr_t41
#             + abs_d_hr_t4*rcs(c.age, 3)
#             + strat(sex_t4)
#             ,data = tdata)
# 
# 
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hr_t4*dic_betweenvisits_60
#             + abs_d_hr_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + hr_t41
#             + abs_d_hr_t4*rcs(c.age, 3)
#             + strat(sex_t4)
#             ,data = tdata)

  # fit6 <- cph(Surv(tstart, tstop, death) ~  
  #             rcs(abs_d_hr_t4, 3)*betweenT4visits
  #           + rcs(abs_d_hr_t4, 3)#*rcs(c.age, 3)
  #           + rcs(c.age, 3)
  #           + hr_t41*rcs(c.age, 3)
  #           + strat(sex_t4)#*ns(c.age, 3)
  #           ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#               + rcs(abs_d_hr_t4, 3)#*rcs(c.age, 3)
#             + rcs(abs_d_hr_t4, 3)*rcs(betweenT4visits, knots=c(20,60,100)) 
#             + rcs(c.age, 3)
#             + hr_t41*rcs(c.age, 3)
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)

# extractAIC(fit)
# extractAIC(fit1)
# extractAIC(fit2)
# extractAIC(fit3)
# extractAIC(fit4)
# extractAIC(fit5)
# extractAIC(fit6)
# extractAIC(fit7)
# 
# 
# anova(fit5)

fit5 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_hr_t4*dic_betweenvisits_60#*rcs(c.age, 3)
           # + abs_d_hr_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + hr_t41:rcs(c.age, 3)
          + hr_t41
           #+ int_hr_time
            + strat(sex_t4)
            ,data = tdata)
extractAIC(fit5) #Slight improvement when removing int.term 56105.77 vs 56107.26 And p>0.1 for interaction. Remove the interaction term
anova(fit5)
# 
# fit5check <- coxph(Surv(tstart, tstop, death) ~
#               abs_d_hr_t4*dic_betweenvisits_60
#           #  + abs_d_hr_t4*ns(c.age, 3)
#             + ns(c.age, 3)
#           + hr_t41*ns(c.age, 3)
#             + strata(sex_t4)
#             ,data = tdata)
# summary(fit5check)
# check <- cox.zph(fit5check, transform = "identity"); print(check); plot(check, resid=F)

sd_hr <- sd(tu$d_hr_t4, na.rm = T)
mean_hr <- mean(tu$hr_t41, na.rm = T)
mean_int <- mean(tdata$int_hr_time, na.rm = T)

cont_fit5 <- contrast(fit5, 
                      a = list(c.age=55:85, abs_d_hr_t4=sd_hr, sex_t4=0,   dic_betweenvisits_60=1),
                      b =  list(c.age=55:85, abs_d_hr_t4=0, sex_t4=0, dic_betweenvisits_60=1))                    

print(cont_fit5, fun=exp)
cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])

cont_fit52 <- contrast(fit5, 
                       a = list(c.age=55:85, abs_d_hr_t4=sd_hr, sex_t4=0, dic_betweenvisits_60=0),
                       b =  list(c.age=55:85, abs_d_hr_t4=0, sex_t4=0, dic_betweenvisits_60=0))                    

print(cont_fit52, fun=exp)
cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_hr <- cbind(cont_fit5, cont_fit52)

HR_hr <- ggplot(cont_fit_hr, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.10, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Age')  + ggtitle("Heart rate") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
HR_hr
#+ ylab('HR for a 9.8 unit absolute difference')


