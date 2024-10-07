tu <- readRDS("~/postdocHC/tu.rds")


tu2 <- tu %>% dplyr::select(Mean_diabp_t4, mean_diabp_t4, d_diabp_t4, death, follow_up_time_t42, age_t4, abs_d_diabp_t4, sex_t4, 
                        betweenT4visits, dic_betweenvisits_20, dic_betweenvisits, dic_betweenvisits_60, 
                         mean_diabp_t4)
dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age

dd <- datadist(tdata)


options(datadist = "dd")
#options(datadist = NULL)
# fit1 <- cph(Surv(tstart, tstop, death) ~  
#                 abs_d_diabp_t4
#                  + abs_d_diabp_t4*rcs(c.age, 4)
#              + rcs(c.age, 4)
#               + mean_diabp_t4
#               + abs_d_diabp_t4    
#           + strat(sex_t4)
#               ,data = tdata)
# fit2 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_diabp_t4, 3)
#             + rcs(abs_d_diabp_t4, 3)*rcs(c.age, 4)*rcs(betweenT4visits, knots=c(10,20,30))
#             + rcs(c.age, 4)
#             + mean_diabp_t4
#             + strat(sex_t4)
#             ,data = tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_diabp_t4
            + abs_d_diabp_t4*rcs(c.age, 4)
            + rcs(c.age, 4)
            + mean_diabp_t4
            + abs_d_diabp_t4*dic_betweenvisits_20   
            + strat(sex_t4)
            ,data = tdata)
# fit3a <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_diabp_t4, 3)
#             + rcs(abs_d_diabp_t4, 3)*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + mean_diabp_t4
#             + rcs(abs_d_diabp_t4, 3)*dic_betweenvisits_20   
#             + strat(sex_t4)
#             ,data = tdata)
# AIC(fit3)
# AIC(fit3a)
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_diabp_t4
#             + abs_d_diabp_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + mean_diabp_t4
#             + abs_d_diabp_t4*dic_betweenvisits    #
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_diabp_t4
#             + abs_d_diabp_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + mean_diabp_t4
#             + abs_d_diabp_t4*btw    #
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit6 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_diabp_t4
#             + abs_d_diabp_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + abs_d_diabp_t4*betweenT4visits
#             + mean_diabp_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_diabp_t4
#             + abs_d_diabp_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#              + abs_d_diabp_t4*rcs(betweenT4visits, knots=c(10, 20, 30))
#             + mean_diabp_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit8 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_diabp_t4
#             + abs_d_diabp_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + abs_d_diabp_t4*diabp_term
#             + mean_diabp_t4
#             #+ education_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)

# extractAIC(fit1)
# extractAIC(fit2)
extractAIC(fit3)
# extractAIC(fit4)
# extractAIC(fit5)
# extractAIC(fit6)
# extractAIC(fit7)
# extractAIC(fit8)
anova(fit3)
fit3check <- coxph(Surv(tstart, tstop, death) ~
               abs_d_diabp_t4
             + abs_d_diabp_t4:ns(c.age, 4)
            + ns(c.age, 4)
            + mean_diabp_t4
            + abs_d_diabp_t4*dic_betweenvisits_20
            + strata(sex_t4)
            ,data = tdata)
AIC(fit3check)
check <- cox.zph(fit3check); print(check); plot(check, resid=F)


sd_diabp <- sd(tu2$d_diabp_t4, na.rm = T)
mean_diabp <- mean(tu2$mean_diabp_t4, na.rm = T)
mean_diabp
cont_fit3 <- contrast(fit3, 
                     a = list(c.age=55:85, abs_d_diabp_t4=sd_diabp, sex_t4=0,  dic_betweenvisits_20=1),
                    b =  list(c.age=55:85, abs_d_diabp_t4=0, sex_t4=0, dic_betweenvisits_20=1))                    
                     
print(cont_fit3, fun=exp)
cont_fit3 <- as.data.frame(cont_fit3[c('Contrast','Lower','Upper')])

cont_fit32 <- contrast(fit3, 
                      a = list(c.age=55:85, abs_d_diabp_t4=sd_diabp, sex_t4=0,  dic_betweenvisits_20=0),
                      b =  list(c.age=55:85, abs_d_diabp_t4=0, sex_t4=0,  dic_betweenvisits_20=0))                    

print(cont_fit32, fun=exp)
cont_fit32 <- as.data.frame(cont_fit32[c('Contrast','Lower','Upper')])
colnames(cont_fit32) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_diabp <- cbind(cont_fit3, cont_fit32)

HR_diabp <- ggplot(cont_fit_diabp, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
 geom_line(aes(y=exp(Contrast2)), col = "blue") +
 geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.2,1.4)) +
  xlab('Age')  + ggtitle("Diastolic blood pressure") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
HR_diabp

