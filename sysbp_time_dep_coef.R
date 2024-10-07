
tu <- readRDS("~/postdocHC/tu.rds")
tu2 <- tu %>% dplyr::select (c(Mean_sysbp_t4, mean_sysbp_t4, d_sysbp_t4, death, 
                               follow_up_time_t42, age_t4, abs_d_sysbp_t4, sex_t4, 
                               betweenT4visits, dic_betweenvisits_20, dic_betweenvisits_60, 
                               dic_betweenvisits))
#tu2 <- subset(tu2, betweenT4visits<121)
#tu2 <- subset(tu2, d_sysbp_t4>=0)
#tu2 <- subset(tu2, d_sysbp_t4<=0)

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
# head(tdata, n=50L)
dd <- datadist(tdata)
# dd$limits$c.age[2] <-70
# #dd$limits$sex_t4[2] <- 0
# dd$limits$betweenT4visits[2] <- 41
# dd$limits$abs_d_sysbp_t4[2] <- 0
# dd$limits$dic_betweenvisits_20[2] <- 1

options(datadist = "dd")
fit <- cph(Surv(tstart, tstop, death) ~  
             + abs_d_sysbp_t4*dic_betweenvisits_20 
             + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)#*ns(c.age, 3)
            ,data = tdata)
fit1 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_sysbp_t4
            + abs_d_sysbp_t4*rcs(c.age, 3) #No improvement of model with interaction with age. PH-assumption ok in fit 
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + abs_d_sysbp_t4    #
            + strat(sex_t4)#*ns(c.age, 3)
            ,data = tdata)
fit2 <- cph(Surv(tstart, tstop, death) ~  
              rcs(abs_d_sysbp_t4, 3)*dic_betweenvisits_20 
            + rcs(abs_d_sysbp_t4, 3)#*rcs(c.age, 3)
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)
            ,data = tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_sysbp_t4*dic_betweenvisits_20
            + abs_d_sysbp_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)
            ,data = tdata)
AIC(fit3) # 3 knots have same AIC as 4
anova(fit3)
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_sysbp_t4, 3)*dic_betweenvisits
#             + rcs(abs_d_sysbp_t4, 3)#*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + mean_sysbp_t4
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)
# 
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_sysbp_t4, 3)*dic_betweenvisits_60
#             + rcs(abs_d_sysbp_t4, 3)#*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + mean_sysbp_t4
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)
# 
# fit6 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_sysbp_t4, 3)*betweenT4visits
#             + rcs(abs_d_sysbp_t4, 3)#*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + mean_sysbp_t4
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#             + rcs(abs_d_sysbp_t4, 3)#*rcs(c.age, 3)
#            + rcs(abs_d_sysbp_t4, 3)*rcs(betweenT4visits, knots=c(20,60,100)) 
#            + rcs(c.age, 3)
#             + mean_sysbp_t4
#             + strat(sex_t4)#*ns(c.age, 3)
#             ,data = tdata)
        
extractAIC(fit)
extractAIC(fit1)
extractAIC(fit2)
extractAIC(fit3)
# extractAIC(fit4)
# extractAIC(fit5)
# extractAIC(fit6)
# extractAIC(fit7)

anova(fit3)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_sysbp_t4*dic_betweenvisits_20
           # + abs_d_sysbp_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)
            ,data = tdata)
AIC(fit3) # Slightly higher AIC without int.term. 56258.09 vs 56256.94 with, p<0.075 for interaction. 
              #Keeping interaction between age and difference
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_sysbp_t4*dic_betweenvisits_20
             + abs_d_sysbp_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)
            ,data = tdata)
fit3check <- coxph(Surv(tstart, tstop, death) ~  
                abs_d_sysbp_t4*dic_betweenvisits_20
              + abs_d_sysbp_t4*ns(c.age, 3)
              + ns(c.age, 3)
              + mean_sysbp_t4
              + strata(sex_t4)
              ,data = tdata)
AIC(fit3check)
check <- cox.zph(fit3check, transform = "identity"); print(check); plot(check, resid=T)
# PH ok
sd_sysbp <- sd(tu$abs_d_sysbp_t4, na.rm = T)
mean_sysbp <- mean(tu2$mean_sysbp_t4, na.rm = T)
mean_sysbp
cont_fit3 <- contrast(fit3, 
                      a = list(c.age=55:85, abs_d_sysbp_t4=sd_sysbp, sex_t4=0, mean_sysbp_t4=mean_sysbp, dic_betweenvisits_20=1),
                      b =  list(c.age=55:85, abs_d_sysbp_t4=0, sex_t4=0, mean_sysbp_t4=mean_sysbp, dic_betweenvisits_20=1))                    

print(cont_fit3, fun=exp)
cont_fit3 <- as.data.frame(cont_fit3[c('Contrast','Lower','Upper')])

cont_fit32 <- contrast(fit3, 
                       a = list(c.age=55:85, abs_d_sysbp_t4=sd_sysbp, sex_t4=0, mean_sysbp_t4=mean_sysbp, dic_betweenvisits_20=0),
                       b =  list(c.age=55:85, abs_d_sysbp_t4=0, sex_t4=0, mean_sysbp_t4=mean_sysbp, dic_betweenvisits_20=0))                    

print(cont_fit32, fun=exp)
cont_fit32 <- as.data.frame(cont_fit32[c('Contrast','Lower','Upper')])
colnames(cont_fit32) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_sysbp <- cbind(cont_fit3, cont_fit32)

HR_sysbp <- ggplot(cont_fit_sysbp, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Age')  + ggtitle("Systolic blood pressure") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
HR_sysbp
#+ ylab('HR for a 8.6 unit absolute difference')