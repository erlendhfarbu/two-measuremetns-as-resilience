tu <- readRDS("~/postdocHC/tu.rds")

tu2 <- tu %>% dplyr::select (c(Mean_hdl_t4, death, follow_up_time_t42, d_hdl_t4, age_t4, abs_d_hdl_t4, hdl_t4, d_hdl_t4, 
                        sex_t4, betweenT4visits, dic_betweenvisits_20, dic_betweenvisits, dic_betweenvisits_60))
#tu2 <- subset(tu2, betweenT4visits<121)
#tu2 <- subset(tu2, d_hdl_t4>=0)
#tu2 <- subset(tu2, d_hdl_t4<=0)
#tu2 <- subset(tu2, dic_betweenvisits_60==1)


dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
# head(tdata, n=50L)
dd <- datadist(tdata)


options(datadist = "dd")
# fit1 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + hdl_t4
#             + abs_d_hdl_t4    #
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit2 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_hdl_t4, 3)
#             + rcs(abs_d_hdl_t4, 3)*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + hdl_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit3 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + hdl_t4
#             + abs_d_hdl_t4*dic_betweenvisits_20    
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + hdl_t4
#             + abs_d_hdl_t4*dic_betweenvisits    #
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + hdl_t4
#             + abs_d_hdl_t4*dic_betweenvisits_60    #
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit6 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + abs_d_hdl_t4*betweenT4visits
#             + hdl_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_hdl_t4
#             + abs_d_hdl_t4*rcs(c.age, 4)
#             + rcs(c.age, 4)
#             + abs_d_hdl_t4*rcs(betweenT4visits, knots=c(20,100))
#             + hdl_t4
#             + strat(sex_t4)#*ns(c.age, 4)
#             ,data = tdata)
# 
# extractAIC(fit1)
# extractAIC(fit2)
# extractAIC(fit3)
# extractAIC(fit4) #fit4 is best
# extractAIC(fit5)
# extractAIC(fit6)
# extractAIC(fit7)
# anova(fit5) # no interaction with age. p>0.48. Removing it. 
fit5 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_hdl_t4
            + abs_d_hdl_t4#*rcs(c.age, 4)#*dic_betweenvisits_60   
            + rcs(c.age, 4)
            + hdl_t4
            + abs_d_hdl_t4*dic_betweenvisits_60    
            + strat(sex_t4)
            ,data = tdata)
AIC(fit5) #55953.46 with age*abs vs 55950.05 without. Removing interaction is probably best, p=0.34.
# fit5check <- coxph(Surv(tstart, tstop, death) ~
#               abs_d_hdl_t4
#             + ns(c.age, 4)
#             + hdl_t4
#             + abs_d_hdl_t4*dic_betweenvisits_60
#             + strata(sex_t4)
#             ,data = tdata)
# check <- cox.zph(fit5check, transform = "identity"); print(check); plot(check, resid=F)
#PH assumption ok
sd_hdl <- sd(tu2$d_hdl_t4, na.rm = T)
mean_hdl <- mean(tu2$hdl_t4, na.rm=T)

cont_fit5 <- contrast(fit5, 
                      a = list(c.age=55:85, abs_d_hdl_t4=sd_hdl, sex_t4=0,  dic_betweenvisits_60=1),
                      b =  list(c.age=55:85, abs_d_hdl_t4=0, sex_t4=0, dic_betweenvisits_60=1))                    

print(cont_fit5, fun=exp)
cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])

cont_fit52 <- contrast(fit5, 
                       a = list(c.age=55:85, abs_d_hdl_t4=sd_hdl, sex_t4=0, dic_betweenvisits_60=0),
                       b =  list(c.age=55:85, abs_d_hdl_t4=0, sex_t4=0, dic_betweenvisits_60=0))                    

print(cont_fit52, fun=exp)
cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_hdl <- cbind(cont_fit5, cont_fit52)

HR_hdl <- ggplot(cont_fit_hdl, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=5,
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Age') + ylab('') + ggtitle("High-density lipoprotein") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
HR_hdl

# 
# ### HDL*betweenT4visits ----
# sd_hdl <- sd(tu2$d_hdl_t4, na.rm = T)
# mean_hdl <- mean(tu2$Mean_hdl_t4, na.rm=T)
# 
# cont_fit5 <- contrast(fit6, 
#                       a = list(c.age=55:85, abs_d_hdl_t4=2*sd_hdl, sex_t4=0, hdl_t4=mean_hdl, betweenT4visits=90),
#                       b =  list(c.age=55:85, abs_d_hdl_t4=0, sex_t4=0, hdl_t4=mean_hdl, betweenT4visits=90))                    
# 
# print(cont_fit5, fun=exp)
# cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])
# 
# cont_fit52 <- contrast(fit6, 
#                        a = list(c.age=55:85, abs_d_hdl_t4=2*sd_hdl, sex_t4=0, hdl_t4=mean_hdl, betweenT4visits=0),
#                        b =  list(c.age=55:85, abs_d_hdl_t4=0, sex_t4=0, hdl_t4=mean_hdl, betweenT4visits=0))                    
# 
# print(cont_fit52, fun=exp)
# cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
# colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
# cont_fit_hdl <- cbind(cont_fit5, cont_fit52)
# 
# HR_hdl <- ggplot(cont_fit_hdl, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
#   geom_line(aes(y=exp(Contrast2)), col = "blue") +
#   geom_line(aes(y=1), col = "black") +
#   geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
#               alpha=0.1, linetype=0, fill = "darkred") + 
#   geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
#               alpha=0.1, linetype=0, fill = "blue") +
#   scale_y_continuous(trans='log10', n.breaks=5,
#                      minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
#   xlab('Age') + ylab('') + ggtitle("High-density lipoprotein") +
#   theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
# HR_hdl

