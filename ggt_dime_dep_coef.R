tu <- readRDS("~/postdocHC/tu.rds")

tu2 <- tu %>% dplyr::select (c(Mean_ggt_t4, d_ggt_t4, ggt_t42, d_ggt_t4, ggt_t4, death, follow_up_time_t42, age_t4, abs_d_ggt_t4, sex_t4, betweenT4visits, 
                               dic_betweenvisits_20, dic_betweenvisits, dic_betweenvisits_60, alco_quart))
#tu2 <- subset(tu2, ggt_t4<200 & ggt_t42<200) # some really high values that could be an indicator of disease
#tu2 <- subset(tu2, betweenT4visits<121)
#tu2 <- subset(tu2, d_ggt_t4>=0)
#tu2 <- subset(tu2, d_ggt_t4<=0)

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
# head(tdata, n=100L)
dd <- datadist(tdata)
# dd$limits$c.age[2] <- 70
# dd$limits$sex_t4[2] <- 1
# dd$limits$betweenT4visits[2] <- 100
# dd$limits$dic_betweenvisits_60[2] <- 0
# dd$limits$abs_d_ggt_t4[2] <- 0

options(datadist = "dd")
# fit <- cph(Surv(tstart, tstop, death) ~  
#              + abs_d_ggt_t4 
#            + rcs(c.age, 3)
#            + ggt_t4
#            + strat(sex_t4)
#            ,data = tdata)
# fit1 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_ggt_t4
#             + abs_d_ggt_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + ggt_t4
#             + abs_d_ggt_t4    #
#             + strat(sex_t4)
#             ,data = tdata)
# fit2 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_ggt_t4, 3) #better fit with rcs
#             + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)
# fit3 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_ggt_t4, 3)*dic_betweenvisits_20
#             + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)
# 
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_ggt_t4, 3)*dic_betweenvisits
#             + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)

fit5 <- cph(Surv(tstart, tstop, death) ~  
              rcs(abs_d_ggt_t4, 3)*dic_betweenvisits_60#*rcs(c.age, 3)
            + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 4) 
            + rcs(c.age, 4)
            + ggt_t4
           # + alco_quart
            + strat(sex_t4)
            ,data = tdata)

# fit5a <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_ggt_t4*dic_betweenvisits_60
#             + abs_d_ggt_t4*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)
# fit6 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_ggt_t4, 3)*betweenT4visits
#             + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_ggt_t4, 3)*rcs(betweenT4visits, knots=c(20,60,100)) 
#             + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3) 
#             + rcs(c.age, 3)
#             + ggt_t4
#             + strat(sex_t4)
#             ,data = tdata)
# 
# extractAIC(fit)
# extractAIC(fit1)
# extractAIC(fit2)
# extractAIC(fit3)
# extractAIC(fit4)
 extractAIC(fit5)
# extractAIC(fit5a)
# extractAIC(fit6)
# extractAIC(fit7)

anova(fit5) #p for interaction age*difference <0.01

fit5check <- coxph(Surv(tstart, tstop, death) ~
              ns(abs_d_ggt_t4, 3)*dic_betweenvisits_60
            + ns(abs_d_ggt_t4, 3)*ns(c.age, 5)
            + ns(c.age, 5)
            + ggt_t4
            + strata(sex_t4)
            ,data = tdata)
AIC(fit5check)
check <- cox.zph(fit5check); print(check); plot(check, resid=F)
##increasing number of knots to meet PH assumption

fit5 <- cph(Surv(tstart, tstop, death) ~  
              rcs(abs_d_ggt_t4, 3)*dic_betweenvisits_60#*rcs(c.age, 3)
            + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 5) 
            + rcs(c.age, 5)
            + ggt_t4
            + strat(sex_t4)
            ,data = tdata)

sd <- sd(tu2$d_ggt_t4, na.rm = T)
mean_ggt <- mean(tu2$ggt_t4, na.rm = T)
cont_fit5 <- contrast(fit5, 
                      a = list(c.age=55:85, abs_d_ggt_t4=sd, sex_t4=0,  dic_betweenvisits_60=1),
                      b =  list(c.age=55:85, abs_d_ggt_t4=0, sex_t4=0, dic_betweenvisits_60=1))                    

print(cont_fit5, fun=exp)
cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])

cont_fit52 <- contrast(fit5, 
                       a = list(c.age=55:85, abs_d_ggt_t4=sd, sex_t4=0,  dic_betweenvisits_60=0),
                       b =  list(c.age=55:85, abs_d_ggt_t4=0, sex_t4=0,  dic_betweenvisits_60=0))                    

print(cont_fit52, fun=exp)
cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_ggt <- cbind(cont_fit5, cont_fit52)

HR_ggt <- ggplot(cont_fit_ggt, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.10, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Age')  + ggtitle("Gamma Glutamyltransferase") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5))+ theme(text = element_text(size = 8))  


HR_ggt


cont_fit5 <- contrast(fit5, 
a = list(abs_d_ggt_t4=0:30, c.age=80, sex_t4=0,  dic_betweenvisits_60=1),
b =  list(abs_d_ggt_t4=0, c.age=80, sex_t4=0,  dic_betweenvisits_60=1))                    

print(cont_fit5, fun=exp)
cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])

cont_fit52 <- contrast(fit5, 
                       a = list(abs_d_ggt_t4=0:30, c.age=80, sex_t4=0, dic_betweenvisits_60=0),
                       b =  list(abs_d_ggt_t4=0, c.age=80, sex_t4=0,  dic_betweenvisits_60=0))                    

print(cont_fit52, fun=exp)
cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_ggt <- cbind(cont_fit5, cont_fit52)

HR_ggt2 <- ggplot(cont_fit_ggt, aes(x=0:30)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.10, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Absolute difference in GGT')  + ggtitle("Gamma Glutamyltransferase") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5))+ theme(text = element_text(size = 8))  


HR_ggt2

