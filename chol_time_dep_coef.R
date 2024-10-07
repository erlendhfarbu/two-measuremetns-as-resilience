tu <- readRDS("~/postdocHC/tu.rds")

tu2 <- tu %>% dplyr::select (c(Mean_chol_t4, cholesterol_t4, d_chol_t4, death, follow_up_time_t42, age_t4, abs_d_chol_t4, 
                        sex_t4, betweenT4visits, dic_betweenvisits_60, dic_betweenvisits_20,
                        dic_betweenvisits ))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)


options(datadist = "dd")
# fit1 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + abs_d_chol_t4    #
#             + strat(sex_t4)
#             ,data = tdata)
# fit2 <- cph(Surv(tstart, tstop, death) ~  
#               rcs(abs_d_chol_t4, 3)
#             + rcs(abs_d_chol_t4, 3)*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + strat(sex_t4)
#             ,data = tdata)
# fit3 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + abs_d_chol_t4*dic_betweenvisits_20    
#             + strat(sex_t4)
#             ,data = tdata)
# fit4 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + abs_d_chol_t4*dic_betweenvisits    #
#             + strat(sex_t4)
#             ,data = tdata)
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + abs_d_chol_t4*dic_betweenvisits_60    #
#             + strat(sex_t4)
#             ,data = tdata)
# fit6 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + abs_d_chol_t4*betweenT4visits
#             + cholesterol_t4
#             + strat(sex_t4)
#             ,data = tdata)
# fit7 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             + abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + abs_d_chol_t4*rcs(betweenT4visits, 3)
#             + cholesterol_t4
#             + strat(sex_t4)
#             ,data = tdata)
# 
# extractAIC(fit1)
# extractAIC(fit2)
# extractAIC(fit3)
# extractAIC(fit4)
# extractAIC(fit5)
# extractAIC(fit6)
# extractAIC(fit7)
# 
# anova(fit5)
# fit5 <- cph(Surv(tstart, tstop, death) ~  
#               abs_d_chol_t4
#             #+ abs_d_chol_t4*rcs(c.age, 3)
#             + rcs(c.age, 3)
#             + cholesterol_t4
#             + abs_d_chol_t4*dic_betweenvisits_60    
#             + strat(sex_t4)
#             ,data = tdata)
# AIC(fit5) # 56142.9 without age*abs vs 56145.23 -> diff <3. Interaction term for age not significant, but keeping the interaction term. 
tdata$year <- ceiling(tdata$tstop)
tdata$int_chol_time <- tdata$cholesterol_t4*log(tdata$year)
dd <- datadist(tdata)
options(datadist = "dd")
fit5 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_chol_t4
            + abs_d_chol_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + cholesterol_t4
            + abs_d_chol_t4*dic_betweenvisits_60    
            + strat(sex_t4)
            ,data = tdata)
AIC(fit5)
anova(fit5)

fit5check <- coxph(Surv(tstart, tstop, death) ~
              abs_d_chol_t4#
            + abs_d_chol_t4:ns(c.age, 3)
            + ns(c.age, 3)
            + cholesterol_t4
            + abs_d_chol_t4*dic_betweenvisits_60
            + strata(sex_t4)
            ,data = tdata)
anova(fit5check)
AIC(fit5check)
check <- cox.zph(fit5check, transform = "identity"); print(check); plot(check, resid=F)

pred <- Predict(fit5, abs_d_chol_t4=seq(0,1, by=0.01),  ref.zero=T, fun = exp)
plot <- ggplot(pred)
plot + scale_y_continuous(trans='log2') + xlab("Absolute difference in cholesterol") + ylab("Hazard ratio") + coord_cartesian(ylim = c(0.5,1.5))

sd_chol <- sd(tu2$d_chol_t4, na.rm = T)
mean_chol <- mean(tu2$cholesterol_t4, na.rm = T)

cont_fit5 <- contrast(fit5, 
                      a = list(c.age=55:85, abs_d_chol_t4=sd_chol, sex_t4=0, dic_betweenvisits_60=1),
                      b =  list(c.age=55:85, abs_d_chol_t4=0, sex_t4=0, dic_betweenvisits_60=1))                    

print(cont_fit5, fun=exp)
cont_fit5 <- as.data.frame(cont_fit5[c('Contrast','Lower','Upper')])

cont_fit52 <- contrast(fit5, 
                       a = list(c.age=55:85, abs_d_chol_t4=sd_chol, sex_t4=0, dic_betweenvisits_60=0),
                       b =  list(c.age=55:85, abs_d_chol_t4=0, sex_t4=0,  dic_betweenvisits_60=0))                    

print(cont_fit52, fun=exp)
cont_fit52 <- as.data.frame(cont_fit52[c('Contrast','Lower','Upper')])
colnames(cont_fit52) <- c("Contrast2", "Lower2", "Upper2")
cont_fit_chol <- cbind(cont_fit5, cont_fit52)

HR_chol <- ggplot(cont_fit_chol, aes(x=55:85)) + geom_line(aes(y=exp(Contrast)), col = "darkred") +
  geom_line(aes(y=exp(Contrast2)), col = "blue") +
  geom_line(aes(y=1), col = "black") +
  geom_ribbon(aes(ymin=exp(Lower), ymax=exp(Upper)),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_ribbon(aes(ymin=exp(Lower2), ymax=exp(Upper2)),
              alpha=0.1, linetype=0, fill = "blue") +
  scale_y_continuous(trans='log10', n.breaks=6, 
                     minor_breaks=c(seq(0.5, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Age')  + ylab('') + ggtitle("Cholesterol") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 8))  
HR_chol

