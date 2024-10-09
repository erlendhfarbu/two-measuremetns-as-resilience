tu <- readRDS("~/postdocHC/tu.rds")
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
## total participant over time ----
hist(tu$betweenT4visits, breaks = 120, main="",
     xlab = "Time between measurements in days", ylab="Number of participants")

num_total <- c()
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits==val)
  num_total[val] <- sum(d$betweenT4visits==val,  na.rm = T)
}

ma_num_total_7 <- c()
for (val in 4:116)
{
  ma_num_total_7[val] <- sum(num_total[(val-3):(val+3)])          
}


num_ma7 <- plot(ma_num_total_7, type = "l")

ma_num_total_11 <- c()
for (val in 6:125)
{
  ma_num_total_11[val] <- sum(num_total[(val-5):(val+5)])          
}

num_ma11 <-plot(ma_num_total_11, type = "l")

ma_num_total_15 <- c()
for (val in 8:112)
{
  ma_num_total_15[val] <- sum(num_total[(val-7):(val+7)])          
}

num_ma15 <- plot(ma_num_total_15, type = "l")

#numb_ma_7_11_15 <- grid.arrange(num_ma7, num_ma11, num_ma15) #does not work with R-originalplots? only ggplot?

      #### Going for 7 ####

        #### sysbp ####
daily_median_d_sysbp <- c()

for (val in 1:120)
{
  daily_median_d_sysbp[val] = median(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_sysbp <- c()
daily_mean_d_sysbp <- c()
ma_se_sysbp <- c()
daily_se_sysbp <- c()
ma_d_median_sysbp <- c()
perc_change_sysbp=c()
ma_perc_change_sysbp <- c()
for (val in 1:120)
{
  daily_mean_d_sysbp[val] = mean(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_sysbp[val] <- sd(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val)))
  perc_change_sysbp[val] <- mean(subset(data$perc_d_sysbp_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_sysbp[val] = mean(daily_mean_d_sysbp[(val-3):(val+3)], na.rm = T)
  ma_se_sysbp[val] <- mean(daily_se_sysbp[(val-3):(val+3)], na.rm = T)
  ma_d_median_sysbp[val] <- mean(daily_median_d_sysbp[(val-3):(val+3)], na.rm = T)
  ma_perc_change_sysbp[val] <- mean(perc_change_sysbp[(val-3):(val+3)], na.rm = T )
}

sysbp_daily_prop_1sd_d <- c()
num_1sd_d <- c()
sum <- summary(data$abs_d_sysbp_t4) 
sd <- sd(data$d_sysbp_t4, na.rm=T) 
#sd <- sum[5] 
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_sysbp_t4>=sd,  na.rm = T)
  sysbp_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_sysbp_t4))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_sysbp, type="l", ylim = c(3, 20), xlim = c(0,120), main = "systolic blood pressure", 
     ylab="Absolute difference in mmHg", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_sysbp + ma_se_sysbp*1.96, type="l", lty=2)
lines(ma_d_sysbp - ma_se_sysbp*1.96, type="l", lty=2)
lines(ma_d_median_sysbp, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 18, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(sysbp_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_sysbp2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_num_total_11, type="h",ylim = c(0, 1800) , xlim = c(0,120), main = "Number of participants in 7 days Window", 
     ylab="Number of participants ", xlab="Time between measurements in The Tromsø Study 4")



cox_sysbp <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                     mean_sysbp_t4
                  # + age_t4 
                   + strata(sex_t4)
                   + abs_d_sysbp_t4
                   ,data = tu)
summary(cox_sysbp)
check <- cox.zph(cox_sysbp, transform = "identity"); print(check); plot(check, resid=F)

ma_HR_sysbp_plain <- c()

for (val in 1:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  
  cox_sysbp <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                       mean_sysbp_t4
                     # + age_t4 
                     + strata(sex_t4)
                     + abs_d_sysbp_t4
                     ,data = d)
  ma_HR_sysbp_plain[val] <- cox_sysbp$coefficients[2]
}
plot(ma_HR_sysbp_plain, type="l")

ma_HR_sysbp <- c()
Lower <- c()
Upper <- c()
sd_sysbp <- sd(tu$abs_d_sysbp_t4, na.rm = T)
mean_sysbp <- mean(tu$mean_sysbp_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_sysbp_t4, mean_sysbp_t4, death, 
                                follow_up_time_t42, age_t4, abs_d_sysbp_t4, sex_t4, 
  ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_sysbp_t4
              + abs_d_sysbp_t4*rcs(c.age, 3)
              + rcs(c.age, 3)
              + mean_sysbp_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_sysbp_t4=sd_sysbp, sex_t4=0, mean_sysbp_t4=mean_sysbp),
                        b =  list(c.age=70, abs_d_sysbp_t4=0, sex_t4=0, mean_sysbp_t4=mean_sysbp))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_sysbp[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_sysbp <- rbind(HR = exp(ma_HR_sysbp), 
                     Lower = exp(Lower),
                     Upper = exp(Upper))
ma_HR_sysbp <- as.data.frame(ma_HR_sysbp)
ma_HR_sysbp <- t(ma_HR_sysbp)
ggplot(ma_HR_sysbp, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.8,1,1.2, 1.4, 1.6), 
                     ) +
  xlab('Days between measurements')  + ggtitle("Systolic blood pressure \n Calculated from a window of 7 days") + ylab("Hazard ratio")+
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_sysbp_t4, mean_sysbp_t4, death, 
                              follow_up_time_t42, age_t4, abs_d_sysbp_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_sysbp_t4
            + abs_d_sysbp_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + mean_sysbp_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_sysbp_t4=sd_sysbp, sex_t4=0, mean_sysbp_t4=mean_sysbp),
                      b =  list(c.age=70, abs_d_sysbp_t4=0, sex_t4=0, mean_sysbp_t4=mean_sysbp))                
print(cont_fit3, fun=exp)



####  Diabp   ####
daily_median_d_diabp <- c()

for (val in 1:120)
{
  daily_median_d_diabp[val] = median(subset(data$abs_d_diabp_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_diabp <- c()
daily_mean_d_diabp <- c()
ma_se_diabp <- c()
daily_se_diabp <- c()
ma_d_median_diabp <- c()
perc_change_diabp=c()
ma_perc_change_diabp <- c()
for (val in 1:120)
{
  daily_mean_d_diabp[val] = mean(subset(data$abs_d_diabp_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_diabp[val] <- sd(subset(data$abs_d_diabp_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_diabp_t4, data$betweenT4visits==val)))
  perc_change_diabp[val] <- mean(subset(data$perc_d_diabp_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_diabp[val] = mean(daily_mean_d_diabp[(val-3):(val+3)], na.rm = T)
  ma_se_diabp[val] <- mean(daily_se_diabp[(val-3):(val+3)], na.rm = T)
  ma_d_median_diabp[val] <- mean(daily_median_d_diabp[(val-3):(val+3)], na.rm = T)
  ma_perc_change_diabp[val] <- mean(perc_change_diabp[(val-3):(val+3)], na.rm = T )
}
diabp_daily_prop_1sd_d <- c()
sum <- summary(data$abs_d_diabp_t4) 
sd <- sd(data$d_diabp_t4, na.rm=T) 
#sd <- sum[5] 
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_diabp_t4>=sd,  na.rm = T)
  diabp_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_diabp_t4))
  
}
par(mar=c(5,5,5,5))
plot(ma_d_diabp, type="l", ylim = c(0, 14), xlim = c(0,120), main = "Diastolic blood pressure", 
     ylab="Absolute difference in mmHg", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_diabp + ma_se_diabp*1.96, type="l", lty=2)
lines(ma_d_diabp - ma_se_diabp*1.96, type="l", lty=2)
lines(ma_d_median_diabp, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 13, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(diabp_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_diabp2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()


plot(ma_perc_change_diabp, type="l", ylim = c(0.05, 0.1), xlim = c(0,120), main = "Percentage change diastolic blood pressure", 
     ylab="Percentage change", xlab="Time between measurements in The Tromsø Study 4")




cox_diabp <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                     mean_diabp_t4
                   # + age_t4 
                   + strata(sex_t4)
                   + abs_d_diabp_t4
                   ,data = tu)
summary(cox_diabp)
check <- cox.zph(cox_diabp, transform = "identity"); print(check); plot(check, resid=F)

ma_HR_diabp <- c()
Lower <- c()
Upper <- c()
sd_diabp <- sd(tu$abs_d_diabp_t4, na.rm = T)
mean_diabp <- mean(tu$mean_diabp_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_diabp_t4, mean_diabp_t4, death, 
                                 follow_up_time_t42, age_t4, abs_d_diabp_t4, sex_t4, 
                                ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_diabp_t4
              + abs_d_diabp_t4*rcs(c.age, 4)
              + rcs(c.age, 4)
              + mean_diabp_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_diabp_t4=sd_diabp, sex_t4=0, mean_diabp_t4=mean_diabp),
                        b =  list(c.age=70, abs_d_diabp_t4=0, sex_t4=0, mean_diabp_t4=mean_diabp))                    
  
  #print(cont_fit3, fun=exp)
        

  ma_HR_diabp[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
  }
plot(ma_HR_diabp, type="l")
lines(Lower)
lines(Upper)
ma_HR_diabp <- rbind(HR = exp(ma_HR_diabp), 
                              Lower = exp(Lower),
                              Upper = exp(Upper))
ma_HR_diabp <- as.data.frame(ma_HR_diabp)
ma_HR_diabp <- t(ma_HR_diabp)
ggplot(ma_HR_diabp, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.6,0.8,1,1.2, 1.4, 1.6, 1.8), 
                     ) +
  xlab('Days between measurements')  + ggtitle("Distolic blood pressure \n Calculated from a window of 7 days") + ylab('') + ylab("Hazard ratio")+
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_diabp_t4, mean_diabp_t4, death, 
                              follow_up_time_t42, age_t4, abs_d_diabp_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_diabp_t4
            + abs_d_diabp_t4*rcs(c.age, 4)
            + rcs(c.age, 4)
            + mean_diabp_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_diabp_t4=sd_diabp, sex_t4=0, mean_diabp_t4=mean_diabp),
                      b =  list(c.age=70, abs_d_diabp_t4=0, sex_t4=0, mean_diabp_t4=mean_diabp))                    

print(cont_fit3, fun=exp)


####  hr   ####
daily_median_d_hr <- c()

for (val in 1:120)
{
  daily_median_d_hr[val] = median(subset(data$abs_d_hr_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_hr <- c()
daily_mean_d_hr <- c()
ma_se_hr <- c()
daily_se_hr <- c()
ma_d_median_hr <- c()
perc_change_hr=c()
ma_perc_change_hr <- c()
for (val in 1:120)
{
  daily_mean_d_hr[val] = mean(subset(data$abs_d_hr_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_hr[val] <- sd(subset(data$abs_d_hr_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_hr_t4, data$betweenT4visits==val)))
  perc_change_hr[val] <- mean(subset(data$perc_d_hr_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_hr[val] = mean(daily_mean_d_hr[(val-3):(val+3)], na.rm = T)
  ma_se_hr[val] <- mean(daily_se_hr[(val-3):(val+3)], na.rm = T)
  ma_d_median_hr[val] <- mean(daily_median_d_hr[(val-3):(val+3)], na.rm = T)
  ma_perc_change_hr[val] <- mean(perc_change_hr[(val-3):(val+3)], na.rm = T )
}

hr_daily_prop_1sd_d <- c()
sum <- summary(data$abs_d_hr_t4) 
sd <- sd(data$d_hr_t4, na.rm=T) 
#sd <- sum[5] 
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_hr_t4>=sd,  na.rm = T)
  hr_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_hr_t4))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_hr, type="l", ylim = c(0, 17), xlim = c(0,120), main = "Heart rate", 
     ylab="Absolute difference", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_hr + ma_se_hr*1.96, type="l", lty=2)
lines(ma_d_hr - ma_se_hr*1.96, type="l", lty=2)
lines(ma_d_median_hr, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 14, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(hr_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_hr2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_perc_change_hr, type="l", ylim = c(0.07, 0.12), xlim = c(0,120), main = "Percentage change hear rate", 
     ylab="Percentage change", xlab="Time between measurements in The Tromsø Study 4")

ma_HR_hr <- c()
Lower <- c()
Upper <- c()
sd_hr <- sd(tu$abs_d_hr_t4, na.rm = T)
mean_hr <- mean(tu$hr_t41, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_hr_t4, hr_t41, death, 
                                follow_up_time_t42, age_t4, abs_d_hr_t4, sex_t4, 
  ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_hr_t4
              + abs_d_hr_t4
              + rcs(c.age, 3)
              + hr_t41
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_hr_t4=sd_hr, sex_t4=0, hr_t41=mean_hr),
                        b =  list(c.age=70, abs_d_hr_t4=0, sex_t4=0, hr_t41=mean_hr))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_hr[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_hr <- rbind(HR = exp(ma_HR_hr), 
                     Lower = exp(Lower),
                     Upper = exp(Upper))
ma_HR_hr <- as.data.frame(ma_HR_hr)
ma_HR_hr <- t(ma_HR_hr)
ggplot(ma_HR_hr, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5))) +
  xlab('Days between measurements')  + ggtitle("Heart rate \n Calculated from a window of 7 days") + ylab('Hazard ratio') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_hr_t4, hr_t41, death, 
                              follow_up_time_t42, age_t4, abs_d_hr_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_hr_t4
            + abs_d_hr_t4
            + rcs(c.age, 3)
            + hr_t41
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_hr_t4=sd_hr, sex_t4=0, hr_t41=mean_hr),
                      b =  list(c.age=70, abs_d_hr_t4=0, sex_t4=0, hr_t41=mean_hr))                    

print(cont_fit3, fun=exp)


####  chol   ####
daily_median_d_chol <- c()

for (val in 1:120)
{
  daily_median_d_chol[val] = median(subset(data$abs_d_chol_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_chol <- c()
daily_mean_d_chol <- c()
ma_se_chol <- c()
daily_se_chol <- c()
ma_d_median_chol <- c()
perc_change_chol=c()
ma_perc_change_chol <- c()
for (val in 1:120)
{
  daily_mean_d_chol[val] = mean(subset(data$abs_d_chol_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_chol[val] <- sd(subset(data$abs_d_chol_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_chol_t4, data$betweenT4visits==val)))
  perc_change_chol[val] <- mean(subset(data$perc_d_chol_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_chol[val] = mean(daily_mean_d_chol[(val-3):(val+3)], na.rm = T)
  ma_se_chol[val] <- mean(daily_se_chol[(val-3):(val+3)], na.rm = T)
  ma_d_median_chol[val] <- mean(daily_median_d_chol[(val-3):(val+3)], na.rm = T)
  ma_perc_change_chol[val] <- mean(perc_change_chol[(val-3):(val+3)], na.rm = T )
}
chol_daily_prop_1sd_d <- c()
sum <- summary(data$abs_d_chol_t4) 
sd <- sd(data$d_chol_t4, na.rm=T) 
#sd <- sum[5] 
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_chol_t4>=sd,  na.rm = T)
  chol_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_chol_t4))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_chol, type="l", ylim = c(0, 1.3), xlim = c(0,120), main = "cholesterol", 
     ylab="Absolute difference in mmol/l", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_chol + ma_se_chol*1.96, type="l", lty=2)
lines(ma_d_chol - ma_se_chol*1.96, type="l", lty=2)
lines(ma_d_median_chol, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 14, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(chol_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_chol2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_perc_change_chol, type="l", ylim = c(0.05, 0.1), xlim = c(0,120), main = "Percentage change cholesterol", 
     ylab="Percentage change", xlab="Time between measurements in The Tromsø Study 4")



ma_HR_chol <- c()
Lower <- c()
Upper <- c()
sd_chol <- sd(tu$abs_d_chol_t4, na.rm = T)
mean_chol <- mean(tu$cholesterol_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_chol_t4, cholesterol_t4, death, 
                                follow_up_time_t42, age_t4, abs_d_chol_t4, sex_t4, 
  ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_chol_t4
              + abs_d_chol_t4*rcs(c.age, 3)
              + rcs(c.age, 3)
              + cholesterol_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_chol_t4=sd_chol, sex_t4=0, cholesterol_t4=mean_chol),
                        b =  list(c.age=70, abs_d_chol_t4=0, sex_t4=0, cholesterol_t4=mean_chol))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_chol[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_chol <- rbind(HR = exp(ma_HR_chol), 
                     Lower = exp(Lower),
                     Upper = exp(Upper))
ma_HR_chol <- as.data.frame(ma_HR_chol)
ma_HR_chol <- t(ma_HR_chol)
ggplot(ma_HR_chol, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.6, 0.8,1,1.2, 1.4, 1.6), 
                    ) +
  xlab('Days between measurements')  + ggtitle("Cholesterol \n Calculated from a window of 7 days") + ylab('Hazard ratio') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_chol_t4, cholesterol_t4, death, 
                              follow_up_time_t42, age_t4, abs_d_chol_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_chol_t4
            + abs_d_chol_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + cholesterol_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=60, abs_d_chol_t4=sd_chol, sex_t4=0, cholesterol_t4=mean_chol),
                      b =  list(c.age=60, abs_d_chol_t4=0, sex_t4=0, cholesterol_t4=mean_chol))                    

print(cont_fit3, fun=exp)


####  trig   ####
daily_median_d_trig <- c()

for (val in 1:120)
{
  daily_median_d_trig[val] = median(subset(data$abs_d_trig_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_trig <- c()
daily_mean_d_trig <- c()
ma_se_trig <- c()
daily_se_trig <- c()
ma_d_median_trig <- c()
perc_change_trig=c()
ma_perc_change_trig <- c()
for (val in 1:120)
{
  daily_mean_d_trig[val] = mean(subset(data$abs_d_trig_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_trig[val] <- sd(subset(data$abs_d_trig_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_trig_t4, data$betweenT4visits==val)))
  perc_change_trig[val] <- mean(subset(data$perc_d_trig_t4, data$betweenT4visits==val), na.rm=T)
  
  
}
for (val in 6:114)
{
  ma_d_trig[val] = mean(daily_mean_d_trig[(val-3):(val+3)], na.rm = T)
  ma_se_trig[val] <- mean(daily_se_trig[(val-3):(val+3)], na.rm = T)
  ma_d_median_trig[val] <- mean(daily_median_d_trig[(val-3):(val+3)], na.rm = T)
  ma_perc_change_trig[val] <- mean(perc_change_trig[(val-3):(val+3)], na.rm = T )
}
trig_daily_prop_1sd_d <- c()
#sum <- summary(data$abs_d_trig_t4) 
sd <- sd(data$d_trig_t4, na.rm=T) 
#sd <- sum[5] 
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_trig_t4>=sd,  na.rm = T)
  trig_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_trig_t4))
  
}
par(mar=c(5,5,5,5))
plot(ma_d_trig, type="l", ylim = c(0, 1.5), xlim = c(0,120), main = "triglycerides", 
     ylab="Absolute difference in mmol/l", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_trig + ma_se_trig*1.96, type="l", lty=2)
lines(ma_d_trig - ma_se_trig*1.96, type="l", lty=2)
lines(ma_d_median_trig, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 14, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(trig_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_trig2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_perc_change_trig, type="l", ylim = c(0.2, 0.3), xlim = c(0,120), main = "Percentage change triglycerides", 
     ylab="Percentage change", xlab="Time between measurements in The Tromsø Study 4")





ma_HR_trig <- c()
Lower <- c()
Upper <- c()
sd_trig <- sd(tu$abs_d_trig_t4, na.rm = T)
mean_trig <- mean(tu$triglycerides_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_trig_t4, triglycerides_t4, death, 
                                follow_up_time_t42, age_t4, abs_d_trig_t4, sex_t4, 
  ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_trig_t4
              + abs_d_trig_t4*rcs(c.age, 3)
              + rcs(c.age, 3)
              + triglycerides_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_trig_t4=sd_trig, sex_t4=0, triglycerides_t4=mean_trig),
                        b =  list(c.age=70, abs_d_trig_t4=0, sex_t4=0, triglycerides_t4=mean_trig))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_trig[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_trig <- rbind(HR = exp(ma_HR_trig), 
                    Lower = exp(Lower),
                    Upper = exp(Upper))
ma_HR_trig <- as.data.frame(ma_HR_trig)
ma_HR_trig <- t(ma_HR_trig)
ggplot(ma_HR_trig, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.6, 0.8, 1, 1.2, 1.4) 
                    ) +
  xlab('Days between measurements')  + ggtitle("Triglycerides \n Calculated from a window of 7 days") + ylab('') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  


d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_trig_t4, triglycerides_t4, death, 
                              follow_up_time_t42, age_t4, abs_d_trig_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_trig_t4
            + abs_d_trig_t4*rcs(c.age, 3)
            + rcs(c.age, 3)
            + triglycerides_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_trig_t4=sd_trig, sex_t4=0, triglycerides_t4=mean_trig),
                      b =  list(c.age=70, abs_d_trig_t4=0, sex_t4=0, triglycerides_t4=mean_trig))                    
 
print(cont_fit3, fun=exp)

#### hdl ####
daily_median_d_hdl <- c()

for (val in 1:120)
{
  daily_median_d_hdl[val] = median(subset(data$abs_d_hdl_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_hdl <- c()
daily_mean_d_hdl <- c()
ma_se_hdl <- c()
daily_se_hdl <- c()
ma_d_median_hdl <- c()
perc_change_hdl=c()
ma_perc_change_hdl <- c()
for (val in 1:120)
{
  daily_mean_d_hdl[val] = mean(subset(data$abs_d_hdl_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_hdl[val] <- sd(subset(data$abs_d_hdl_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_hdl_t4, data$betweenT4visits==val)))
  perc_change_hdl[val] <- mean(subset(data$perc_d_hdl_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_hdl[val] = mean(daily_mean_d_hdl[(val-3):(val+3)], na.rm = T)
  ma_se_hdl[val] <- mean(daily_se_hdl[(val-3):(val+3)], na.rm = T)
  ma_d_median_hdl[val] <- mean(daily_median_d_hdl[(val-3):(val+3)], na.rm = T)
  ma_perc_change_hdl[val] <- mean(perc_change_hdl[(val-3):(val+3)], na.rm = T )
}

hdl_daily_prop_1sd_d <- c()

ma_hdl_prop_1sd_d <- c()
sd <- sd(data$d_hdl_t4, na.rm=T)
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_hdl_t4>=sd,  na.rm = T)
  hdl_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_hdl_t4))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_hdl, type="l", ylim = c(0, 0.4), xlim = c(0,120), main = "hdl", 
     ylab="Absolute difference in mmol/l", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_hdl + ma_se_hdl*1.96, type="l", lty=2)
lines(ma_d_hdl - ma_se_hdl*1.96, type="l", lty=2)
lines(ma_d_median_hdl, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 0.4, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 1 SD in the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(hdl_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_hdl2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_perc_change_hdl, type="l", ylim = c(0.08, 0.2), xlim = c(0,120), main = "Percentage change hdl", 
     ylab="Percentage change", xlab="Time between measurements in The Tromsø Study 4")



ma_HR_hdl <- c()

for (val in 1:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  cox_hdl <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                     hdl_t4
                   + strata(sex_t4)
                   + abs_d_hdl_t4
                   ,data = d)
  ma_HR_hdl[val] <- cox_hdl$coefficients[2]
}
plot(ma_HR_hdl, type="l")

ma_HR_hdl <- c()
Lower <- c()
Upper <- c()
sd_hdl <- sd(tu$abs_d_hdl_t4, na.rm = T)
mean_hdl <- mean(tu$hdl_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_hdl_t4, hdl_t4, death, 
                                follow_up_time_t42, age_t4, abs_d_hdl_t4, sex_t4, 
  ))
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                abs_d_hdl_t4
              + abs_d_hdl_t4*rcs(c.age, 4)
              + rcs(c.age, 4)
              + hdl_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_hdl_t4=sd_hdl, sex_t4=0, hdl_t4=mean_hdl),
                        b =  list(c.age=70, abs_d_hdl_t4=0, sex_t4=0, hdl_t4=mean_hdl))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_hdl[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_hdl <- rbind(HR = exp(ma_HR_hdl), 
                    Lower = exp(Lower),
                    Upper = exp(Upper))
ma_HR_hdl <- as.data.frame(ma_HR_hdl)
ma_HR_hdl <- t(ma_HR_hdl)
ggplot(ma_HR_hdl, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)) +
  xlab('Days between measurements')  + ggtitle("High density lipoprotein \n Calculated from a window of 7 days") + ylab('Hazard ratio') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_hdl_t4, hdl_t4, death, 
                              follow_up_time_t42, age_t4, abs_d_hdl_t4, sex_t4, 
))

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              abs_d_hdl_t4
            + abs_d_hdl_t4*rcs(c.age, 4)
            + rcs(c.age, 4)
            + hdl_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_hdl_t4=sd_hdl, sex_t4=0, hdl_t4=mean_hdl),
                      b =  list(c.age=70, abs_d_hdl_t4=0, sex_t4=0, hdl_t4=mean_hdl))                    
print(cont_fit3, fun=exp)

#### ggt ####
daily_median_d_ggt <- c()

for (val in 1:120)
{
  daily_median_d_ggt[val] = median(subset(data$abs_d_ggt_t4, data$betweenT4visits==val), na.rm = T)
}
ma_d_ggt <- c()
daily_mean_d_ggt <- c()
ma_se_ggt <- c()
daily_se_ggt <- c()
ma_d_median_ggt <- c()
perc_change_ggt=c()
ma_perc_change_ggt <- c()
for (val in 1:120)
{
  daily_mean_d_ggt[val] = mean(subset(data$abs_d_ggt_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_ggt[val] <- sd(subset(data$abs_d_ggt_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_ggt_t4, data$betweenT4visits==val)))
  perc_change_ggt[val] <- mean(subset(data$perc_d_ggt_t4, data$betweenT4visits==val), na.rm=T)
}
for (val in 6:114)
{
  ma_d_ggt[val] = mean(daily_mean_d_ggt[(val-3):(val+3)], na.rm = T)
  ma_se_ggt[val] <- mean(daily_se_ggt[(val-3):(val+3)], na.rm = T)
  ma_d_median_ggt[val] <- mean(daily_median_d_ggt[(val-3):(val+3)], na.rm = T)
  ma_perc_change_ggt[val] <- mean(perc_change_ggt[(val-3):(val+3)], na.rm = T )
}

ggt_daily_prop_1sd_d <- c()
sum <- summary(data$abs_d_ggt_t4) 
#sd <- sd(data$d_ggt_t4) #Long tailed distr
sd <- sum[5] #extremely long tail, using 3rd quartile
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d[val] <- sum(d$abs_d_ggt_t4>=sd,  na.rm = T)
  ggt_daily_prop_1sd_d[val] = num_1sd_d[val]/sum(!is.na(d$abs_d_ggt_t4))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_ggt, type="l", ylim = c(0, 16), xlim = c(0,120), main = "ggt", 
     ylab="Absolute difference in U/L", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_ggt + ma_se_ggt*1.96, type="l", lty=2)
lines(ma_d_ggt - ma_se_ggt*1.96, type="l", lty=2)
lines(ma_d_median_ggt, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 14,legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 3. quartile from the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(ggt_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_ggt2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_num_total_11, type="l", ylim = c(0, 1800), xlim = c(0,120), main = "Number in Window", 
     ylab="Total number in Window", xlab="Time between measurements in The Tromsø Study 4")

plot(ggt_daily_prop_1sd_d, type="l")

ma_HR_ggt <- c()

for (val in 1:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  cox_ggt <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                       ggt_t4
                     + strata(sex_t4)
                     + abs_d_ggt_t4
                     ,data = d)
  ma_HR_ggt[val] <- cox_ggt$coefficients[2]
}
plot(ma_HR_ggt, type="l")


ma_HR_ggt <- c()
Lower <- c()
Upper <- c()
sd_ggt <- sd(tu$abs_d_ggt_t4, na.rm = T)
mean_ggt <- mean(tu$ggt_t4, na.rm = T)
for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(Mean_ggt_t4, ggt_t4, ggt_t42, death, 
                                follow_up_time_t42, age_t4, abs_d_ggt_t4, sex_t4, 
  ))
#tu2 <- subset(tu2, ggt_t4<200 & ggt_t42<200) # some really high values that could be an indicator of disease
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                rcs(abs_d_ggt_t4, 3)
              + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3)
              + rcs(c.age, 3)
              + ggt_t4
              + strat(sex_t4)
              ,data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, abs_d_ggt_t4=sd_ggt, sex_t4=0, ggt_t4=mean_ggt),
                        b =  list(c.age=70, abs_d_ggt_t4=0, sex_t4=0, ggt_t4=mean_ggt))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_ggt[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_ggt <- rbind(HR = exp(ma_HR_ggt), 
                   Lower = exp(Lower),
                   Upper = exp(Upper))
ma_HR_ggt <- as.data.frame(ma_HR_ggt)
ma_HR_ggt <- t(ma_HR_ggt)
ggplot(ma_HR_ggt, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', breaks=c(0.2 ,0.4, 0.6, 0.8, 1, 1.5, 2, 3 )) +
  xlab('Days between measurements')  + ggtitle("GGT \n Calculated from a window of 7 days") + ylab('Hazard ratio') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  

d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(Mean_ggt_t4, ggt_t4, ggt_t42, death, 
                              follow_up_time_t42, age_t4, abs_d_ggt_t4, sex_t4, 
))
#tu2 <- subset(tu2, ggt_t4<200 & ggt_t42<200) # some really high values that could be an indicator of disease

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              rcs(abs_d_ggt_t4, 3)
            + rcs(abs_d_ggt_t4, 3)*rcs(c.age, 3)
            + rcs(c.age, 3)
            + ggt_t4
            + strat(sex_t4)
            ,data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, abs_d_ggt_t4=sd_ggt, sex_t4=0, ggt_t4=mean_ggt),
                      b =  list(c.age=70, abs_d_ggt_t4=0, sex_t4=0, ggt_t4=mean_ggt))                    
print(cont_fit3, fun=exp)





#### SUM OF ALL d BIOMARKERS ####

daily_median_d_sum <- c()

for (val in 1:120)
{
  daily_median_d_sum[val] = median(subset(data$sum_abs_d, data$betweenT4visits==val), na.rm = T)
}
ma_d_sum <- c()
daily_mean_d_sum <- c()
ma_se_sum <- c()
daily_se_sum <- c()
ma_d_median_sum <- c()

for (val in 1:120)
{
  daily_mean_d_sum[val] = mean(subset(data$sum_abs_d, data$betweenT4visits==val), na.rm = T)
  daily_se_sum[val] <- sd(subset(data$sum_abs_d, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$sum_abs_d, data$betweenT4visits==val)))
}
for (val in 6:114)
{
  ma_d_sum[val] = mean(daily_mean_d_sum[(val-3):(val+3)], na.rm = T)
  ma_se_sum[val] <- mean(daily_se_sum[(val-3):(val+3)], na.rm = T)
  ma_d_median_sum[val] <- mean(daily_median_d_sum[(val-3):(val+3)], na.rm = T)
}

sum_daily_prop_1sd_d <- c()
sum <- summary(data$sum_abs_d) 
sum
#sd <- sd(data$d_ggt_t4) #Long tailed distr
sd <- sum[5] #using 3rd quartile
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  num_1sd_d <- sum(d$sum_abs_d>=sd,  na.rm = T)
  sum_daily_prop_1sd_d[val] = num_1sd_d/sum(!is.na(d$sum_abs_d))
  
}

par(mar=c(5,5,5,5))
plot(ma_d_sum, type="l", ylim = c(3, 7), xlim = c(0,120), main = "sum", 
     ylab="Absolute difference in U/L", xlab="Time between measurements in The Tromsø Study 4")
lines(ma_d_sum + ma_se_sum*1.96, type="l", lty=2)
lines(ma_d_sum - ma_se_sum*1.96, type="l", lty=2)
lines(ma_d_median_sum, type="l", lty=1, col="blue")
legend(-2.5, 7,legend=c("Moving average of mean including confidence interval", "Moving average of median", "Proportion over 3 quartile from the whole sample", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
#box()
#par(new=T)
#plot(seg, add = TRUE, col="blue", ylim = c(0, 2.5), yaxt = "n", axes= FALSE, xlab = "", ylab="", xlim=c(0,120))
box()
par(new=T)
plot(sum_daily_prop_1sd_d, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(0.1, 0.8), xlim = c(0,120), )
axis(4, las=1)
mtext("Proportion", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_sum2.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

plot(ma_num_total_11, type="l", ylim = c(0, 1800), xlim = c(0,120), main = "Number in Window", 
     ylab="Total number in Window", xlab="Time between measurements in The Tromsø Study 4")


ma_HR_sum <- c()

for (val in 1:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  cox_sum <- coxph(Surv(age_t4, follow_up_time_t42/365.25+age_t4, death) ~ 
                   + strata(sex_t4)
                   + sum_abs_d
                   ,data = d)
  ma_HR_sum[val] <- cox_sum$coefficients[1]
}
plot(ma_HR_sum, type="l")


ma_HR_sum <- c()
Lower <- c()
Upper <- c()
sd_sum <- sd(tu$sum_abs_d, na.rm = T)
mean_sum <- mean(tu$sum_abs_d, na.rm = T)
quintile <- quantile(tu$sum_abs_d, c(.1, .3, .5, .7, .9), na.rm = T)

for (val in 4:65)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  tu2 <- d %>% dplyr::select (c(sum_abs_d, death, 
                                follow_up_time_t42, age_t4, sex_t4, 
  ))
#  tu2 <- subset(tu2, ggt_t4<200 & ggt_t42<200) # some really high values that could be an indicator of disease
  
  dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))
  
  tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
  tdata$tstart <- tdata$tstart/10
  tdata$tstop <- tdata$tstop/10
  tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
  dd <- datadist(tdata)
  fit3 <- cph(Surv(tstart, tstop, death) ~  
                + sum_abs_d*rcs(c.age, 3)
              + rcs(c.age, 3)
              + strat(sex_t4)
              , data = tdata)
  
  cont_fit3 <- contrast(fit3, 
                        a = list(c.age=70, sum_abs_d=quintile[5], sex_t4=0),
                        b =  list(c.age=70, sum_abs_d=quintile[1], sex_t4=0))                    
  
  #print(cont_fit3, fun=exp)
  
  
  ma_HR_sum[val] <- cont_fit3$Contrast[1]
  Lower[val] <-       cont_fit3$Lower[1]
  Upper[val] <-        cont_fit3$Upper[1]
}

ma_HR_sum <- rbind(HR = exp(ma_HR_sum), 
                   Lower = exp(Lower),
                   Upper = exp(Upper))
ma_HR_sum <- as.data.frame(ma_HR_sum)
ma_HR_sum <- t(ma_HR_sum)
plot_MA_sum <- ggplot(ma_HR_sum, aes(x=1:65)) + geom_line(aes(y=HR), col = "darkred") +
  geom_ribbon(aes(ymin=Lower, ymax=Upper),
              alpha=0.1, linetype=0, fill = "darkred") + 
  geom_line(aes(y=1), col = "black") +
  scale_y_continuous(trans='log10', n.breaks=5, 
                     minor_breaks=c(seq(0.1, 1, by=.1), seq(1, 10, by=.5)), breaks= c(0.1, 0.5, 1, 2, 3, 5, 10)) + 
  xlab('Days between measurements')  + ggtitle("Sumscore \n Calculated from a window of 7 days") + ylab('Hazard ratio') +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 11))  
plot_MA_sum
# Add histogram as facet
ma_num_total_7 <- c()
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits>=(val-3) & data$betweenT4visits<=(val+3))
  
  ma_num_total_7[val] <- sum(!is.na(d$sex_t4))
}
# ma_num_total_7 <- as.data.frame(ma_num_total_7 <- ma_num_total_7)

plot(ma_num_total_7, type="h", lwd=5, ylim = c(0, 1200) , xlim = c(0, 120), main = "Number of participants in 7-day moving window", 
     ylab="Number of participants ", xlab="Time between measurements in The Tromsø Study 4")

# combined_plot <- plot_MA_sum +
#   scale_y_continuous(
#     
#   geom_col(aes(x = 1:65, y = ma_num_total_7),
#                  fill = "blue",  alpha = 0.5) +
#   facet_grid(~. , scales = "free_y", space = "free") 
# 
#   
# combined_plot
d <- subset(data, data$betweenT4visits>=65)
tu2 <- d %>% dplyr::select (c(sum_abs_d, ggt_t4, ggt_t42, death, 
                              follow_up_time_t42, age_t4, abs_d_ggt_t4, sex_t4, 
))
#  tu2 <- subset(tu2, ggt_t4<200 & ggt_t42<200) # some really high values that could be an indicator of disease

dtimes <- sort(unique(with(tu2, ceiling(follow_up_time_t42/36.525)[death==1])))

tdata <- survSplit(Surv(ceiling(follow_up_time_t42/36.525), death) ~., tu2, cut=dtimes)
tdata$tstart <- tdata$tstart/10
tdata$tstop <- tdata$tstop/10
tdata$c.age <- tdata$age_t4 + tdata$tstop #current age
dd <- datadist(tdata)
fit3 <- cph(Surv(tstart, tstop, death) ~  
              + sum_abs_d*rcs(c.age, 3)
            + rcs(c.age, 3)
            + strat(sex_t4)
            , data = tdata)

cont_fit3 <- contrast(fit3, 
                      a = list(c.age=70, sum_abs_d=quintile[5], sex_t4=0),
                      b =  list(c.age=70, sum_abs_d=quintile[1], sex_t4=0)) 
print(cont_fit3, fun=exp)
