# Exploring differences
## Checking if difference is associated with mean or time between visits ----
source("~/postdocHC/load.R")
tu <- readRDS("~/postdocHC/tu.rds")
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
## age sex and education over time ----
ma_mean_age_t4 <- c()
daily_mean_age_t4 <- c()
daily_se_age_t4 <- c()
ma_se_age <- c()
for (val in 1:120)
{
  daily_mean_age_t4[val] = mean(subset(data$age_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_age_t4[val] = sd(subset(data$age_t4, data$betweenT4visits==val)/
                              sqrt(length(subset(data$age_t4, data$betweenT4visits==val))), na.rm = T)
  
  
}
for (val in 4:116)
{
  ma_mean_age_t4[val] = mean(daily_mean_age_t4[(val-3):(val+3)], na.rm = T)
  ma_se_age[val] <- mean(daily_se_age_t4[(val-3):(val+3)], na.rm = T)
  
}
plot(ma_mean_age_t4, type="l", ylim = c(40, 70 ), main = "7 days moving average of age", 
     ylab="Age in years", xlab="Time between visits in The Tromsø Study 4")
lines(ma_mean_age_t4 + ma_se_age*1.96, type="l", lty=2) # confidence interval constructed by a moving average of standard error. 
lines(ma_mean_age_t4 - ma_se_age*1.96, type="l", lty=2)

daily_prop_men <- c()
num_total <- c()
ma_prop_men <- c()
for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits==val)
  num_male <- sum(d$sex_t4==1,  na.rm = T)
  num_female <- sum(d$sex_t4==0,  na.rm = T)
  num_total[val] <- num_male+num_female
  daily_prop_men[val] = num_male/(num_male+num_female)
  
}

for (val in 4:116)
{
  ma_prop_men[val] <- mean(daily_prop_men[(val-3):(val+3)])          
}

plot_ma_prop_men <- plot(ma_prop_men, type="l", ylim = c(0, 1 ), ylab="Proportion men", xlab="Time between visits in The Tromsø Study 4")

daily_prop_edu_1 <- c()
daily_prop_edu_2 <- c()
daily_prop_edu_3 <- c()
daily_prop_edu_4 <- c()
daily_prop_edu_5 <- c()
daily_prop_edu_test <- c()

ma_prop_edu_1 <- c()
ma_prop_edu_2 <- c()
ma_prop_edu_3 <- c()
ma_prop_edu_4 <- c()
ma_prop_edu_5 <- c()
ma_prop_edu_test <- c()

for (val in 1:120)
{
  d <- subset(data, data$betweenT4visits==val)
  num_edu_1 <- sum(d$education_t4==1,  na.rm = T)
  num_edu_2 <- sum(d$education_t4==2,  na.rm = T)
  num_edu_3 <- sum(d$education_t4==3,  na.rm = T)
  num_edu_4 <- sum(d$education_t4==4,  na.rm = T)
  num_edu_5 <- sum(d$education_t4==5,  na.rm = T)
  
  num_edu <- sum(!is.na(d$education_t4),  na.rm = T)
  daily_prop_edu_1[val] = num_edu_1/num_edu
  daily_prop_edu_2[val] = num_edu_2/num_edu
  daily_prop_edu_3[val] = num_edu_3/num_edu
  daily_prop_edu_4[val] = num_edu_4/num_edu
  daily_prop_edu_5[val] = num_edu_5/num_edu
  daily_prop_edu_test[val] = daily_prop_edu_1[val]+ daily_prop_edu_2[val] +daily_prop_edu_3[val] + daily_prop_edu_4[val] + daily_prop_edu_5[val]
  
}

for (val in 4:116)
{
  
  ma_prop_edu_1[val] <-  mean(daily_prop_edu_1[(val-3):(val+3)], na.rm=T)
  ma_prop_edu_2[val] <-  mean(daily_prop_edu_2[(val-3):(val+3)], na.rm=T)
  ma_prop_edu_3[val] <-  mean(daily_prop_edu_3[(val-3):(val+3)], na.rm=T)
  ma_prop_edu_4[val] <-  mean(daily_prop_edu_4[(val-3):(val+3)], na.rm=T)
  ma_prop_edu_5[val] <-  mean(daily_prop_edu_5[(val-3):(val+3)], na.rm=T)
  ma_prop_edu_test[val] <-  mean(daily_prop_edu_test[(val-3):(val+3)], na.rm=T)
}
summary(daily_prop_edu_test)
summary(ma_prop_edu_test)
plot_ma_edu <- plot(ma_prop_edu_1, type="l", ylim = c(0, 1 ), ylab="Proportion", 
                    main = "Moving average of daily proportions of participants with different education "
                    , xlab="Time between visits in The Tromsø Study 4")
legend(-2.5, 1, legend=c("7-10 years primary/secondary school, modern secondary school"
                         , "Technical school, middle school, vocational school, 1-2 years senior high school"
                         , "High school diploma (3-4 years)"
                         , "College/university, less than 4 years"
                         , "College/university, 4 or more years"),
       col=c("black", "blue", "yellow", "red", "orange"), lty=c(1,1,1,1,1), cex=0.7)
lines(ma_prop_edu_2, type="l", col="blue")
lines(ma_prop_edu_3, type="l", col="yellow")
lines(ma_prop_edu_4, type="l", col="red")
lines(ma_prop_edu_5, type="l", col="orange")



hist(data$betweenT4visits, breaks = 120, main="Frequency of attendance at different intervals of time between visits", xlab="Time between visits")

#### diastolic ####
data <- subset(tu,  betweenT4visits<121             )
hist( data$d_diabp_t4, breaks=100, 
      main = paste("Histogram of difference between visit 1 and 2 in Tromsø4"),
      xlab = "Change in diastolic blood pressure")
summary(data$d_diabp_t4)
plot(data$Mean_diabp_t4, data$d_diabp_t4)
abline(lm(data$d_diabp_t4 ~ data$Mean_diabp_t4), col = "red")

cor.test(data$abs_d_diabp_t4, data$Mean_diabp_t4)

data <- tu %>% drop_na(abs_d_diabp_t4)
# data <- data %>% drop_na(education_t4)
# data <- data %>% drop_na(smoking_t4)

data <- subset(data, betweenT4visits<121)
#data <- subset(data, age_t4<100 & age_t4>=70)

spline_fit_diabp <- lm(log(abs_d_diabp_t4+0.01) ~ 
                         + sex_t4 + ns(age_t4, 3) + ns(betweenT4visits, knots = c(10, 20, 30))
                       #+ as.factor(education_t4) + smoking_t4
                       , data = data)
anova(spline_fit_diabp)
termplot(spline_fit_diabp, term=3)

summary(spline_fit_diabp)
hist(spline_fit_diabp$residuals)
fitted_diabp <- exp(spline_fit_diabp$fitted.values)
data <- data %>% ungroup() %>%
  mutate(fitted_diabp = fitted_diabp)
head(data$fitted_diabp)
term <- as.vector(predict(spline_fit_diabp, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>%
  mutate(term = term)
data <- data[order(data$betweenT4visits),]
plot(data$betweenT4visits, data$term)


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
term=c()
for (val in 1:120)
{
  daily_mean_d_diabp[val] = mean(subset(data$abs_d_diabp_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_diabp[val] <- sd(subset(data$abs_d_diabp_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_diabp_t4, data$betweenT4visits==val)))
  term[val] =mean(subset(data$term, data$betweenT4visits==val), na.rm = T) 
}
for (val in 4:116)
{
  ma_d_diabp[val] = mean(daily_mean_d_diabp[(val-3):(val+3)], na.rm = T)
  ma_se_diabp[val] <- mean(daily_se_diabp[(val-3):(val+3)], na.rm = T)
  ma_d_median_diabp[val] <- mean(daily_median_d_diabp[(val-3):(val+3)], na.rm = T)
  
}


par(mar=c(5,5,5,5))
plot(ma_d_diabp, type="l", ylim = c(0, 14), xlim = c(0,120), main = "Diastolic blood pressure", 
     ylab="Absolute difference in mmHg", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_diabp + ma_se_diabp*1.96, type="l", lty=2)
lines(ma_d_diabp - ma_se_diabp*1.96, type="l", lty=2)
lines(ma_d_median_diabp, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 14, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.8)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_diabp.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()


#### systolic ####
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
hist(data$d_sysbp_t4, breaks = 100)
summary(data$d_sysbp_t4)
plot(data$Mean_sysbp_t4, data$d_sysbp_t4)
abline(lm(data$d_sysbp_t4 ~ data$Mean_sysbp_t4), col = "red")
lm <- lm(data$d_sysbp_t4 ~ data$Mean_sysbp_t4)
plot(data$Mean_sysbp_t4, log(data$abs_d_sysbp_t4))
abline(lm(log(data$abs_d_sysbp_t4+1) ~ data$Mean_sysbp_t4), col = "red")
lm <- lm(log(data$abs_d_sysbp_t4) ~ data$Mean_sysbp_t4)
summary(lm)

plot(data$betweenT4visits, data$d_sysbp_t4)
abline(lm(data$d_sysbp_t4 ~ data$betweenT4visits), col = "red")
cor.test(data$betweenT4visits, data$d_sysbp_t4)

plot(log(data$betweenT4visits), log(data$abs_d_sysbp_t4+1))
abline(lm(log(data$abs_d_sysbp_t4+1) ~ log(data$betweenT4visits), na.action = na.exclude), col = "red")
lm <- lm(log(data$abs_d_sysbp_t4+1) ~ log(data$betweenT4visits), na.action = na.exclude)
summary(lm)

cor.test(data$abs_d_sysbp_t4, data$Mean_sysbp_t4)


data <- tu %>% drop_na(abs_d_sysbp_t4)
data <- subset(data, betweenT4visits<121)
spline_fit_sysbp <- lm(log(abs_d_sysbp_t4+0.01) ~ 
                         + sex_t4 + ns(age_t4, 3)+ ns(betweenT4visits, knots = c(10, 20, 30))
                       #  + as.factor(education_t4) + smoking_t4
                       , data = data)
anova(spline_fit_sysbp)
termplot(spline_fit_sysbp, term=3)
summary(spline_fit_sysbp)
hist(spline_fit_sysbp$residuals)
fitted_sysbp <- exp(spline_fit_sysbp$fitted.values)
data <- data %>% ungroup() %>% mutate(fitted_sysbp = fitted_sysbp)
head(data$fitted_sysbp)
term <- as.vector(predict(spline_fit_sysbp, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]

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

for (val in 1:120)
{
  daily_mean_d_sysbp[val] = mean(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_sysbp[val] <- sd(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_sysbp_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_sysbp[val] = mean(daily_mean_d_sysbp[(val-3):(val+3)], na.rm = T)
  ma_se_sysbp[val] <- mean(daily_se_sysbp[(val-3):(val+3)], na.rm = T)
  ma_d_median_sysbp[val] <- mean(daily_median_d_sysbp[(val-3):(val+3)], na.rm = T)
  
}

par(mar=c(5,5,5,5))
plot(ma_d_sysbp, type="l", ylim = c(3, 20), xlim = c(0,120), main = "Systolic blood pressure", 
     ylab="Absolute difference in mmHg", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_sysbp + ma_se_sysbp*1.96, type="l", lty=2)
lines(ma_d_sysbp - ma_se_sysbp*1.96, type="l", lty=2)
lines(ma_d_median_sysbp, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 20, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.5)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_sysbp.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()


#### heart rate #### 
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
hist(data$d_hr_t4, breaks = 100)
summary(data$d_hr_t4)
plot(data$Mean_hr_t4, data$d_hr_t4)
abline(lm(data$d_hr_t4 ~ data$Mean_hr_t4), col = "red")
lm <- lm(data$d_hr_t4 ~ data$Mean_hr_t4)
summary(lm)

plot(data$Mean_hr_t4, log(data$abs_d_hr_t4+1))
abline(lm(log(data$abs_d_hr_t4+1) ~ data$Mean_hr_t4), col = "red")
lm <- lm(log(data$abs_d_hr_t4+1) ~ data$Mean_hr_t4)
summary(lm)

plot(data$Mean_hr_t4, data$abs_d_hr_t4)
abline(lm(data$abs_d_hr_t4 ~ data$Mean_hr_t4), col = "red")
lm <- lm(data$abs_d_hr_t4 ~ data$Mean_hr_t4)
summary(lm)

plot(data$betweenT4visits,data$d_hr_t4)
abline(lm(data$d_hr_t4 ~ data$betweenT4visits), col = "red")
cor.test(data$betweenT4visits, data$d_hr_t4)
lm <- lm(data$d_hr_t4 ~ data$betweenT4visits)
summary(lm)

plot(log(data$betweenT4visits),log(data$abs_d_hr_t4+1))
abline(lm(log(data$abs_d_hr_t4+1) ~ log(data$betweenT4visits)),  col = "red")
lm <- lm(log(data$abs_d_hr_t4+1) ~ log(data$betweenT4visits))
summary(lm)


data <- tu %>% drop_na(abs_d_hr_t4)
data <- subset(data, betweenT4visits<121)
spline_fit_hr <- lm(log(abs_d_hr_t4+0.01) ~ 
                      + sex_t4 + ns(age_t4, 3)+ ns(betweenT4visits, knots = c(20, 40, 60, 80, 100))
                    #  + as.factor(education_t4) + smoking_t4
                    , data = data)
anova(spline_fit_hr)
termplot(spline_fit_hr, term=3)
summary(spline_fit_hr)
hist(spline_fit_hr$residuals)
fitted_hr <- exp(spline_fit_hr$fitted.values)
data <- data %>% ungroup() %>% mutate(fitted_hr = fitted_hr)
head(data$fitted_hr)
term <- as.vector(predict(spline_fit_hr, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]

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

for (val in 1:120)
{
  daily_mean_d_hr[val] = mean(subset(data$abs_d_hr_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_hr[val] <- sd(subset(data$abs_d_hr_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_hr_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_hr[val] = mean(daily_mean_d_hr[(val-3):(val+3)], na.rm = T)
  ma_se_hr[val] <- mean(daily_se_hr[(val-3):(val+3)], na.rm = T)
  ma_d_median_hr[val] <- mean(daily_median_d_hr[(val-3):(val+3)], na.rm = T)
  
}



par(mar=c(5,5,5,5))
plot(ma_d_hr, type="l", ylim = c(0, 17), xlim = c(0,120), main = "Heart rate", 
     ylab="Absolute difference in beats per minute", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_hr + ma_se_hr*1.96, type="l", lty=2)
lines(ma_d_hr - ma_se_hr*1.96, type="l", lty=2)
lines(ma_d_median_hr, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 17.5, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.5)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_hr.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()


#### cholesterol ####
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
hist( data$d_chol_t4, breaks=100)
summary(data$d_chol_t4)
plot(data$Mean_chol_t4, data$d_chol_t4)
abline(lm(data$d_chol_t4 ~ data$Mean_chol_t4), col = "red")
lm <- lm(data$d_chol_t4 ~ data$Mean_chol_t4)
summary(lm)

plot(data$Mean_chol_t4, data$abs_d_chol_t4)
abline(lm(data$abs_d_chol_t4 ~ data$Mean_chol_t4), col = "red")
lm <- lm(data$abs_d_chol_t4 ~ data$Mean_chol_t4)
summary(lm)

plot(data$Mean_chol_t4, log(1+data$abs_d_chol_t4))
abline(lm(log(data$abs_d_chol_t4+1) ~ data$Mean_chol_t4), col = "red")
lm <- lm(log(1+data$abs_d_chol_t4) ~ data$Mean_chol_t4)
summary(lm)

plot(data$betweenT4visits, data$d_chol_t4)
abline(lm(data$d_chol_t4 ~ data$betweenT4visits), col = "red")
lm <- lm(data$d_chol_t4 ~ data$betweenT4visits)
summary(lm)

plot(data$betweenT4visits, data$abs_d_chol_t4)
abline(lm(data$abs_d_chol_t4 ~ data$betweenT4visits), col = "red")
lm <- lm(data$abs_d_chol_t4 ~ data$betweenT4visits)
summary(lm)
cor.test(log(data$betweenT4visits), log(1 + data$abs_d_chol_t4))

plot(log(data$betweenT4visits),  log(1 + data$abs_d_chol_t4))
abline(lm(log(1 + data$abs_d_chol_t4) ~ log(data$betweenT4visits)), col = "red")
lm <- lm(log(data$abs_d_chol_t4+1) ~ log(data$betweenT4visits))
summary(lm)



data <- tu %>% drop_na(abs_d_chol_t4)

data <- subset(data, betweenT4visits<121)



#simple model
spline_fit_chol <- lm(log(abs_d_chol_t4+0.01) ~ 
                        + sex_t4 + ns(age_t4, 3) + ns(betweenT4visits, knots = c(50, 70, 90))
                      , data = data)
anova(spline_fit_chol)
summary(spline_fit_chol)

termplot(spline_fit_chol, term=3)
hist(spline_fit_chol$residuals)
fitted_chol <- exp(spline_fit_chol$fitted.values)
data <- data %>% ungroup() %>% mutate(fitted_chol = fitted_chol)
head(data$fitted_chol)
term <- as.vector(predict(spline_fit_chol, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]

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

for (val in 1:120)
{
  daily_mean_d_chol[val] = mean(subset(data$abs_d_chol_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_chol[val] <- sd(subset(data$abs_d_chol_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_chol_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_chol[val] = mean(daily_mean_d_chol[(val-3):(val+3)], na.rm = T)
  ma_se_chol[val] <- mean(daily_se_chol[(val-3):(val+3)], na.rm = T)
  ma_d_median_chol[val] <- mean(daily_median_d_chol[(val-3):(val+3)], na.rm = T)
  
}
par(mar=c(5,5,5,5))
plot(ma_d_chol, type="l", ylim = c(0, 1.3), xlim = c(0,120), main = "Cholesterol", 
     ylab="Absolute difference in mmol/l", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_chol + ma_se_chol*1.96, type="l", lty=2)
lines(ma_d_chol - ma_se_chol*1.96, type="l", lty=2)
lines(ma_d_median_chol, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 1.3, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.5)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_chol.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()




#### triglycerides ####
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
hist( data$d_trig_t4, breaks=100)
summary(data$d_trig_t4)
plot(data$Mean_trig_t4, data$d_trig_t4)
abline(lm(data$d_trig_t4 ~ data$Mean_trig_t4), col = "red")
lm <- lm(data$d_trig_t4 ~ data$Mean_trig_t4)
summary(lm)

plot(data$Mean_trig_t4, data$abs_d_trig_t4)
abline(lm(data$abs_d_trig_t4 ~ data$Mean_trig_t4), col = "red")
lm <- lm(data$abs_d_trig_t4 ~ data$Mean_trig_t4)
summary(lm)

plot(log(data$Mean_trig_t4), log(1+data$abs_d_trig_t4))
abline(lm(log(data$abs_d_trig_t4+1) ~ log(data$Mean_trig_t4)), col = "red")
lm <- lm(log(1+data$abs_d_trig_t4) ~ log(data$Mean_trig_t4))
summary(lm)

plot(data$betweenT4visits, data$d_trig_t4)
abline(lm(data$d_trig_t4 ~ data$betweenT4visits), col = "red")
lm <- lm(data$d_trig_t4 ~ data$betweenT4visits)
summary(lm)

plot(data$betweenT4visits, data$abs_d_trig_t4)
abline(lm(data$abs_d_trig_t4 ~ data$betweenT4visits), col = "red")
cor.test(log(data$betweenT4visits), log(1 + data$abs_d_trig_t4))

plot(log(data$betweenT4visits),  log(1 + data$abs_d_trig_t4))
abline(lm(log(1 + data$abs_d_trig_t4) ~ log(data$betweenT4visits)), col = "red")
lm <- lm(log(data$abs_d_trig_t4+1) ~ log(data$betweenT4visits))
summary(lm)

plot(data$date_t42, data$d_trig_t4)
cor.test(as.numeric(data$date_t42), data$d_trig_t4)
lm(data$d_trig_t4 ~ data$date_t42)
plot(data$date_t4, log(data$triglycerides_t4))
abline(lm(log(data$triglycerides_t4) ~ data$Date_T4), col = "red")
plot(data$date_t42, log(data$triglycerides_t42))
cor.test(as.numeric(data$date_t42), data$triglycerides_t42)
cor.test(as.numeric(data$date_t4), data$triglycerides_t4)

data <- tu %>% drop_na(abs_d_trig_t4)

data <- subset(data, betweenT4visits<121)
spline_fit_trig <- lm(log(abs_d_trig_t4+0.01) ~ 
                        + sex_t4 + age_t4 + ns(betweenT4visits, knots = c(20, 30, 50, 70, 100))
                      , data = data)
anova(spline_fit_trig)
termplot(spline_fit_trig, term=3)
summary(spline_fit_trig)
hist(spline_fit_trig$residuals)
fitted_trig <- exp(spline_fit_trig$fitted.values)
data <- data %>% ungroup() %>% mutate(fitted_trig = fitted_trig)
head(data$fitted_trig)
term <- as.vector(predict(spline_fit_trig, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]

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

for (val in 1:120)
{
  daily_mean_d_trig[val] = mean(subset(data$abs_d_trig_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_trig[val] <- sd(subset(data$abs_d_trig_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_trig_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_trig[val] = mean(daily_mean_d_trig[(val-3):(val+3)], na.rm = T)
  ma_se_trig[val] <- mean(daily_se_trig[(val-3):(val+3)], na.rm = T)
  ma_d_median_trig[val] <- mean(daily_median_d_trig[(val-3):(val+3)], na.rm = T)
  
}
par(mar=c(5,5,5,5))
plot(ma_d_trig, type="l", ylim = c(0, 1.5), xlim = c(0,120), main = "Triglycerides", 
     ylab="Absolute difference in mmol/l", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_trig + ma_se_trig*1.96, type="l", lty=2)
lines(ma_d_trig - ma_se_trig*1.96, type="l", lty=2)
lines(ma_d_median_trig, type="l", lty=1, col="blue")
abline(v=20, lty=2, col="red")
legend(-2.5, 1.55, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.5)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_trig.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

#### HDL ####
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
hist( data$d_hdl_t4, breaks=100)
summary(data$d_hdl_t4)
plot(data$Mean_hdl_t4, data$d_hdl_t4)
abline(lm(data$d_hdl_t4 ~ data$Mean_hdl_t4), col = "red")
lm <- lm(data$d_hdl_t4 ~ data$Mean_hdl_t4)
summary(lm)

plot(data$Mean_hdl_t4, data$abs_d_hdl_t4)
abline(lm(data$abs_d_hdl_t4 ~ data$Mean_hdl_t4), col = "red")
lm <- lm(data$abs_d_hdl_t4 ~ data$Mean_hdl_t4)
summary(lm)

plot(data$Mean_hdl_t4, log(1+data$abs_d_hdl_t4))
abline(lm(log(data$abs_d_hdl_t4+1) ~ data$Mean_hdl_t4), col = "red")
lm <- lm(log(1+data$abs_d_hdl_t4) ~ data$Mean_hdl_t4)
summary(lm)

plot(data$betweenT4visits, data$d_hdl_t4)
abline(lm(data$d_hdl_t4 ~ data$betweenT4visits), col = "red")
lm <- lm(data$d_hdl_t4 ~ data$betweenT4visits)
summary(lm)

plot(data$betweenT4visits, data$abs_d_hdl_t4)
abline(lm(data$abs_d_hdl_t4 ~ data$betweenT4visits), col = "red")
cor.test(log(data$betweenT4visits), log(1 + data$abs_d_hdl_t4))

plot(log(data$betweenT4visits),  log(1 + data$abs_d_hdl_t4))
abline(lm(log(1 + data$abs_d_hdl_t4) ~ log(data$betweenT4visits)), col = "red")
lm <- lm(log(data$abs_d_hdl_t4+1) ~ log(data$betweenT4visits))
summary(lm)

data <- tu %>% drop_na(abs_d_hdl_t4)
data <- subset(data, betweenT4visits<121)
spline_fit_hdl <- lm(log(abs_d_hdl_t4+0.01) ~ 
                       + sex_t4 + ns(age_t4, 3)+ ns(betweenT4visits, knots = c(20, 30, 50, 70, 100))
                     , data = data)
extractAIC(spline_fit_hdl) # used to choice the best model. Tested 2-5 knots
anova(spline_fit_hdl)
termplot(spline_fit_hdl, term=3)
summary(spline_fit_hdl)
hist(spline_fit_hdl$residuals)
 fitted_hdl <- exp(spline_fit_hdl$fitted.values)
 data <- data %>% ungroup() %>% mutate(fitted_hdl = fitted_hdl)
 head(data$fitted_hdl)
term <- as.vector(predict(spline_fit_hdl, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]

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

for (val in 1:120)
{
  daily_mean_d_hdl[val] = mean(subset(data$abs_d_hdl_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_hdl[val] <- sd(subset(data$abs_d_hdl_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_hdl_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_hdl[val] = mean(daily_mean_d_hdl[(val-3):(val+3)], na.rm = T)
  ma_se_hdl[val] <- mean(daily_se_hdl[(val-3):(val+3)], na.rm = T)
  ma_d_median_hdl[val] <- mean(daily_median_d_hdl[(val-3):(val+3)], na.rm = T)
  
}

par(mar=c(5,5,5,5))
plot(ma_d_hdl, type="l", ylim = c(0, 0.4), xlim = c(0,120), main = "High-density lipoprotein", 
     ylab="Absolute difference in mmol/l", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_hdl + ma_se_hdl*1.96, type="l", lty=2)
lines(ma_d_hdl - ma_se_hdl*1.96, type="l", lty=2)
lines(ma_d_median_hdl, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 0.4, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.5)
box()
par(new=T)
plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-50, 30), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_hdl.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()

#### gamma glutamyltransfrase ####
data <- subset(tu,  betweenT4visits<121 
               #& sex_t4==0
)
plot(data$Mean_ggt_t4, data$d_ggt_t4)
abline(lm(data$d_ggt_t4 ~ data$Mean_ggt_t4), col = "red")
lm <- lm(data$d_ggt_t4 ~ data$Mean_ggt_t4)
summary(lm)

plot(data$Mean_ggt_t4, data$abs_d_ggt_t4)
abline(lm(data$abs_d_ggt_t4 ~ data$Mean_ggt_t4), col = "red")
lm <- lm(data$abs_d_ggt_t4 ~ data$Mean_ggt_t4)
summary(lm)

plot(log(data$Mean_ggt_t4), log(1+data$abs_d_ggt_t4))
abline(lm(log(data$abs_d_ggt_t4+1) ~ log(data$Mean_ggt_t4)), col = "red")
lm <- lm(log(1+data$abs_d_ggt_t4) ~ log(data$Mean_ggt_t4))
summary(lm)

plot(data$betweenT4visits, data$d_ggt_t4)
abline(lm(data$d_ggt_t4 ~ data$betweenT4visits), col = "red")
lm <- lm(data$d_ggt_t4 ~ data$betweenT4visits)
summary(lm)

plot(data$betweenT4visits, data$abs_d_ggt_t4)
abline(lm(data$abs_d_ggt_t4 ~ data$betweenT4visits), col = "red")
cor.test(log(data$betweenT4visits), log(1 + data$abs_d_ggt_t4))

plot(log(data$betweenT4visits),  log(1 + data$abs_d_ggt_t4))
abline(lm(log(1 + data$abs_d_ggt_t4) ~ log(data$betweenT4visits)), col = "red")
lm <- lm(log(data$abs_d_ggt_t4+1) ~ rcs(data$betweenT4visits, 3))
anova(lm)
summary(lm)


data <- tu %>% drop_na(abs_d_ggt_t4)

data <- subset(data, betweenT4visits<121 
)

spline_fit_ggt <- lm(log(abs_d_ggt_t4+0.1) ~ 
                       + sex_t4 + ns(age_t4, 3)  + ns(betweenT4visits, knots = c(10, 20, 40, 60, 80, 100))
                     #+ as.factor(education_t4) + smoking_t4 + as.factor(phys_activity_leisure_hard_t4) 
                     , data = data)
anova(spline_fit_ggt)
termplot(spline_fit_ggt, term = 3)      
summary(spline_fit_ggt)
hist(spline_fit_ggt$residuals)
fitted_ggt <- exp(spline_fit_ggt$fitted.values)
data <- data %>% ungroup() %>% mutate(fitted_ggt = fitted_ggt)
head(data$fitted_ggt)
term <- as.vector(predict(spline_fit_ggt, type = "terms", terms = 3))
head(term)
tail(term)
term <- (exp(term)-1)*100
data <- data %>% ungroup() %>% mutate(term = term)
plot(data$betweenT4visits, data$term)
data <- data[order(data$betweenT4visits),]
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

for (val in 1:120)
{
  daily_mean_d_ggt[val] = mean(subset(data$abs_d_ggt_t4, data$betweenT4visits==val), na.rm = T)
  daily_se_ggt[val] <- sd(subset(data$abs_d_ggt_t4, data$betweenT4visits==val), na.rm=T)/
    sqrt(length(subset(data$abs_d_ggt_t4, data$betweenT4visits==val)))
}
for (val in 5:115)
{
  ma_d_ggt[val] = mean(daily_mean_d_ggt[(val-3):(val+3)], na.rm = T)
  ma_se_ggt[val] <- mean(daily_se_ggt[(val-3):(val+3)], na.rm = T)
  ma_d_median_ggt[val] <- mean(daily_median_d_ggt[(val-3):(val+3)], na.rm = T)
}
par(mar=c(5,5,5,5))
plot_ma_ggt <- plot(ma_d_ggt, type="l", ylim = c(0, 16), xlim = c(0,120), main = "Gamma glutamyltransferase", 
                    ylab="Absolute difference in u/l", xlab="Time between visits in The Tromsø Study 4")
lines(ma_d_ggt + ma_se_ggt*1.96, type="l", lty=2)
lines(ma_d_ggt - ma_se_ggt*1.96, type="l", lty=2)
lines(ma_d_median_ggt, type="l", lty=1, col="blue")
abline(v=60, lty=2, col="red")
legend(-2.5, 16, legend=c("Moving average of mean including confidence interval", "Moving average of median", "Effect of time between visits in percent from a linear model using spline", "Cut-off for dichotomous variable"),
       col=c("black", "blue", "red", "red"), lty=c(1,1,1,2), cex=0.4)
box()
par(new=T)

plot(data$betweenT4visits, data$term, type="l", lty=1, col = "red", axes=FALSE, xlab = "", ylab="", ylim = c(-30, 15), xlim = c(0,120), )
axis(4, las=1)
mtext("Partial effect of time between visits in percent", side=4, line=3, las=0)
dev.copy(tiff, 'plot_ma_ggt.tiff',width = 28, height = 19, units = "cm", res = 250)
dev.off()
