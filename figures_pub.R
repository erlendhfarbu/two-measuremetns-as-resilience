
#### FIGURE 1 ####
recode_d <- function(df, name, variable){
  variable <- enquo(variable)
  name <-  enquo(name)
  df <- df %>%
    mutate(recoded_quintile = case_when(
      ({{variable}}) == 1 ~ 1,
      ({{variable}}) == 2 ~ 2,
      ({{variable}}) == 3 ~ 2,
      ({{variable}}) == 4 ~ 2,
      ({{variable}}) == 5 ~ 3
    )) 
  df <- df %>% 
    rename("recoded_quint_{{name}}":= recoded_quintile)
}
tu <- recode_d(tu, diabp, quart_d_diabp_t4)
head(tu$recoded_quint_diabp, n=20)
head(tu$quart_d_diabp_t4, n=20)
tu <- recode_d(tu, sysbp, quart_d_sysbp_t4)
tu <- recode_d(tu, hr, quart_d_hr_t4)
tu <- recode_d(tu, chol, quart_d_chol_t4)
tu <- recode_d(tu, trig, quart_d_trig_t4)
tu <- recode_d(tu, hdl, quart_d_hdl_t4)
tu <- recode_d(tu, ggt, quart_d_ggt_t4)

#cut_off <- subset(tu, betweenT4visits<=20)
cut_off <- subset(tu, betweenT4visits>20)
#cut_off <- tu
cut_off$follow_up_time_t42 <- cut_off$follow_up_time_t42/365.25


fit_sysbp <- survfit(Surv(follow_up_time_t42, death) 
                     ~ recoded_quint_sysbp, data = cut_off)

KM_sysbp <-  autoplot(fit_sysbp, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "Systolic blood pressure  ") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5), 
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))
KM_sysbp
fit_diabp <- survfit(Surv(follow_up_time_t42, death)
                     ~ recoded_quint_diabp, data = cut_off)
KM_diabp <- autoplot(fit_diabp, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "Diastolic blood pressure") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),  
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

KM_diabp
fit_trig <- survfit(Surv(follow_up_time_t42, death) 
                    ~ recoded_quint_trig, data = cut_off)
KM_trig <- autoplot(fit_trig, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "Triglycerides ") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        legend.position = "none",
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5), 
        plot.margin=unit(c(0,0,0,0), "cm"))
KM_trig

#cut_off <- subset(tu, betweenT4visits<=60)
cut_off <- subset(tu, betweenT4visits>60)
#cut_off <- tu
cut_off$follow_up_time_t42 <- cut_off$follow_up_time_t42/365.25

fit_hr <- survfit(Surv(follow_up_time_t42, death)
                  ~ recoded_quint_hr, data = cut_off)
KM_hr <- autoplot(fit_hr, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "Heart rate") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),  
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

fit_chol <- survfit(Surv(follow_up_time_t42, death) 
                    ~ recoded_quint_chol, data = cut_off)
KM_chol <- autoplot(fit_chol, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "Cholesterol") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        legend.position = "none",
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"))
KM_chol
fit_hdl <- survfit(Surv(follow_up_time_t42, death)
                   ~ recoded_quint_hdl, data = cut_off)
KM_hdl <- autoplot(fit_hdl, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "HDL") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        legend.position = "none",
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),  
        plot.margin=unit(c(0,0,0,0), "cm"))
KM_hdl

fit_ggt <- survfit(Surv(follow_up_time_t42, death)
                   ~ recoded_quint_ggt, data = cut_off)
KM_ggt <- autoplot(fit_ggt, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "", y = "GGT") +
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 8),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 8, vjust = -3),
        legend.position = "none",
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5), 
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"))
KM_ggt
#data to plot legend
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 3
cols = gg_color_hue(n)
KM_legend <- autoplot(fit_ggt) + 
  labs(x = "\n Survival Time (Years) ", y = "",
       title = "Survival times of quintiles of difference in gamma glutamyltransferase \n") +
  scale_x_continuous(limits=c(60, 90)) 
KM_legend <- KM_legend  +
  guides(fill="none") +
  #labs(colour = "groups") +
  scale_colour_manual(name = " Quintiles of difference", labels = c("Largest negative difference", "2, 3 or 4", "Largest positive difference"), values = cols, breaks = waiver()) +
  #scale_fill_manual(name = " Quintiles of difference", labels = c("Largest negative difference", "2, 3 or 4", "Largest positive difference"), values = cols, breaks = waiver()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", size = 8))
KM_legend

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

shared_legend <- extract_legend(KM_legend)

figure1 <- grid.arrange( KM_sysbp, KM_diabp, KM_hr, KM_chol, KM_trig,  KM_hdl, KM_ggt, shared_legend, 
                         ncol = 4, nrow = 2, left = "Survival proabilities", bottom = "Follow up time (years)") 
figure1
#### FIGURE 2 #####
source("~/postdocHC/ggt_dime_dep_coef.R")
source("~/postdocHC/trig_time_dep_coef.R")
source("~/postdocHC/diabp_time_dep_coef.R")
source("~/postdocHC/sysbp_time_dep_coef.R")
source("~/postdocHC/chol_time_dep_coef.R")
source("~/postdocHC/hdl_time_dep_coef.R")
source("~/postdocHC/hr_time_dep_coef.R")
#data to plot legend
for_legend <- data.frame(cutoff = factor(c("Within shorter","Within longer", "Within shorter","Within longer" )),
                         blabla = c(3,5,4,7))
# Create plot with legend
my_col <- c("Blue", "Red")
ggp1_legend <- ggplot(for_legend, aes(x = cutoff, y = blabla, group = cutoff, color=cutoff)) +
  geom_line() +   scale_colour_manual(values = my_col, name="Time interval between measurements",
                                      breaks=c("Within shorter", "Within longer"),
                                      labels=c("Within shorter", "Within longer"))
ggp1_legend 

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

shared_legend_fig2 <- extract_legend(ggp1_legend)

figure2 <- grid.arrange( HR_sysbp, HR_diabp, HR_hr, HR_chol, HR_trig,  HR_hdl, HR_ggt, shared_legend_fig2, 
                         ncol = 4, nrow = 2) 
figure2

#### Figure 3 ####
source("~/postdocHC/sum_whole_sample.R")

sum <- subset(tu, betweenT4visits<121)
sum <- sum %>% drop_na(sum_abs_d, quint_sum_abs_d, mean_diabp_t4, death, follow_up_time_t42, age_t4,  sex_t4, 
                       ggt_t4, ggt_t42,  mean_sysbp_t4, triglycerides_t4, cholesterol_t4, hr_t41
                       ,hdl_t4, childhood_economy_t4, education_t4, health_t4, cigarettes_t4, bmi_t4, smoking_t4,
                       phys_activity_leisure_hard_t4, number_diseases , number_med,betweenT4visits, 
                       angina_t4, heart_attack_t4, asthma_t4, diabetes_t4, dic_betweenvisits, dic_betweenvisits_20, time_quart, time_quart2)

sumfigure3 <- subset(sum, dic_betweenvisits==1)
#sumfigure3 <- subset(sum, time_quart2==3) #Possible to change to categories based on biomarker-specific cut-offs

sumfigure3$follow_up_time_t42 <- sumfigure3$follow_up_time_t42/365.25

fit_sum_d <- survfit(Surv(follow_up_time_t42, death) ~ quint_sum_abs_d, data = sumfigure3)
KM_sum_long <- autoplot(fit_sum_d, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "Survival time (years)", y = "", 
       title = "≥41 days between visits") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(colour="black", size = 8),
        axis.title.y = element_text(colour="#FF7A33", size = 8, vjust = -3),
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),  
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

sumfigure3 <- subset(sum, dic_betweenvisits==0)
sumfigure3$follow_up_time_t42 <- sumfigure3$follow_up_time_t42/365.25
fit_sum_d <- survfit(Surv(follow_up_time_t42, death) ~ quint_sum_abs_d, data = sumfigure3)
KM_sum_short <- autoplot(fit_sum_d, censor.size = 0, conf.int = F, surv.size=0.8) +
  labs(x = "Survival time (years)", y = "", 
       title = "<41 days between visits") + 
  scale_x_continuous(limits=c(0, 27)) +
  scale_y_continuous(limits=c(0.35, 1)) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(colour="black", size = 8),
        axis.title.y = element_text(colour="#FF7A33", size = 8, vjust = -3),
        axis.text.x=element_text(size = 6),
        axis.ticks.x=element_line(size = 0.5), 
        axis.text.y=element_text(size = 6, hjust = 1),
        axis.ticks.y=element_line(size = 0.5),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),  
        legend.position = "none",
        plot.margin=unit(c(0,0,0,0), "cm"))

#function to create ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 5
cols = gg_color_hue(n)
KM_legend_3 <- autoplot(fit_sum_d) + 
  labs(x = "\n Survival Time (Years) ", y = "",
       title = "Survival times of quintiles of difference in gamma glutamyltransferase \n") +
  scale_x_continuous(limits=c(60, 90)) 
KM_legend_3 <- KM_legend_3  +
  guides(fill="none") +
  #labs(colour = "groups") +
  scale_colour_manual(name = " Quintiles of difference", labels = c("Smallest sum of difference", "2", "3", "4", "Largest sum of difference"), values = cols, breaks = waiver()) +
  #scale_fill_manual(name = " Quintiles of difference", labels = c("Smallest sum of difference", "2", "3", "4", "Largest sum of difference"), values = cols, breaks = waiver()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(colour="black", size = 12),
        axis.title.y = element_text(colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", size = 8))
KM_legend_3
# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

shared_legend_fig3 <- extract_legend(KM_legend_3)
lay <- rbind(c(1,1,2,2,3),
             c(1,1,2,2,3))
grid.arrange(KM_sum_short, KM_sum_long, shared_legend_fig3, nrow=1
             , left = "Survival proabilities", layout_matrix=lay)

#### FIGURE 4 ####

figure_interaction_time <- grid.arrange( 
  HR_sum_crude, HR_sum_behav_bmi, HR_sum_full,shared_legend,
  ncol = 4, nrow = 1, left = "Hazard ratio", bottom = "") 
figure_interaction_time
