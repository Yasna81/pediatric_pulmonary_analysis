#this script is survaial > survival model > survival in different groups > cox model for 1- clinical values and nodules >
# > hazard ratio for clincal values
#-----------------------------------------------------------------------------------------------------------------------------------


library(haven)
df_3$Outcome_nu <- as_factor(df_3$Outcome)
df_3$Outcome_nu <- as.numeric(df_3$Outcome_nu)
table(df_1$Outcome_nu , useNA = "ifany")
df_3$Outcome_date[df_1$Outcome == 1 ] # we have 2 patients without outcome date 
table(df_3$Admission_date, useNA =  "ifany")
df_3$Outcome_nu <- ifelse(df_3$Outcome_nu == 2,1,0)
#survival
df_3$last_follow <- as.Date("2002-10-29")
df_3$surve_time <- as.numeric(
    ifelse(df_3$Outcome_nu == 1 ,
           df_3$Outcome_date - df_3$Admission_date,
           df_3$last_follow - df_3$Admission_date)
                          )
library(survival)
surv_obj <- Surv(time = df_3$surve_time, event = df_3$Outcome_nu)
km_fit <- survfit((surv_obj) ~ 1  , data = df_3 )
summary(km_fit)
sum(km_fit$n.risk)
sum(km_fit$n.event) #32
sum(km_fit$n.censor)#81
plot(km_fit)
summary(km_fit, times = c(7, 14, 28,60,90))
#table
library(survminer)
ggsurvplot(km_fit,
           data=df_3,
           risk.table = TRUE,
           conf.int = TRUE,
           xlab = "Days since admission",
           ylab = "Survival probability",
           palette = "Dark2")
#different groups :
survdiff((surv_obj) ~ Nodule_CT, data = df_3) #Chisq= 0.2  on 1 degrees of freedom, p= 0.6 
survdiff((surv_obj) ~ EORTC_MSG_label, data = df_3) #Chisq= 0.8  on 1 degrees of freedom, p= 0.4
survdiff((surv_obj) ~ Relapse , data = df_3)#Chisq= 0.2  on 1 degrees of freedom, p= 0.7
survdiff((surv_obj) ~ Atypic_positive_1 , data = df_3)#, 0.2 ,p value is 0.6
#variables 

cox_model_2 <- coxph((surv_obj) ~ CRP + ESR + WBC_1 + GM_peak + age_adjusted + AF_delay , data = df_3 )
summary(cox_model_2)

cox_model_6 <- coxph((surv_obj) ~  Number_nodule+Size_nodule , data = df_8 )
summary(cox_model_6)

cox_model_ct <- coxph((surv_obj) ~ Nodule_CT + Halo_CT + GGO_CT + Cavity_CT , data = df_3)
summary(cox_model_ct)
library(car)
vif(cox_model_ct) #no multicollinearity 


df_8 <- df_1[df_1$EORTC_MSG_label != "Possible",]

#  Load required packages
library(survival)
library(rms)
library(ggplot2)


#  Set up for rms package
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(GM_peak, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, GM_peak, fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = GM_peak, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs  maximum GM (Galactomannan)",
        x = "GM Value (ODI)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)

#ESR
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(ESR, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, ESR, fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = ESR, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs  ESR (Erythrocyte Sedimentation Rate) ",
        x = "ESR Value (mm/hr)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)

#crp
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(CRP, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, CRP, fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = CRP, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs  CRP (C-Reactive Protien)",
        x = "CRP Value (mg/L)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)

#age 
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(age_adjusted, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, age_adjusted , fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = GM_peak, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs Age",
        x = "Age (years)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)
#AF_delay
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(AF_delay, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, AF_delay, fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = AF_delay, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs Antifungal delay",
        x = "AF delay (days)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)
#wbc
dd <- datadist(df_3)
options(datadist = "dd")

#  Fit Cox model with restricted cubic spline on GM
# You can add other covariates if needed (e.g., + age + CRP)
fit <- cph(surv_obj ~ rcs(WBC_1, 4), data = df_3, x = TRUE, y = TRUE, surv = TRUE)

# Predict hazard ratios across GM range
pred <- Predict(fit, WBC_1, fun = exp)

#  Plot with ggplot2
ggplot(pred, aes(x = WBC_1, y = yhat)) +
    geom_line(color = "#2C3E50", linewidth = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498DB") +
    labs(
        title = "Hazard Ratio vs  WBC (White Blood Cell Count)",
        x = "WBC Value (10^9/L)",
        y = "Hazard Ratio"
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 14)



