# this script has added characteristics of the cohort 
#---------------------------------------------------
table(df_3$sex_n)
library(haven)
df_3$sex_n <- as_factor(df_3$Sex)
df_3$sex_n <- as.character(df_3$sex_n)
median(df_3$age_adjusted)
sd(df_3$age_adjusted)
table(df_3$Outcome_nu)
df_3%>%
    filter(Outcome_nu == 0) %>%
    summarize (mean_of = mean(Length_of_stay,na.rm = TRUE))

df_3%>%
    filter(Outcome_nu == 1) %>%
    summarize (mean_of = mean(Length_of_stay,na.rm = TRUE),
               sd_of = sd(Length_of_stay, na.rm = TRUE))
table(df_3$Cancer_Group)
