#this script helped to find GM_peaks
#--------------------------------------------------------------
nrow(df_1)
colnames(df_1)
library(dplyr)
df_main <- df_1 %>% select(-AF_dalay,-time,-ot_n,-class_binary)
save(df_main,file = "~/project/final_GM/df_clean.RData")
length(intersect(df_clean$Admission_date, df$Admission_date))
length(intersect(df_clean$Length_of_stay, df$Length_of_stay))      
length(intersect(df_clean$age_adjusted, df$Age))
library(dplyr)
df_3 <- df %>% select(Admission_date,Age,matches("^GM_value_([1-9]|1[0-9]|2[0-5])$"))
gm_cols <- paste0("GM_value_",1:25)
df_3$GM_peak <- apply(df_3[,gm_cols],1,max,na.rm = TRUE)
gm_peak_only <- df_3 %>%
    select(Admission_date,Age,GM_peak)
df_merged <- left_join(df_clean,gm_peak_only, by = "Admission_date")
gm_peak_unique <- gm_peak_only %>%
    group_by(Admission_date) %>%
    summarise(GM_peak = max(GM_peak, na.rm = TRUE))
df_merged <- left_join(df_clean,gm_peak_unique, by = "Admission_date")