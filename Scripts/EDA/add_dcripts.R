# this script has added EDA to infection sites , location and transplant and nodule charactristics (552) also a table for clinical vs HAlo (115)
#----------------------------------------------------------------------------------------------------------------


#fungus info 

library(haven)
df_6$Fungal_infection_site_1 <- as_factor(df_6$Fungal_infection_site)
df_6$Fungus_species_1 <- as_factor(df_6$Fungus_species)
table(df_6$Fungal_infection_site_1)
table(df_6$Fungus_species_1)
df_6$Transplant_1 <- as_factor(df_6$Transplant)
table(df_6$Transplant_1)
df_3$Location_CT_1 <- as_factor(df_3$Location_CT)
table(df_3$Location_CT_1)
table(df_3$Halo_CT_1) #37 yes -78 no
#halo
df_3$Halo_CT_1 <- as_factor(df_3$Halo_CT)
df_3$Halo_CT_1 <- as.numeric(df_3$Halo_CT_1)
df_3$Halo_CT_1 <- ifelse(df_3$Halo_CT_1 == 1 , 0, 1)




Halo_table <- df_3 %>%
    group_by(Halo_CT) %>%
    summarise(
        "number of patients"  = n() ,
        "median WBC" = median(WBC_1,na.rm = TRUE),
        "Q1 WBC" = quantile(WBC_1,probs = 0.25 ,na.rm = TRUE),
        "Q2 WBC" = quantile(WBC_1,probs = 0.75 ,na.rm = TRUE),
        "median ESR" = median(ESR,na.rm = TRUE),
        "Q1 ESR"= quantile(ESR,probs = 0.25 ,na.rm = TRUE),
        "Q2 ESR" = quantile(ESR,probs = 0.75 ,na.rm = TRUE),
        "median CRP" = median(CRP,na.rm = TRUE),
        "iqr 1-crp" = quantile(CRP,probs = 0.25 ,na.rm = TRUE),
        "iqr 2-crp" = quantile(CRP,probs = 0.75 ,na.rm = TRUE),
        "median GM"= median(GM_peak,na.rm = TRUE),
        "iqr 1" = quantile(GM_peak,probs = 0.25,na.rm= TRUE),
        "iqr 2" = quantile(GM_peak,probs = 0.75 ,na.rm = TRUE))



wilcox.test(WBC_1 ~ Halo_CT_1, data = df_3) #0.9
wilcox.test(ESR ~ Halo_CT_1, data = df_3) #0.5
wilcox.test(CRP ~ Halo_CT_1, data = df_3)#0.9
wilcox.test(GM_peak ~ Halo_CT_1, data= df_3)#0.2





#nodule info 
library(dplyr)
table_nodule <- df_6 %>%
    group_by(EORCT_MSG_criteria_1) %>%
    summarise("number of patients"  = n(),
              "number of nude" = median(Number_nodule_1 , na.rm = TRUE),
              "iqr1 number of nodule" = quantile(Number_nodule_1,probs = 0.25 , na.rm = TRUE),
              "iqr number of nodule" = quantile(Number_nodule_1,probs = 0.75 , na.rm = TRUE),
              "median size" = median(Size_nodule_1,na.rm = TRUE),
              "iqr 1-crp" = quantile(Size_nodule_1,probs = 0.25 ,na.rm = TRUE),
              "iqr 2-crp" = quantile(Size_nodule_1,probs = 0.75 ,na.rm = TRUE),
)
kruskal.test(EORCT_MSG_criteria_1 ~ Size_nodule_1, data = df_6) #0.04
kruskal.test(EORCT_MSG_criteria_1 ~ Number_nodule, data = df_6) #0.4


library(haven)
df_6$Size_nodule_1 <- as_factor(df_6$Size_nodule)
df_6$Size_nodule_1 <- as.numeric(df_6$Size_nodule_1)

df_6$Number_nodule_1 <- as_factor(df_6$Number_nodule)
df_6$Number_nodule_1 <- as.numeric(df_6$Number_nodule_1)

df_6$EORCT_MSG_criteria_1 <- as_factor(df_6$EORCT_MSG_criteria)
