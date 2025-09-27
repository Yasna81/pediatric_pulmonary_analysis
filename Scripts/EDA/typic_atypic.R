#this scripts typic vs atypic  
#---------------------------------------------------------------------------

atypic_cols <- c("GGO_CT", "Consolidation_CT","Pleuraleffusion_CT")
typic_cols <- c("Nodule_CT","Halo_CT","Reversehalo_CT","Cavity_CT")
df_3$Atypic_positive_1 <- apply(df_3[,atypic_cols],1,function(x) ifelse(any(x==1),1,0))
df_3$typic_positive_1 <- apply(df_3[,typic_cols],1,function(x) ifelse(any(x==1),1,0))

library(dplyr)
CT_table_at_1 <- df_3 %>%
    group_by(Atypic_positive_1) %>%
    summarise(
        "number of patients"  = n(),
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


wilcox.test(WBC_1 ~ Atypic_positive_1, data = df_3) #0.01
wilcox.test(ESR ~ Atypic_positive_1, data = df_3) #0.4
wilcox.test(CRP ~ Atypic_positive_1, data = df_3)#0.6
wilcox.test(GM_peak ~ Atypic_positive_1, data= df_3)#0.8


#typic tabel
library(dplyr)
CT_table_at_2 <- df_3 %>%
    group_by(typic_positive_1) %>%
    summarise(
        "number of patients"  = n(),
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
