#this script is an EDA to clinical values stratified by : EORTC > FN > Nodule and stat
#----------------------------------------------------------------------------------------------------




library(dplyr)
summary_EORTC <- df_3 %>%
    group_by(EORTC_MSG_label) %>%
    summarise("number of patients"  = n() ,
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

#stat test :
kruskal_wbc <-kruskal.test(WBC_1 ~ EORTC_MSG_label, data = df_3) #p-value = 0.7907
kruskal_CRP <-kruskal.test(CRP ~ EORTC_MSG_label, data = df_3) #p-value = 0.9126
kruskal_ESR <-kruskal.test(ESR ~ EORTC_MSG_label, data = df_3) #p-value = 0.564
kruskal_GM <-kruskal.test(GM_peak ~ EORTC_MSG_label, data = df_3)#p-value = 0.5127
#
library(haven)
df_3$FN_n <- as_factor(df_3$FN)
df_3$FN_n <- as.numeric(df_3$FN_n)
library(dplyr)
summary_table_FN <- df_3 %>%
    group_by(FN_n) %>%
    summarise(
        "number of patients"  = n() ,
        "Median FN episodes" = median(FN_episodes),
        "IQR 1 FN episodes" = quantile(FN_episodes,probs = 0.25 ,na.rm = TRUE),
        "IQR 2 FN episodes" = quantile(FN_episodes,probs = 0.75 ,na.rm = TRUE),
        "Min FN episodes" = min(as.numeric(FN_episodes)),
        "Max FN episodes" = max(as.numeric(FN_episodes)),
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


kruskal.test(WBC_1 ~ FN,data= df_3) #p-value = 1.088e-05
kruskal.test(CRP ~ FN,data= df_3) #P_value = 0.03672
kruskal.test(ESR ~ FN,data= df_3) #p-value = 0.02741
kruskal.test(GM_peak ~ FN , data= df_3) #p-value 0.9364
       
library(dplyr)
Nodule_table <- df_3 %>%
    group_by(Nodule_CT) %>%
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



wilcox.test(WBC_1 ~ Nodule_CT, data = df_3) #W = 1985, p-value = 0.09037
wilcox.test(ESR ~ Nodule_CT, data = df_3) #W = 1924.5, p-value = 0.1054
wilcox.test(CRP ~ Nodule_CT, data = df_3)#W = 1465.5, p-value = 0.1676
wilcox.test(GM_peak ~ Nodule_CT, data= df_3)#0.6191
