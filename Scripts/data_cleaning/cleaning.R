#this is the initial script :  552 > checking missingnes > imputation > sensitivity analysis > handling patients with missing values (cleaning)
#> 119
#------------------------------------------------------------------------------------------------------------------------------
library(haven)
df_6 <- read_sav("~/project/GM/data/Dr. Amanati_GM_23.02.1403.sav")

# checking the missingness in cohort :
library(mice)
vars_to_check <- df[,c("Halo_CT","WBC_1","ESR","CRP","GM_value_1","AF_date_first")]
md.pattern(vars_to_check)
#install.packages("MissMech")
library(MissMech)
TestMCARNormality(df[,c("WBC_1","ESR","CRP","GM_value_1","Length_of_stay")])
#P-value for the non-parametric test of homoscedasticity:  0.4274495 Missing ness at random 
df_6$Halo_CT_binary <- ifelse(is.na(df_6$Halo_CT),1,0)
df_7 <- subset(df_6,Halo_CT_binary %in% c("1","0"))

wilcox.test(GM_value_1 ~ Halo_CT_binary , df_7)


#sensitivity analysis (we compare clinical values between included and excluded ones)
#imputation
#included and excluded onse :
df_6$Halo_CT_coho <-ifelse(is.na(df_6$Halo_CT),0,1)
library(mice)
df_imput <- df_6[,c("WBC_1","ESR","CRP","GM_value_1","Length_of_stay")]
imp <- mice(df_imput,m=5,seed = 123)
df_comp <- complete(imp,1)
df_6$WBC_1 <- df_comp$WBC_1
df_6$ESR <- df_comp$ESR
df_6$GM_value_1 <- df_comp$GM_value_1
df_6$Length_of_stay <- df_comp$Length_of_stay
df_6$CRP <- df_comp$CRP
#sensivity analysis 
qqnorm(df_6$WBC_1)
qqnorm(df_6$GM_value_1)
qqnorm(df_6$ESR)
qqnorm(df_6$Length_of_stay)
qqnorm(df_6$CRP)
#no one is normal ;)
wilcox.test(WBC_1 ~ Halo_CT_coho, data=df_6) #0.3266
wilcox.test(CRP ~ Halo_CT_coho, data = df_6) #4.995e-07
wilcox.test(ESR ~ Halo_CT_coho, data = df_6) #1.239e-05
wilcox.test(GM_value_1 ~ Halo_CT_coho , data = df_6) #0.0001071
wilcox.test(Length_of_stay ~ Halo_CT_coho, data = df_6) #p-value = 2.444e-11
#table of sensivity analysis 
library(dplyr)
summary_table <- df%>%
    group_by(Halo_CT_coho) %>%
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
        "median GM"= median(GM_value_1,na.rm = TRUE),
        "iqr 1" = quantile(GM_value_1,probs = 0.25,na.rm= TRUE),
        "iqr 2" = quantile(GM_value_1,probs = 0.75 ,na.rm = TRUE),
        "leng median" = median(Length_of_stay,na.rm = TRUE),
        "leng iqr1"= quantile(Length_of_stay,probs = 0.25,na.rm = TRUE),
        "leng iqr2" = quantile(Length_of_stay,probs = 0.75 ,na.rm = TRUE),
    )
#standerd mean difference
install.packages("tableone")
library(tableone) 
vars <- c("WBC_1","ESR","CRP","GM_value_1")
tab <- CreateTableOne(vars= vars ,strata = "Halo_CT_coho",data = df_6 , test= FALSE)
print(tab,smd = TRUE)
#absolute difference :
install.packages("cobalt")
library(cobalt)
bal.tab(Halo_CT_coho ~ WBC_1 + ESR + CRP + GM_value_1, data = df_6 ,un = TRUE, disp.means =TRUE)
#cleaning 
#one is adult
df <- df[is.na(df$Age) | df$Age != 19 ,]
#EORTC :
library(haven)
print(attributes(df$EORCT_MSG_criteria)$labels)
df$EORCT_MSG_criteria[df$EORCT_MSG_criteria == 4] <- NA
df <- df %>% 
    mutate(EORTC_MSG_label = case_when(
        EORCT_MSG_criteria == 1 ~ "Proven",
        EORCT_MSG_criteria == 2 ~ "Probable",
        EORCT_MSG_criteria == 3 ~ "Possible",
        EORCT_MSG_criteria == 5 ~ "False positive",
        TRUE ~ NA_character_
    ))
#2 patients have NA value for EORCT and one is false positive we exclude them.
df <- df[!is.na(df$EORTC_MSG_label),]
df <- df[df$EORTC_MSG_label!="False positive",]
nrow(df)


#fn_episodes
df$FN_episodes[is.na(df$FN_episodes) ] <- 0

#cancer diagnose
library(dplyr)
library(stringr)

df$Diagnosis_lable <- as_factor(df$Diagnosis)
#df <- df %>%
    #mutate(Diagnosis_clean = str_trim(Diagnosis),            # trim spaces
           #Diagnosis_clean = str_replace_all(Diagnosis_clean, "\"", ""))   # unify case to uppercase



leukemias <- c("ALL", "AML", "T-cell ALL", "APL",  "Mix ALL+AML", "Mixed or undifferentiated leukemia","Mix ALL+AML")

lymphomas <- c("Non-Hodgkin Lymphoma", "Burkitt Lymphoma", "Hodgkin Lymphoma", "DLBCL", "LPD")

solid_tumors <- c(
    "Neuroblastoma", "Germ Cell Tumor", "Yolk Sak Tumor", "PNET", 
    "Hepatoblastomma", "Ependimoma", "Retinoblastoma", "Schwannoma", 
    "Nephroblastoma", "Wilmes Tumor", "Osteosarcoma", "Ewing Sarcoma", 
    "Rhabdomyosarcoma", "Soft tissue sarcoma", "Astrocytoma", 
    "Brain Tumor", "glioma"
)

non_malignant <- c(
    "Aplastic Anemia", "MDS", "Anemia", "Histiocytosis", "HLH",
    "CGD", "IgE syndrome", "PID", "LAD", 
    "Sickle cell Anemia", "Malignancy_rule outed"
)

df<- df %>%
    mutate(Cancer_Group = case_when(
        Diagnosis_lable %in% leukemias ~ "Leukemia",
        Diagnosis_lable %in% lymphomas ~ "Lymphoma",
        Diagnosis_lable %in% solid_tumors ~ "Solid Tumor",
        Diagnosis_lable %in% non_malignant ~ "Non-Malignant",
        TRUE ~ "Other"
    ))

#subsetting 
df<- df[!is.na(df$Halo_CT),]
nrow(df) #119 patients left

df_2 <- df %>% select(-Name,-name2,-Avg_WBC,-WBC_2,-WBC_2_group)

#not nescessry with missing data
df_2 <- df_2 %>% select(-Combination_AF_therapy,-Fungus_species,-Fungal_infection_site,-Transplant,-Caspofungin_start_date)
df_2 <- df_2 %>% select(-Mix_location,-Otherfinding_CT)

#
gm_cols <- paste0("GM_value_",1:25)
df_2$GM_peak <- apply(df_2[,gm_cols],1,max,na.rm = TRUE)
df_2 <- df_2 %>%
    select(-matches("^GM") | GM_peak) 

#CT cleaning 
#those who did not have nodules are marked as NA we change the nodule size and number of these cases from NA to 0.
df_2$Number_nodule[is.na(df_2$Nodule_CT) | df_2$Nodule_CT == 0 ] <- 0
df_2$Size_nodule[is.na(df_2$Nodule_CT) | df_2$Nodule_CT == 0 ] <- 0
# some patients have positive ct "yes" and a nodule size but the number of noudle is missing we assume that they have at least 1 nodule:
df_2$Number_nodule[is.na(df_2$Number_nodule) & df_2$Nodule_CT == 1 ] <- 1
#only one patient has a NA size of tumor. we leave it as it is .
#some patients with no nodules have ct location which should turn to NA.
df_2$Location_CT[df_2$Nodule_CT == 0] <- NA

#age for those under 1 year old 
df_2 <- df_2 %>% mutate(age_adjusted = if_else(is.na(Age),0.5,Age))
# fouces is on proven and probable 
df_3 <- df_2 %>% filter(EORTC_MSG_label != "Possible")

