# pediatric_pulmonary_analysis

### This repo contains scripts and plots for this study 


### 📁 Directory Details

#### 💻 `/Scripts` - R scripts for the final analysis

      
- 🪥 **`/data_cleaning`** - R scripts for data wrangling
    - **`/cleaning.R`** - Handling missing values, sensitivity analysis, labeling, etc.
    - **`/data_revising.R`** - Creating GM-peak object


- 📊 **`/EDA`** - R scripts for exploratory data analysis of the cohort
    - **`/EDA.R`** - EDA of clinical values in the cohort
    - **`/eda_additional.R`** - EDA for general characteristics of the cohort (age, sex, etc.)
    - **`/main_co_occurrence.R`** - Correlation and co-occurrence of various CT findings
    - **`/add_script.R`** - EDA of infection sites and related factors
    - **`/typic_atypic.R`** - EDA of clinical variables in patients with typical and atypical CT findings
 
  
- 🤖 **`/models`** - R scripts for machine learning models
    - **`/Model_last.R`** - A logistf model trained on patients with/without nodules using clinical values as predictors (main analysis)
    - **`/main_survival.R`** - Cox model for survival analysis (main analysis)
    - **`/halo_mdl.R`** - A logistf model trained on patients with/without halo signs using clinical values as predictors (best practices)
    - **`/multi_model.R`** - A logistf model trained on patients with/without nodules, halo signs, and GGO using clinical values as predictors (best practices)
#### 🖼️`/plots_300` - Plots and tables


#### 📦 packages used :
```R
packages <- c("survival","car","ggplot2", "rms","mice", "MissMech","pheatmap","pROC","caret", "logistf","ResourceSelection","survminer")
```
