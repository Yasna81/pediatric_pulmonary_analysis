# this script is for a model on halo, nodule and ggo , the results were the same as nodule.

df_multi <- df_3
df_multi$CT <- ifelse(df_multi$Nodule_CT == 1 & df_multi$Halo_CT == 1 & df_multi$GGO_CT == 1,1,0)
ct_vars <- c ("Nodule_CT","Halo_CT","GGO_CT")
df_multi$CT_max <- ifelse(rowSums(df_multi[ct_vars]) >= 2,1,0)
# we have to manage imbalene its 35 80 
library(caret)
balanced_data <- upSample(
    x = df_multi[,-which(names(df_multi)=="CT_max")],
    y = factor(df_multi$CT_max),
    yname =  "CT_max"
)
 table(balanced_data$CT_max)
##model :
#cross-validation 
# Load packages
library(logistf)
library(pROC)
library(caret)

# Set seed for reproducibility
set.seed(123)
#we keep the ctrl to avoid data leakage.
ctrl <- trainControl(method = "cv" , number = 5 ,sampling = "up",classProbs = TRUE ,summaryFunction = twoClassSummary)
# 5-fold stratified cross-validation
folds <- createFolds(balanced_data$CT_max, k = 5, list = TRUE)

# Initialize metric storage
auc_list <- c()
acc_list <- c()
f1_list <- c()
sens_list <- c()
spec_list <- c()

# Start cross-validation
for (i in 1:5) {
    test_idx <- folds[[i]]
    test_data <- balanced_data[test_idx, ]
    train_data <- balanced_data[-test_idx, ]
    
    # Fit Firth logistic regression
    model <- logistf(CT_max ~ WBC_1 + CRP + ESR  + age_adjusted, trControl = ctrl   ,  data = train_data)
    
    # Predict probabilities
    probs <- predict(model, newdata = test_data, type = "response")
    
    # Predicted classes using 0.5 cutoff
    preds <- ifelse(probs > 0.5, 1, 0)
    
    # Actual values
    actuals <- test_data$CT_max
    
    # Compute AUC
    roc_obj <- roc(actuals, probs)
    auc_val <- auc(roc_obj)
    auc_list[i] <- auc_val
    
    # Confusion matrix
    conf_mat <- confusionMatrix(as.factor(preds), as.factor(actuals), positive = "1")
    
    acc_list[i] <- conf_mat$overall["Accuracy"]
    sens <- conf_mat$byClass["Sensitivity"]
    spec <- conf_mat$byClass["Specificity"]
    precision <- conf_mat$byClass["Precision"]
    recall <- sens
    
    # Compute F1 Score
    f1 <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
    
    f1_list[i] <- f1
    sens_list[i] <- sens
    spec_list[i] <- spec
}

# Final results
cat("======== Cross-Validation Results ========\n")
cat("Average AUC: ", round(mean(auc_list, na.rm = TRUE), 3), "\n")
cat("Average Accuracy: ", round(mean(acc_list, na.rm = TRUE), 3), "\n")
cat("Average F1 Score: ", round(mean(f1_list, na.rm = TRUE), 3), "\n")
cat("Average Sensitivity:", round(mean(sens_list, na.rm = TRUE), 3), "\n")
cat("Average Specificity:", round(mean(spec_list, na.rm = TRUE), 3), "\n")
cat("==========================================\n")
#> # Final results
#> cat("======== Cross-Validation Results ========\n")
#======== Cross-Validation Results ========
#    > cat("Average AUC: ", round(mean(auc_list, na.rm = TRUE), 3), "\n")
#Average AUC:  0.662 
#> cat("Average Accuracy: ", round(mean(acc_list, na.rm = TRUE), 3), "\n")
#Average Accuracy:  0.6 
#> cat("Average F1 Score: ", round(mean(f1_list, na.rm = TRUE), 3), "\n")
#Average F1 Score:  0.586 
#> cat("Average Sensitivity:", round(mean(sens_list, na.rm = TRUE), 3), "\n")
#Average Sensitivity: 0.562 
#> cat("Average Specificity:", round(mean(spec_list, na.rm = TRUE), 3), "\n")
#Average Specificity: 0.637 
#> cat("==========================================\n")
#==========================================






#model :
library(logistf)
#df_3$Proven <- ifelse(df_3$EORTC_MSG_label == "Proven",1,0)
model_multi_ct <- logistf(CT_max ~ WBC_1+CRP+ESR+age_adjusted, data=balanced_data)


summary(model_multi_ct)
exp(cbind(OR=coef(model_multi_ct),confint(model_multi_ct)))
library(pROC)
pred_probs <- predict(model_multi_ct, type = "response")
roc_obj <- roc(balanced_data$CT_max,pred_probs)
plot(roc_obj)
auc(roc_obj) #Area under the curve: 0.6705

library(caret)
#predicted class
predicted_class <- ifelse(pred_probs > 0.5 ,1,0 )
actual <- balanced_data$CT_max
conf_matrix <- confusionMatrix(as.factor(predicted_class),as.factor(actual),positive = "1")
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"] # Recall = Sensitivity
precision <- conf_matrix$byClass["Pos Pred Value"] # Precision
specificity <- conf_matrix$byClass["Specificity"]


F1 <- 2 * (precision * recall) / (precision + recall)


metrics <- data.frame(
    Accuracy = round(accuracy, 3),
    Recall = round(recall, 3),
    Precision = round(precision, 3),
    Specificity = round(specificity, 3),
    F1_Score = round(F1, 3)
)

print(metrics)
#
#print(metrics)
#> print(metrics)
#Accuracy Recall Precision Specificity F1_Score
#    0.613  0.575     0.622        0.65    0.597

library(ResourceSelection)
hoslem.test(as.numeric(actual), y= pred_probs, g= 10)
library(rms)
cal <- val.prob(pred_probs, as.numeric(balanced_data$CT_max))
#<0.05
library(dplyr)
summary_model <- summary(model_multi_ct)
####
# Create a data frame manually
coef_table <- data.frame(
    term = names(model_multi_ct$coefficients),
    estimate = model_multi_ct$coefficients,
    lower = model_multi_ct$ci.lower,
    upper = model_multi_ct$ci.upper,
    p_value = model_multi_ct$prob
)

# Remove the intercept if you don't want to plot it
coef_table <- coef_table[coef_table$term != "(Intercept)", ]

# Compute Odds Ratios
coef_table$OR <- exp(coef_table$estimate)
coef_table$CI_lower <- exp(coef_table$lower)
coef_table$CI_upper <- exp(coef_table$upper)
library(dplyr)
coef_table$term <- recode(coef_table$term,
                          age_adjusted = "Age (years)",
                          WBC_1 = "WBC",
                          CRP = "CRP",
                          ESR = "ESR")

library(ggplot2)

ggplot(coef_table, aes(x = term, y = OR)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() + # horizontal layout
    labs(
        title = "Odds Ratios with 95% CI",
        x = "Predictor",
        y = "Odds Ratio (log scale)"
    ) +
    scale_y_log10() + # log scale for ORs
    theme_classic()
#vif 
modle_glm <- glm(CT_max ~ WBC_1 + ESR + CRP + age_adjusted , data = balanced_data, family = binomial)
library(car)
vif(modle_glm)#all 1
#WBC_1          ESR          CRP age_adjusted 
#1.020504     1.250877     1.236441     1.043030 
#----------------------------------------------------------------

#           OR           Lower 95%  Upper 95%
#(Intercept)  0.7831792 0.3432566 1.7708926
#WBC_1        0.9999995 0.9999938 1.0000050
#CRP          0.9996935 0.9901405 1.0093766
#ESR          0.9903737 0.9807209 0.9996373
#age_adjusted 1.1131217 1.0423885 1.1926372
#-----------------------------------------------------------------
#Coefficients:
#    coef     se(coef)    lower 0.95    upper 0.95
#(Intercept)  -2.443938e-01 4.160068e-01 -1.069277e+00  5.714837e-01
#WBC_1        -5.051150e-07 2.743896e-06 -6.157159e-06  5.030076e-06
#CRP          -3.065218e-04 4.872814e-03 -9.908405e-03  9.332869e-03
#ESR          -9.672938e-03 4.833485e-03 -1.946733e-02 -3.627498e-04
#age_adjusted  1.071684e-01 3.416061e-02  4.151472e-02  1.761670e-01
#Chisq                       p               method
#(Intercept)   0.345229506 0.556826094      2
#WBC_1         0.033284626 0.855236852      2
#CRP           0.003934344 0.949986006      2
#ESR           4.151662292 0.041593672      2
#age_adjusted 10.438279064 0.001234302      2