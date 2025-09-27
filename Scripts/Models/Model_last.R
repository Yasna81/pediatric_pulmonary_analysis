#in this script : > model > metrics and curve > cross-validation > metrics > goodness of fit 
#---------------------------------------------------------------------------------------------------




library(logistf)
#df_3$Proven <- ifelse(df_3$EORTC_MSG_label == "Proven",1,0)
model_x <- logistf(Nodule_CT ~ WBC_1+CRP+ESR+age_adjusted, data=df_3)


summary(model_x)
exp(cbind(OR=coef(model_x),confint(model_x)))
library(pROC)
pred_probs <- predict(model_x, type = "response")
roc_obj <- roc(df_3$Nodule_CT,pred_probs)
plot(roc_obj)
auc(roc_obj)

###
library(caret)
#predicted class
predicted_class <- ifelse(pred_probs > 0.5 ,1,0 )
actual <- df_3$Nodule_CT
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

#print(metrics)
#Accuracy Recall Precision Specificity F1_Score
#Accuracy    0.678  0.822     0.714       0.429    0.764
#cross-validation 
# Load packages
library(logistf)
library(pROC)
library(caret)

# Set seed for reproducibility
set.seed(123)

# 5-fold stratified cross-validation
folds <- createFolds(df_3$Nodule_CT, k = 5, list = TRUE)

# Initialize metric storage
auc_list <- c()
acc_list <- c()
f1_list <- c()
sens_list <- c()
spec_list <- c()

# Start cross-validation
for (i in 1:5) {
    test_idx <- folds[[i]]
    test_data <- df_3[test_idx, ]
    train_data <- df_3[-test_idx, ]
    
    # Fit Firth logistic regression
    model <- logistf(Nodule_CT ~ WBC_1 + CRP + ESR  + age_adjusted, data = train_data)
    
    # Predict probabilities
    probs <- predict(model, newdata = test_data, type = "response")
    
    # Predicted classes using 0.5 cutoff
    preds <- ifelse(probs > 0.5, 1, 0)
    
    # Actual values
    actuals <- test_data$Nodule_CT
    
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
#Average AUC:  0.619 
#> cat("Average Accuracy: ", round(mean(acc_list, na.rm = TRUE), 3), "\n")
#Average Accuracy:  0.617 
#> cat("Average F1 Score: ", round(mean(f1_list, na.rm = TRUE), 3), "\n")
#Average F1 Score:  0.71 
#> cat("Average Sensitivity:", round(mean(sens_list, na.rm = TRUE), 3), "\n")
#Average Sensitivity: 0.762 
#> cat("Average Specificity:", round(mean(spec_list, na.rm = TRUE), 3), "\n")
#Average Specificity: 0.372 
#> cat("==========================================\n")
#==========================================
#    > 
#goodness of fit :
library(ResourceSelection)
hoslem.test(as.numeric(actual), y= pred_probs, g= 10)
#0.9
library(dplyr)
summary_model <- summary(model_x)
####
# Create a data frame manually
coef_table <- data.frame(
    term = names(model_x$coefficients),
    estimate = model_x$coefficients,
    lower = model_x$ci.lower,
    upper = model_x$ci.upper,
    p_value = model_x$prob
)

# Remove the intercept if you don't want to plot it
coef_table <- coef_table[coef_table$term != "(Intercept)", ]

# Compute Odds Ratios
coef_table$OR <- exp(coef_table$estimate)
coef_table$CI_lower <- exp(coef_table$lower)
coef_table$CI_upper <- exp(coef_table$upper)

coef_table$term <-recode(coef_table$term,
                         "age_adjusted" = "Age (years)",
                         "WBC_1" = "WBC",
                         "CRP" = "CRP",
                         "ESR" = "ESR")
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
modle_glm <- glm(Nodule_CT ~ WBC_1 + ESR + CRP + age_adjusted , data = df_3, family = binomial)
library(car)
vif(modle_glm)

# calib plot 
library(rms)
cal <- val.prob(pred_probs, as.numeric(df_3$Nodule_CT, g = 10 , pl = TRUE ))
#<0.05