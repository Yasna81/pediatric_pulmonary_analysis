# this scrip was made for halo sign the results were the same as previous models
library(logistf)
library(pROC)
library(caret)

# Set seed for reproducibility
set.seed(123)
# 5-fold stratified cross-validation
folds <- createFolds(df_multi$Halo_CT, k = 5, list = TRUE)

# Initialize metric storage
auc_list <- c()
acc_list <- c()
f1_list <- c()
sens_list <- c()
spec_list <- c()

# Start cross-validation
for (i in 1:5) {
    test_idx <- folds[[i]]
    test_data <- df_multi[test_idx, ]
    train_data <- df_multi[-test_idx, ]
    
    # Fit Firth logistic regression
    model <- logistf(Halo_CT ~ WBC_1 + CRP + ESR  + age_adjusted, trControl = ctrl   ,  data = train_data)
    
    # Predict probabilities
    probs <- predict(model, newdata = test_data, type = "response")
    
    # Predicted classes using 0.5 cutoff
    preds <- ifelse(probs > 0.5, 1, 0)
    
    # Actual values
    actuals <- test_data$Halo_CT
    
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
model_halo <- logistf(Halo_CT ~ WBC_1 + CRP + ESR  + age_adjusted, trControl = ctrl   ,  data = df_multi)
summary(model_halo)
