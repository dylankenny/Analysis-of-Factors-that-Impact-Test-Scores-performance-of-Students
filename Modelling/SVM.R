library(caret)
library(kernlab)       
library(doParallel)    
library(foreach)
library(ggplot2)
library(dplyr)

# Enable parallel processing using all but one core
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Data Preparation -----------------------------------------------------------
X <- as.data.frame(year5_dataset_scaled[, !names(year5_dataset_scaled) %in% c("ENG5")])
y <- year5_dataset_scaled$ENG5
X <- X[, sapply(X, is.numeric)]

# Enhanced SVM Cross-Validation Function -------------------------------------
# Performs 10-fold cross-validation.
evaluate_svm_cv <- function(X, y, k = 10) {
  set.seed(123)
  folds <- createFolds(y, k = k, list = TRUE)
  
  # Run each fold in parallel with foreach and explicitly load required packages.
  results <- foreach(i = seq_along(folds), .combine = 'rbind',
                     .packages = c("caret", "kernlab")) %dopar% {
                       train_indices <- folds[[i]]
                       test_indices <- setdiff(seq_along(y), train_indices)
                       
                       # Hyperparameter grid: Using a logarithmic scale for comprehensive search.
                       tune_grid <- expand.grid(
                         C = 10^seq(-2, 2, length = 5),           # Cost parameter
                         sigma = 10^seq(-3, 1, length = 5)          # Kernel (RBF) parameter
                       )
                       
                       # Train SVM model with 5-fold CV inside each fold for tuning.
                       # Note: allowParallel is set to FALSE here to avoid nested parallelism.
                       svm_model <- train(
                         x = X[train_indices, , drop = FALSE],
                         y = y[train_indices],
                         method = "svmRadial",
                         tuneGrid = tune_grid,
                         preProcess = c("center", "scale"),
                         trControl = trainControl(
                           method = "cv",
                           number = 5,
                           allowParallel = FALSE
                         )
                       )
                       
                       # Generate predictions on the test set.
                       predictions <- predict(svm_model, newdata = X[test_indices, , drop = FALSE])
                       
                       # Return calculated metrics.
                       data.frame(
                         fold = i,
                         rmse = RMSE(predictions, y[test_indices]),
                         mae  = MAE(predictions, y[test_indices]),
                         r2   = R2(predictions, y[test_indices])
                       )
                     }
  
  # Return aggregated performance measures.
  list(
    mean_rmse = mean(results$rmse, na.rm = TRUE),
    mean_mae  = mean(results$mae, na.rm = TRUE),
    mean_r2   = mean(results$r2, na.rm = TRUE)
  )
}

# Initial Model Evaluation ---------------------------------------------------
svm_performance <- evaluate_svm_cv(X, y)
cat("SVM Initial Performance:\n",
    "RMSE:", svm_performance$mean_rmse, "\n",
    "MAE:", svm_performance$mean_mae, "\n",
    "R²:", svm_performance$mean_r2, "\n")

# Recursive Feature Elimination (RFE) using SVM ------------------------------
set.seed(123)
ctrl <- rfeControl(
  functions = caretFuncs,
  method = "cv",
  number = 10,
  saveDetails = TRUE,
  returnResamp = "final",
  allowParallel = TRUE
)

# Define subset sizes for RFE
subset_sizes <- seq(5, ncol(X), by = 5)
if(max(subset_sizes) != ncol(X)) subset_sizes <- c(subset_sizes, ncol(X))

svm_rfe <- rfe(
  x = X,
  y = y,
  sizes = subset_sizes,
  rfeControl = ctrl,
  metric = "RMSE",
  method = "svmRadial",
  tuneGrid = expand.grid(C = 1, sigma = 0.1)
)

# RFE Results -----------------------------------------------------------------
print(svm_rfe)
optimal_features <- predictors(svm_rfe)
cat("\nOptimal features:", optimal_features, "\n")

# Final Model Evaluation using selected features -----------------------------
final_performance <- evaluate_svm_cv(X[, optimal_features, drop = FALSE], y)
cat("\nFinal SVM Performance (using optimal features):\n",
    "RMSE:", final_performance$mean_rmse, "\n",
    "MAE:", final_performance$mean_mae, "\n",
    "R²:", final_performance$mean_r2, "\n")

# Feature Importance Visualization -------------------------------------------
var_imp <- varImp(svm_rfe$fit, scale = TRUE)

# Convert to data frame and clean feature names
importance_df <- as.data.frame(var_imp$importance)
importance_df$Feature <- rownames(importance_df)
colnames(importance_df) <- c("Importance", "Feature")

# Normalize importance scores to percentage scale
importance_df$Importance <- (importance_df$Importance / sum(importance_df$Importance)) * 100

# Order features by importance
importance_df <- importance_df[order(-importance_df$Importance), ]

# Display top 15 features
print(head(importance_df),15)
# Create horizontal bar plot with improved styling
ggplot(head(importance_df,15), 
       aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#0072B2", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Importance)), 
            hjust = -0.1, size = 3.5, color = "darkgray") +
  coord_flip() +
  labs(title = "Feature Importance for Year 9 English Performance",
       subtitle = "Percentage Contribution to Model Accuracy",
       x = "",
       y = "Relative Importance (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Cleanup parallel processing resources
stopCluster(cl)


