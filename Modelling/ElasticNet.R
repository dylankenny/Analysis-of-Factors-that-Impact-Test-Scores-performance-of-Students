library(caret)
library(glmnet)
library(doParallel)
library(dplyr)

# Enable parallel processing
registerDoParallel(cores = detectCores() - 1)

# Data Preparation -----------------------------------------------------------
X <- as.data.frame(year9_dataset_scaled[, !names(year9_dataset_scaled) %in% c("ENG9")])
y <- year9_dataset_scaled$ENG9
X <- X[, sapply(X, is.numeric)]

# Cross-Validation Function -------------------------------------------------
evaluate_en_cv <- function(X, y, k = 10) {
  X <- as.data.frame(X)
  y <- as.numeric(y)
  
  results <- list(
    train_rmse = numeric(k),
    test_rmse = numeric(k),
    train_mae = numeric(k),
    test_mae = numeric(k),
    train_r2 = numeric(k),
    test_r2 = numeric(k)
  )
  
  folds <- createFolds(y, k = k, list = TRUE)
  
  for(i in seq_along(folds)) {
    tryCatch({
      test_indices <- folds[[i]]
      train_indices <- setdiff(seq_along(y), test_indices)
      
      train_data <- data.frame(y = y[train_indices], X[train_indices, , drop = FALSE])
      test_data <- data.frame(y = y[test_indices], X[test_indices, , drop = FALSE])
      
      en_model <- train(
        x = X[train_indices, , drop = FALSE],
        y = y[train_indices],
        method = "glmnet",
        trControl = trainControl(method = "cv", number = 5),
        tuneLength = 5  # Adjust for more thorough tuning
      )
      
      train_predictions <- predict(en_model, newdata = train_data, n.trees = gbm_model$n.trees)
      test_predictions <- predict(en_model, newdata = test_data, n.trees = gbm_model$n.trees)
      
      if(length(test_predictions) > 0 && !any(is.na(test_predictions))) {
        results$train_rmse[i] <- sqrt(mean((train_data$y - train_predictions)^2))
        results$test_rmse[i] <- sqrt(mean((test_data$y - test_predictions)^2))
        results$train_mae[i] <- mean(abs(train_data$y - train_predictions))
        results$test_mae[i] <- mean(abs(test_data$y - test_predictions))
        results$train_r2[i] <- cor(train_data$y, train_predictions)^2
        results$test_r2[i] <- cor(test_data$y, test_predictions)^2
      }
    }, error = function(e) {
      message("Error in fold ", i, ": ", e$message)
    })
  }
  
  data.frame(
    Train_RMSE = mean(results$train_rmse, na.rm = TRUE),
    Test_RMSE = mean(results$test_rmse, na.rm = TRUE),
    Train_MAE = mean(results$train_mae, na.rm = TRUE),
    Test_MAE = mean(results$test_mae, na.rm = TRUE),
    Train_R2 = mean(results$train_r2, na.rm = TRUE),
    Test_R2 = mean(results$test_r2, na.rm = TRUE)
  )
}

# Initial Model Evaluation ---------------------------------------------------
initial_performance <- evaluate_en_cv(X, y)
print(initial_performance)


# Feature Selection via Elastic Net ------------------------------------------
set.seed(123)
ctrl <- rfeControl(
  functions = caretFuncs,  # Using caret's default functions
  method = "cv",
  number = 10,
  saveDetails = TRUE,
  allowParallel = TRUE
)

# Reduced subset sizes for efficiency
subset_sizes <- seq(5, ncol(X), by = 10)
if(max(subset_sizes) != ncol(X)) subset_sizes <- c(subset_sizes, ncol(X))

rfe_result <- rfe(
  x = X,
  y = y,
  sizes = subset_sizes,
  rfeControl = ctrl,
  metric = "RMSE",
  method = "glmnet",
  tuneLength = 5
)

# RFE Results ----------------------------------------------------------------
print(rfe_result)
optimal_features <- predictors(rfe_result)
cat("\nOptimal features:", optimal_features, "\n")

# Final Model Evaluation ------------------------------------------------------
final_performance <- evaluate_en_cv(X[, optimal_features], y)
print(final_performance)
# Feature Importance Visualization -------------------------------------------

# Extract variable importance from the final GBM model
importance_obj <- varImp(rfe_result$fit, scale = TRUE)  # Scale importance scores

# Convert to data frame and clean feature names
importance_df <- as.data.frame(importance_obj$importance)
importance_df$Feature <- rownames(importance_df)
colnames(importance_df) <- c("Importance", "Feature")

# Normalize importance scores to percentage scale
importance_df$Importance <- (importance_df$Importance / sum(importance_df$Importance)) * 100

# Order features by importance
importance_df <- importance_df[order(-importance_df$Importance), ]

# Display top 15 features
print(head(importance_df, 15))
# Create horizontal bar plot with improved styling
ggplot(head(importance_df, 15), 
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

#------------------------------------------------------------------------------------------
###
### Depening on what variable is your target variables, change the name of the saved RDS files to suit your target variable
###

# Calculate feature means using the optimal features only
feature_means <- colMeans(year9_dataset_scaled[, optimal_features, drop = FALSE], na.rm = TRUE)

# Save final model, optimal features, and feature means for later use
saveRDS(rfe_result$fit, "EN_ENG9.rds")
saveRDS(optimal_features, "opt_ft_ENG9.rds")
saveRDS(feature_means, "feature_means_ENG9.rds")

# Stop parallel processing when done
stopImplicitCluster()
