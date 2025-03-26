library(caret)
library(randomForest)
library(doParallel)  # For parallel processing

# Enable parallel processing using all but one core
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Data Preparation -----------------------------------------------------------
X <- as.data.frame(year5_dataset_scaled[, !names(year5_dataset_scaled) %in% c("ENG5")])
y <- year5_dataset_scaled$ENG5
X <- X[, sapply(X, is.numeric)]

# Cross-Validation Function -------------------------------------------------
evaluate_model_cv <- function(X, y, k = 10) {
  # Convert to data frame and check for invalid values[1][4]
  X <- as.data.frame(X)
  y <- as.numeric(y)
  
  
  # Ensure valid data dimensions
  if(nrow(X) == 0 || length(y) == 0) {
    stop("No valid data remaining after preprocessing")
  }
  
  # Initialize results storage
  results <- list(
    rmse = numeric(k),
    mae = numeric(k),
    r2 = numeric(k)
  )
  
  folds <- createFolds(y, k = k, list = TRUE)
  
  for(i in seq_along(folds)) {
    tryCatch({
      train_indices <- folds[[i]]
      test_indices <- setdiff(seq_along(y), train_indices)
      
      # Dynamic mtry calculation with floor protection[3]
      mtry_value <- max(floor(ncol(X)/3), 1)
      
      # Train model with error handling[1]
      rf_model <- randomForest(
        x = X[train_indices, , drop = FALSE],
        y = y[train_indices],
        ntree = 1000,
        mtry = mtry_value,
        importance = TRUE,
        na.action = na.omit
      )
      
      # Generate predictions
      predictions <- predict(rf_model, newdata = X[test_indices, , drop = FALSE])
      
      # Calculate metrics with validity checks
      if(length(predictions) > 0 && !any(is.na(predictions))) {
        results$rmse[i] <- sqrt(mean((y[test_indices] - predictions)^2))
        results$mae[i] <- mean(abs(y[test_indices] - predictions))
        results$r2[i] <- cor(y[test_indices], predictions)^2
      }
    }, error = function(e) {
      message("Error in fold ", i, ": ", e$message)
    })
  }
  
  # Return valid results
  list(
    mean_rmse = mean(results$rmse, na.rm = TRUE),
    mean_mae = mean(results$mae, na.rm = TRUE),
    mean_r2 = mean(results$r2, na.rm = TRUE)
  )
}

# Initial Model Evaluation ---------------------------------------------------
initial_performance <- evaluate_model_cv(X, y)
cat("Initial Performance:\n",
    "RMSE:", initial_performance$mean_rmse, "\n",
    "MAE:", initial_performance$mean_mae, "\n",
    "R²:", initial_performance$mean_r2, "\n")

# Recursive Feature Elimination ----------------------------------------------
set.seed(123)
ctrl <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  saveDetails = TRUE,
  returnResamp = "final",
  allowParallel = TRUE
)

# Use more efficient subset sizes
subset_sizes <- seq(5, ncol(X), by = 5)  
if(max(subset_sizes) != ncol(X)) subset_sizes <- c(subset_sizes, ncol(X))

rfe_result <- rfe(
  x = X,
  y = y,
  sizes = subset_sizes,
  rfeControl = ctrl,
  metric = "RMSE",
  ntree = 500
)

# RFE Results ----------------------------------------------------------------
print(rfe_result)
optimal_features <- predictors(rfe_result)
cat("\nOptimal features:", optimal_features, "\n")

# Final Model Evaluation ------------------------------------------------------
final_performance <- evaluate_model_cv(X[, optimal_features], y)
cat("\nFinal Performance:\n",
    "RMSE:", final_performance$mean_rmse, "\n",
    "MAE:", final_performance$mean_mae, "\n",
    "R²:", final_performance$mean_r2, "\n")

# Feature Importance Visualization -------------------------------------------
importance_df <- data.frame(
  Feature = names(rfe_result$fit$importance[, "%IncMSE"]),
  Importance = rfe_result$fit$importance[, "%IncMSE"]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance (RFE Optimized Model)",
       x = "Features",
       y = "% Increase in MSE") +
  theme_minimal()


# Cleanup parallel processing resources
stopCluster(cl)

