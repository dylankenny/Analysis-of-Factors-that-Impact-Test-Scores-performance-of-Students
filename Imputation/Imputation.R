# Load necessary libraries
library(mice)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(VIM)
library(naniar)
library(missForest)

# Function to calculate correlations and handle NA values
calculate_missingness_correlations <- function(data_with_features_removed) {
  # Calculate missingness percentages
  missingness_percentages <- colSums(is.na(data_with_features_removed)) / nrow(data_with_features_removed) * 100
  
  # Create missingness matrix (1 = missing, 0 = observed)
  missingness_matrix <- as.data.frame(lapply(data_with_features_removed, function(x) as.numeric(is.na(x))))
  
  # Compute correlation matrix
  missingness_correlation <- cor(missingness_matrix)
  
  # Replace NA values with 0
  missingness_correlation[is.na(missingness_correlation)] <- 0
  
  # Calculate the second highest absolute correlation for each feature
  max_correlations <- apply(missingness_correlation, 2, function(x) {
    # Remove the self-correlation
    x <- x[-1]
    
    # Sort absolute correlations in descending order
    sorted_correlations <- sort(abs(x), decreasing = TRUE)
    
    # Return the second highest correlation if it exists, otherwise return 0
    if (length(sorted_correlations) > 1) {
      return(sorted_correlations[2])
    } else if (length(sorted_correlations) == 1) {
      return(sorted_correlations[1])
    } else {
      return(0)  # Return 0 if there are no correlations
    }
  })
  
  # Replace any remaining NA values with 0
  max_correlations[is.na(max_correlations)] <- 0
  
  return(list(
    missingness_percentages = missingness_percentages,
    missingness_correlation = missingness_correlation,
    max_correlations = max_correlations
  ))
}


# Function to categorize variables by missingness type
categorize_missingness <- function(data_with_features_removed) {
  # Calculate missingness correlations
  missingness_info <- calculate_missingness_correlations(data_with_features_removed)
  missingness_percentages <- missingness_info$missingness_percentages
  max_correlations <- missingness_info$max_correlations
  
  # Create categories based on specifications
  categories <- list(
    MNAR = names(missingness_percentages[missingness_percentages > 10 & max_correlations >= 0.6]),
    MAR = names(missingness_percentages[missingness_percentages >= 1 & 
                                          missingness_percentages <= 10 & 
                                          max_correlations >= 0.6]),
    MCAR = names(missingness_percentages[missingness_percentages > 0 & 
                                           missingness_percentages < 1 & 
                                           max_correlations <= 0.6]),
    No_Missingness = names(missingness_percentages[missingness_percentages == 0])
  )
  
  # Check if all variables with missing values are categorized
  all_vars_with_missing <- names(missingness_percentages[missingness_percentages > 0])
  categorized_vars <- c(categories$MNAR, categories$MAR, categories$MCAR)
  uncategorized_vars <- setdiff(all_vars_with_missing, categorized_vars)
  
  # Handle uncategorized variables
  if (length(uncategorized_vars) > 0) {
    cat("\nWARNING: Found", length(uncategorized_vars), "variables with missing values that weren't categorized:\n")
    for (var in uncategorized_vars) {
      cat("  -", var, ":", missingness_percentages[var], "% missing, max correlation:", max_correlations[var], "\n")
      
      # Assign uncategorized variables based on missingness percentage
      if (missingness_percentages[var] > 10) {
        cat("  Adding", var, "to MNAR category (high missingness)\n")
        categories$MNAR <- c(categories$MNAR, var)
      } else if (missingness_percentages[var] >= 1) {
        cat("  Adding", var, "to MAR category (moderate missingness)\n")
        categories$MAR <- c(categories$MAR, var)
      } else {
        cat("  Adding", var, "to MCAR category (low missingness)\n")
        categories$MCAR <- c(categories$MCAR, var)
      }
    }
  }
  
  # Print summary
  cat("\nMissingness categorization summary:\n")
  cat("MNAR variables (>10% missing, correlation ≥0.6):", 
      paste(categories$MNAR, collapse=", "), "\n")
  cat("MAR variables (1-10% missing, correlation ≥0.6):", 
      paste(categories$MAR, collapse=", "), "\n")
  cat("MCAR variables (<1% missing, correlation ≤0.6):", 
      paste(categories$MCAR, collapse=", "), "\n")
  cat("No Missingness:", paste(categories$No_Missingness, collapse=", "), "\n")
  
  return(categories)
}

# Function to check distributions before and after imputation
check_distribution <- function(original, imputed, var_name) {
  if (sum(is.na(original[[var_name]])) == 0) {
    return(NULL)  # Skip if no missing values
  }
  
  orig_vals <- original[[var_name]][!is.na(original[[var_name]])]
  imp_vals <- imputed[[var_name]][is.na(original[[var_name]])]
  
  if (length(imp_vals) == 0) {
    return(NULL)  # Skip if no values were imputed
  }
  
  if (is.numeric(orig_vals)) {
    # For numeric variables, plot density
    p <- ggplot() +
      geom_density(aes(x = orig_vals, color = "Original"), size = 1) +
      geom_density(aes(x = imp_vals, color = "Imputed"), size = 1) +
      scale_color_manual(values = c("Original" = "blue", "Imputed" = "red")) +
      labs(title = var_name, x = "Values", y = "Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else {
    # For categorical variables, create barplots
    orig_table <- table(orig_vals)
    imp_table <- table(imp_vals)
    
    # Combine data for plotting
    categories <- unique(c(names(orig_table), names(imp_table)))
    
    orig_counts <- sapply(categories, function(cat) {
      if (cat %in% names(orig_table)) orig_table[cat] else 0
    })
    
    imp_counts <- sapply(categories, function(cat) {
      if (cat %in% names(imp_table)) imp_table[cat] else 0
    })
    
    plot_data <- data.frame(
      Category = rep(categories, 2),
      Count = c(orig_counts, imp_counts),
      Type = factor(rep(c("Original", "Imputed"), each = length(categories)))
    )
    
    p <- ggplot(plot_data, aes(x = Category, y = Count, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
      labs(title = var_name, x = "Values", y = "Count") +
      theme_minimal() +
      theme(legend.position = "bottom", 
            axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  return(p)
}

# Function to impute data based on missingness categories
impute_data <- function(data_with_features_removed, categories) {
  imputed_data <- data_with_features_removed
  
  # Impute MNAR variables (Predictive Mean Matching)
  if (!is.null(categories$MNAR) && length(categories$MNAR) > 0) {
    cat("Imputing MNAR variables...\n")
    if (length(categories$MNAR) > 1) {
      mnar_imp <- mice(data_with_features_removed[categories$MNAR], m = 20, maxit = 25, 
                       method = "pmm", seed = 123, printFlag = FALSE)
      imputed_data[categories$MNAR] <- complete(mnar_imp)
    } else {
      # Handle single column case
      col <- categories$MNAR
      # Get predictors - use all other columns
      predictors <- setdiff(colnames(data_with_features_removed), col)
      
      # Create temporary dataset
      temp_data <- data_with_features_removed[, c(col, predictors), drop = FALSE]
      
      # Use mice for imputation
      mice_imp <- mice(temp_data, m = 5, maxit = 20, 
                       method = "pmm", seed = 123, printFlag = FALSE)
      imputed_data[[col]] <- complete(mice_imp)[[col]]
    }
  }
  
  # Impute MAR variables (MICE with CART)
  if (!is.null(categories$MAR) && length(categories$MAR) > 0) {
    cat("Imputing MAR variables...\n")
    if (length(categories$MAR) > 1) {
      mar_imp <- mice(data_with_features_removed[categories$MAR], m = 20, maxit = 50, 
                      method = "cart", seed = 123, printFlag = FALSE)
      imputed_data[categories$MAR] <- complete(mar_imp)
    } else {
      # Handle single column case
      col <- categories$MAR
      # Get predictors - use all other columns
      predictors <- setdiff(colnames(data_with_features_removed), col)
      
      # Create temporary dataset 
      temp_data <- data_with_features_removed[, c(col, predictors), drop = FALSE]
      
      # Use mice for imputation
      mice_imp <- mice(temp_data, m = 5, maxit = 20, 
                       method = "cart", seed = 123, printFlag = FALSE)
      imputed_data[[col]] <- complete(mice_imp)[[col]]
    }
  }
  
  # Impute MCAR variables
  if (!is.null(categories$MCAR) && length(categories$MCAR) > 0) {
    cat("Imputing MCAR variables...\n")
    mcar_data <- as.data.frame(data_with_features_removed[categories$MCAR])
    
    # Calculate missingness percentages
    missing_percentages <- colSums(is.na(mcar_data)) / nrow(mcar_data) * 100
    
    # Split into low/high missingness
    low_missing_vars <- names(missing_percentages[missing_percentages < 0.5])
    high_missing_vars <- names(missing_percentages[missing_percentages >= 0.5])
    
    # Impute low missingness using kNN
    if (length(low_missing_vars) > 0) {
      cat("  Using kNN for variables with <0.5% missingness...\n")
      knn_imp <- VIM::kNN(mcar_data, variable = low_missing_vars, k = 5)
      # Extract only the actual imputed columns (not the '_imp' columns)
      actual_cols <- intersect(colnames(knn_imp), low_missing_vars)
      mcar_data[actual_cols] <- knn_imp[actual_cols]
    }
    
    # Fix for high missingness using PMM
    if (length(high_missing_vars) > 0) {
      cat("  Using PMM for variables with ≥0.5% missingness...\n")
      for (col in high_missing_vars) {
        # Use available predictors from the dataset
        potential_predictors <- setdiff(colnames(data_with_features_removed), col)
        
        # Check if we have enough predictors
        if (length(potential_predictors) > 0) {
          # Select a limited number of predictors to avoid overfitting
          selected_predictors <- head(potential_predictors, min(5, length(potential_predictors)))
          
          # Create temporary dataset with selected predictors
          temp_data <- data_with_features_removed[, c(col, selected_predictors), drop = FALSE]
          mice_imp <- mice(temp_data, m = 5, maxit = 20, 
                           method = "pmm", seed = 123, printFlag = FALSE)
          mcar_data[[col]] <- complete(mice_imp)[[col]]
        } else {
          # Fallback to mean/mode imputation if no predictors available
          if (is.numeric(mcar_data[[col]])) {
            mcar_data[[col]][is.na(mcar_data[[col]])] <- mean(mcar_data[[col]], na.rm = TRUE)
          } else {
            mode_val <- names(sort(table(mcar_data[[col]]), decreasing = TRUE))[1]
            mcar_data[[col]][is.na(mcar_data[[col]])] <- mode_val
          }
        }
      }
    }
    
    # Update the imputed data with the imputed MCAR variables
    imputed_data[categories$MCAR] <- mcar_data
  }
  
  return(imputed_data)
}

# Function to visualize distribution comparisons
visualize_distributions <- function(data_with_features_removed, imputed_data, categories) {
  # Visualize each category
  for (category_name in c("MNAR", "MAR", "MCAR")) {
    cat("Visualizing", category_name, "variables...\n")
    variables <- categories[[category_name]]
    
    if (length(variables) == 0) {
      cat("  No variables in this category.\n")
      next
    }
    
    plots <- list()
    for (var in variables) {
      p <- check_distribution(data_with_features_removed, imputed_data, var)
      if (!is.null(p)) {
        plots[[var]] <- p
      }
    }
    
    if (length(plots) > 0) {
      # Display plots in batches to avoid overcrowding
      batch_size <- 6
      num_batches <- ceiling(length(plots) / batch_size)
      
      for (i in 1:num_batches) {
        start_idx <- (i-1) * batch_size + 1
        end_idx <- min(i * batch_size, length(plots))
        batch_plots <- plots[start_idx:end_idx]
        
        grid.arrange(grobs = batch_plots, 
                     ncol = min(3, length(batch_plots)),
                     top = paste(category_name, "Variables - Batch", i))
      }
    } else {
      cat("  No plots to display for this category.\n")
    }
  }
}

# Main workflow function
imputation_workflow <- function(data_with_features_removed) {
  # Step 1: Categorize variables by missingness type
  cat("Categorizing variables by missingness patterns...\n")
  categories <- categorize_missingness(data_with_features_removed)
  
  # Step 2: Apply imputation
  cat("\nApplying imputation based on categorization...\n")
  imputed_data <- impute_data(data_with_features_removed, categories)
  
  # Step 3: Visualize missingness before and after imputation
  cat("\nVisualizing missingness before imputation...\n")
  vis_miss(data_with_features_removed, cluster = TRUE)
  
  cat("\nVisualizing missingness after imputation...\n")
  vis_miss(imputed_data, cluster = TRUE)
  
  # Step 4: Check any remaining missing values
  missing_counts <- colSums(is.na(imputed_data))
  if (sum(missing_counts) > 0) {
    cat("\nRemaining missing values after imputation:\n")
    print(missing_counts[missing_counts > 0])
  } else {
    cat("\nAll missing values successfully imputed.\n")
  }
  
  # Step 5: Visualize distributions before and after imputation
  cat("\nComparing distributions before and after imputation...\n")
  visualize_distributions(data_with_features_removed, imputed_data, categories)
  
  # Return the imputed dataset
  return(imputed_data)
}

# Run the imputation workflow on your dataset
imputed_data <- imputation_workflow(data_with_features_removed)
vis_miss(imputed_data)
