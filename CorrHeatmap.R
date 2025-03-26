library(tidyverse)
library(naniar)
library(ggplot2)
library(corrplot)

# Correlation heatmap
data = read.csv("School_Survey.csv")

# Create a binary missingness indicator matrix
missing_indicators <- is.na(data)

# Identify variables with missing values
missing_vars <- colSums(missing_indicators) > 0
filtered_data <- missing_indicators[, missing_vars]

# Compute correlations between missingness indicators for filtered data
cor_matrix <- cor(filtered_data, use = "pairwise.complete.obs")

# Plot the correlation heatmap with improved formatting
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         tl.col = "black", 
         tl.cex = 0.7,                    # Slightly larger text
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Heatmap of Missingness Indicators",
         mar = c(0,0,2,0),               # Adjust margins
         addgrid.col = "darkgray")        # Add grid lines
