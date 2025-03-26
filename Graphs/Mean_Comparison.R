library(tidyverse)

# Extract base feature names (without the .5 or .9 suffix)
base_features <- unique(gsub("\\.5$|\\.9$", "", names(data_restored)[grepl("\\.5$|\\.9$", names(data_restored))]))

# Create a data frame to store the differences
mean_differences <- data.frame(Feature = character(), Difference = numeric())

# Calculate the mean differences for each feature
for(feat in base_features) {
  year5_var <- paste0(feat, ".5")
  year9_var <- paste0(feat, ".9")
  
  # Check if both variables exist in the dataset
  if(year5_var %in% names(data_restored) && year9_var %in% names(data_restored)) {
    # Calculate the mean difference (Year 9 - Year 5)
    diff_value <- mean(data_restored[[year9_var]], na.rm = TRUE) - 
      mean(data_restored[[year5_var]], na.rm = TRUE)
    
    # Add to the data frame
    mean_differences <- rbind(mean_differences, 
                              data.frame(Feature = feat, Difference = diff_value))
  }
}

# Sort the data by the difference value
mean_differences <- mean_differences %>%
  arrange(desc(Difference))

# Create the lollipop plot
ggplot(mean_differences, aes(x = Difference, y = reorder(Feature, Difference))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_segment(aes(x = 0, xend = Difference, y = Feature, yend = Feature), 
               color = "black") +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Mean Difference Between Year 9 and Year 5",
    x = "Difference (Year 9 - Year 5)",
    y = "Feature"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
