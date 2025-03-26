library(tidyverse)

# Reshape the data for plotting
data_long <- data %>%
  select(MATH5, MATH9, ENG5, ENG9) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "Scores")

# Create the boxplot
ggplot(data_long, aes(x = variable, y = Scores, fill = variable)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +  # Boxplot with red outliers
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue") +  # Mean as blue dot
  labs(
    title = "Boxplot of ENG5, ENG9, MATH5, and MATH9 Scores",
    x = "Exam Year",
    y = "Scores",
    fill = "Variable"  
  ) +
  scale_fill_manual(values = c("ENG5" = "#FF6F61", "ENG9" = "#6B8E23", "MATH5" = "#40E0D0", "MATH9" = "#9370DB")) +  
  theme_minimal() +
  theme(
    legend.position = "right",  
    plot.title = element_text(hjust = 0.5)  
  )
