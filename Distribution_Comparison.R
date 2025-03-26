library(tidyverse)

# Identify pairs of variables to compare
vars_to_compare <- c(
  "ATTITUDE_ENG",
  "CAMPUS_CONDITION",
  "CLASSMATERELATION",
  "INTEREST_ENG", 
  "PISA_12_ST81_ENG",
  "SATISIFACTION_LEARNING_ENG",
  "STRATEGE_ENG",
  "THR_ENG_NEW",
  "THR_MATH_NEW"
)

# Prepare the data for plotting
plot_data <- data.frame()

for(base_var in vars_to_compare) {
  var5 <- paste0(base_var, ".5")
  var9 <- paste0(base_var, ".9")
  
  # Check if both variables exist in the dataset
  if(var5 %in% names(data) && var9 %in% names(data)) {
    temp_data <- data %>%
      select(all_of(c(var5, var9))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        base_var = base_var,
        grade = ifelse(str_detect(variable, "\\.5$"), "Year5", "Year9")
      )
    
    plot_data <- bind_rows(plot_data, temp_data)
  }
}

# Remove NA values
plot_data <- plot_data %>% filter(!is.na(value))

# Create the density plot
ggplot(plot_data, aes(x = value, fill = grade)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~ base_var, ncol = 1, strip.position = "left") +
  scale_fill_manual(values = c("Year5" = "#FF7F7F", "Year9" = "#66CCC6")) +
  labs(
    title = "Comparison of Year 5 and Year 9 Measures",
    x = "Values",
    fill = "Grade"
  ) +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0, hjust = 1),
    panel.spacing = unit(0.5, "lines"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
