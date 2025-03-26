library(ggplot2)

# Create the data frame with GBM performance metrics
final_gbm_perf <- data.frame(
  Metric = c("RMSE", "MAE", "R2"),
  Train = c(0.490495, 0.3849797, 0.794903),
  Test = c(0.8237046, 0.6581673, 0.339235)
)

final_elastic <- data.frame(
  Metric = c("RMSE", "MAE", "R2"),
  Train = c(0.8523354, 0.692742, 0.272137),
  Test = c(0.865197, 0.7055703, 0.2675806)  
)

# Reshape the data to long format for ggplot2
final_performance_long <- reshape2::melt(final_gbm_perf, id.vars = "Metric", 
                                         variable.name = "Dataset", value.name = "Value")

# Create the bar chart
performance_plot <- ggplot(final_performance_long, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Test" = "indianred2", "Train" = "#00CDCD")) + # Custom colors
  labs(
    title = "Comparison of Model Performance Metrics: Training vs Test",
    x = "Performance Metric",
    y = "Value"
  ) +
  theme_minimal()

# Display the plot
print(performance_plot)
