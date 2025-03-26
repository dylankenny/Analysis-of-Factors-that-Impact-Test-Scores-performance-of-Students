library(ggplot2)
library(reshape2)

# Data for Elastic Net
elastic_net <- data.frame(
  Metric = c("RMSE", "MAE", "R²"),
  Train_Initial = c(0.8352234, 0.67814, 0.3077106),
  Test_Initial = c(0.8730806, 0.7114465, 0.2442234),
  Train_Final = c(0.8523354, 0.692742, 0.272137),
  Test_Final = c(0.865197, 0.7055703, 0.2675806)
)

# Data for GBM
gbm <- data.frame(
  Metric = c("RMSE", "MAE", "R²"),
  Train_Initial = c(0.4791101, 0.375694, 0.810069),
  Test_Initial = c(0.8434596, 0.671005, 0.3040875),
  Train_Final = c(0.493516, 0.3882441, 0.7917241),
  Test_Final = c(0.8262528, 0.6621518, 0.3347315)
)


# Melt the data for Elastic Net
elastic_net_melted <- melt(elastic_net, id.vars = "Metric", variable.name = "Condition", value.name = "Value")
elastic_net_melted$Model <- "Elastic Net"

# Melt the data for GBM
gbm_melted <- melt(gbm, id.vars = "Metric", variable.name = "Condition", value.name = "Value")
gbm_melted$Model <- "GBM"

# Combine both datasets
combined_data <- rbind(elastic_net_melted, gbm_melted)

# Create the graph
ggplot(combined_data, aes(x = Metric, y = Value, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Model) +
  labs(
    title = "Comparison of Performance Metrics Before and After Recursive Feature Elimination",
    x = "Metrics",
    y = "Values",
    fill = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
