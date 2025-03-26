library(ggplot2)

# Create the data frame with "After" data
data <- data.frame(
  Dataset = rep(c("ENG5", "ENG9", "MATH5", "MATH9"), each = 4),
  Model = rep(c("Elastic Net", "GBM", "Random Forest", "SVM"), times = 4),
  RMSE = c(0.874456, 0.8787334, 0.9161693, 0.9597182,
           0.9084324, 0.935809, 0.9637164, 0.9697058,
           0.9279847, 0.9203154, 0.9826119, 0.9742123,
           0.865197, 0.8345554, 0.9332452, 0.9504189),
  MAE = c(0.6789691, 0.6816953, 0.7212761, 0.751053,
          0.7261072, 0.7380983, 0.7733577, 0.7802532,
          0.7314808, 0.7407609, 0.7725499, 0.7719495,
          0.7055703, 0.6667842, 0.758616, 0.7678282),
  R_Squared = c(0.2460909, 0.2507383, 0.1713945, 0.156769,
                0.1988781, 0.1632626, 0.09694301, 0.07618836,
                0.1472663, 0.1790626, 0.05377718, 0.06972454,
                0.2675806, 0.3225532, 0.143977, 0.1885904)
)


# Create RMSE bar chart
rmse_plot <- ggplot(data, aes(x = Dataset, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "RMSE after RFE", y = "RMSE", x = "Dataset") +
  theme_minimal()

# Create MAE bar chart
mae_plot <- ggplot(data, aes(x = Dataset, y = MAE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "MAE after RFE", y = "MAE", x = "Dataset") +
  theme_minimal()

# Create R-Squared bar chart
r_squared_plot <- ggplot(data, aes(x = Dataset, y = R_Squared, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = expression(R^2 ~ "after RFE"), y = expression(R^2), x = "Dataset") +
  theme_minimal()

# Display the plots
library(gridExtra)
grid.arrange(rmse_plot, mae_plot, r_squared_plot, ncol = 2)

