# Load dplyr library
library(dplyr)

# Remove "MATH9" from year9_dataset using dplyr
year9_dataset <- year9_dataset %>%
  select(+MATH9)

# Check if "MATH9" was removed
if (!"MATH9" %in% colnames(year9_dataset)) {
  cat("'MATH9' has been removed from year9_dataset.\n")
} else {
  cat("'MATH9' is still present in year9_dataset.\n")
}

# Remove "MATH9" from year9_dataset using dplyr
year5_dataset <- year5_dataset %>%
  select(+ENG5)

# Check if "MATH9" was removed
if (!"ENG5" %in% colnames(year5_dataset)) {
  cat("'MATH5' has been removed from year9_dataset.\n")
} else {
  cat("'MATH9' is still present in year9_dataset.\n")
}
