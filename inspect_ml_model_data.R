
library(feather)
library(dplyr) # For glimpse

data_file <- "/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data.feather"
if (!file.exists(data_file)) {
  stop(paste("Data file not found at:", data_file))
}
dt <- read_feather(data_file)

print("Glimpse of ml_model_data.feather:")
glimpse(dt)
