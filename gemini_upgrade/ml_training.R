library(h2o)
library(feather)
library(dplyr)

# Initialize H2O
h2o.init()

# Load Data
dt <- feather::read_feather("/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data_hourly_demand.feather")

# Calculate wood needed to reach 35 kWh
dt <- dt %>%
  mutate(wood_needed = (35 - Q_buf) / 4) %>%
  filter(wood_needed > 0)

# Convert to H2OFrame
dt_h2o <- as.h2o(dt)

# Define predictors and response
predictors <- c("temp_home", "temp", "temp_piec", "temp_co", "bufor_top", "bufor_mid1", "bufor_mid2", "bufor_bottom", "WABT", "heating_cycle", "discharging_cycle", "heat_demand_kwh", "temp_lag1", "heat_demand_kwh_lag1")
response <- "wood_needed"

# Split the data
splits <- h2o.splitFrame(data = dt_h2o, ratios = 0.8, seed = 123)
train <- splits[[1]]
test <- splits[[2]]

# Train the GBM model
gbm_model <- h2o.gbm(x = predictors,
                     y = response,
                     training_frame = train,
                     validation_frame = test,
                     ntrees = 150,
                     max_depth = 3,
                     learn_rate = 0.1,
                     seed = 123)

# Print model summary
print(gbm_model)

# Save the trained model
h2o.saveModel(gbm_model, path = "/home/dsl/gemini/chrust_plus/gemini_upgrade/", force = TRUE)

# Shutdown H2O
h2o.shutdown(prompt = FALSE)