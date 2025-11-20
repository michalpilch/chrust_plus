library(dplyr)
library(feather)

# Source the data update script to get df_processed
# This script connects to the database and performs initial cleaning.
source("mqtt_update.R")

# Source the functions script which contains the main data processing logic
source("functions.R")

# Process the raw data to get the final dt
# The process_data function will pivot the data and perform feature engineering
dt_final <- process_data(df_processed)

# Define the output path for the processed data
output_file <- "ml_model_data_for_app.feather"

# Save the processed data to a feather file that the Shiny app and flexdashboard will load
tryCatch({
  feather::write_feather(dt_final, output_file)
  print(paste("ETL process completed successfully. Data saved to:", output_file))
}, error = function(e) {
  stop(paste("Failed to write feather file:", e$message))
})