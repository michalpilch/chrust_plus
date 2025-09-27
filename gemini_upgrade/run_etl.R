library(dplyr)
library(feather)

# Source the data update script to get df_processed
source("/home/dsl/gemini/chrust_plus/gemini_upgrade/mqtt_update.R")

# Source the functions script which contains process_data
source("/home/dsl/gemini/chrust_plus/gemini_upgrade/functions.R")

# Process the raw data to get the final dt
dt_final <- process_data(df_processed)

# Save the processed data to a file that the Shiny app will load
feather::write_feather(dt_final, "/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data_for_app.feather")

print("ETL process completed and data saved to ml_model_data_for_app.feather")
