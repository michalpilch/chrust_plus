
library(feather)
library(dplyr) # For glimpse

data_file <- "/home/dsl/gemini/chrust_plus/gemini_upgrade/backup_mqtt_mpm.feather"
if (!file.exists(data_file)) {
  stop(paste("Data file not found at:", data_file))
}
dt <- read_feather(data_file)

print("Class of 'date' column:")
print(class(dt$date))

print("First few entries of 'date' column:")
print(head(dt$date))

print("Glimpse of the data frame:")
glimpse(dt)
