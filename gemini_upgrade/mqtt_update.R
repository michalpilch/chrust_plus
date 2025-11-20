library(data.table)
library(DBI)
library(dplyr)
library(lubridate)
library(tidyr)

# Connect to the local Domoticz database
con <- dbConnect(RSQLite::SQLite(), "/home/dsl/domoticz/domoticz.db")

# Read new temperature data for the last 7 days
df_raw <- dbGetQuery(con, "SELECT * FROM Temperature WHERE Date > strftime('%Y-%m-%d %H:%M:%S', 'now', '-7 days')")

# Disconnect from the database
dbDisconnect(con)

# Initial processing (rename and convert Date)
df_processed <- df_raw %>%
  rename(
    date = Date,
    chill = Chill,
    rh = Humidity,
    pressure = Barometer,
    dew = DewPoint,
    setpoint = SetPoint
  ) %>%
  mutate(date = as.POSIXct(date)) %>%
  # Create a 'name' column by mapping DeviceRowID to meaningful names for pivoting
  mutate(name = case_when(
    DeviceRowID == 1 ~ "temp_piec",
    DeviceRowID == 2 ~ "temp_co",
    DeviceRowID == 3 ~ "bufor_top",
    DeviceRowID == 4 ~ "bufor_mid1",
    DeviceRowID == 5 ~ "bufor_mid2",
    DeviceRowID == 6 ~ "bufor_bottom",
    DeviceRowID == 7 ~ "temp_home",
    DeviceRowID == 8 ~ "temp",
    DeviceRowID == 9 ~ "wind",
    TRUE ~ as.character(DeviceRowID) # Fallback for any unexpected DeviceRowID
  ))

# The df_processed dataframe will be used as input to the process_data function in functions.R