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
  mutate(date = as.POSIXct(date))

# Return the processed data frame
# This df_processed will be used as input to the process_data function in functions.R
