library(dplyr)
library(lubridate)
library(scales)
library(feather)
library(ggplot2)
library(data.table)
library(IAPWS95)
library(tidyr)
library(pracma)
library(plotly)

process_data <- function(df_raw) {
  # Isolate temperature data and pivot
  temp_data_wide <- df_raw %>%
    select(date, name, Temperature) %>%
    group_by(date, name) %>%
    summarise(Temperature = mean(Temperature, na.rm = TRUE)) %>%
    pivot_wider(names_from = name, values_from = Temperature)

  # Get the other data, ensuring we only have one row per timestamp
  other_data <- df_raw %>%
    select(date, chill, rh, pressure, dew, setpoint) %>%
    group_by(date) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

  # Join the pivoted temperature data with the other data
  df_wide <- left_join(temp_data_wide, other_data, by = "date")

  # Define all expected columns (including those from initial rename in mqtt_update.R)
  all_expected_cols <- c("chill", "rh", "pressure", "dew", "setpoint", "date",
                         "temp_piec", "temp_co", "bufor_top", "bufor_mid1", 
                         "bufor_mid2", "bufor_bottom", "temp_home", "temp", "wind")

  # Ensure all expected columns are present, adding NA if missing
  for (col_name in all_expected_cols) {
    if (!(col_name %in% colnames(df_wide))) {
      df_wide[[col_name]] <- NA
    }
  }
  
  # Select and reorder columns to ensure consistency
  df_wide <- df_wide %>% select(all_of(all_expected_cols))

  df_wide$date <- as.POSIXct(df_wide$date)
  
  # Removed the problematic ifelse statement for temp as temp2 is not present
  # df$temp=ifelse(df$date < "2022-11-05 13:00" | df$date > "2024-01-06 00:00",df$temp, df$temp2)

  # Group and summarize for ML model
  dt <- df_wide %>% group_by(date = floor_date(date, unit="5 mins")) %>%
    summarize(
      temp = mean(temp,na.rm=T),
      temp_home = mean(temp_home,na.rm=T) ,
      temp_piec = mean(temp_piec,na.rm=T) ,
      temp_co = mean(temp_co,na.rm=T) ,
      bufor_top = mean(bufor_top,na.rm=T) ,
      bufor_mid1 = mean(bufor_mid1,na.rm=T) ,
      bufor_mid2 = mean(bufor_mid2,na.rm=T) ,
      bufor_bottom = mean(bufor_bottom,na.rm=T),
      # Also need to summarize chill, rh, pressure, dew, setpoint, wind if they are used later
      chill = mean(chill, na.rm = TRUE),
      rh = mean(rh, na.rm = TRUE),
      pressure = mean(pressure, na.rm = TRUE),
      dew = mean(dew, na.rm = TRUE),
      setpoint = mean(setpoint, na.rm = TRUE),
      wind = mean(wind, na.rm = TRUE)
      )
  
  # Remove rows with all NAs
  dt <- dt[rowSums(is.na(dt)) < (ncol(dt) - 1), ]
  
  # Calculate buffer energy and deltas
  dt$WABT <- (bufor_bottom * 52 + bufor_mid2*37 + bufor_mid1 * 34 + bufor_top * 46) / 169

dt$WABT <- ifelse(dt$WABT < 10 , NA, dt$WABT)
dt <- dt %>% filter(!is.na(WABT) & WABT > 10 )

dt$delta_WABT <- dt$WABT-dt$temp_co

dt$enth <- NA

  # If dt is empty, return early to prevent errors in the loop
  if (nrow(dt) == 0) {
    return(dt)
  }

for (i in c(1:nrow(dt))){
  if(!is.na(dt$WABT[i])) {
    dt$enth[i] <- hTp(273.15+dt$WABT[i], 0.1)
  }
}

dt$Q_buf <- (600 * dt$enth * 0.277 / 1000)  - 17.43

dt$deltaT = dt$temp_home - dt$temp
dt$temp_co_delta <- c(NA,diff(dt$temp_co))
dt$temp_home_delta <- c(NA,diff(dt$temp_home))
dt$Q_buf_delta <- c(NA,diff(dt$Q_buf))

# Identify sign changes in Q_buf_delta
dt <- dt %>%
  mutate(sign_change = sign(Q_buf_delta) != lag(sign(Q_buf_delta)))

# Create cycle groups
dt$cycle_group <- cumsum(ifelse(is.na(dt$sign_change), 0, dt$sign_change))

# Find peaks (end of charge) and valleys (end of discharge)
peaks <- dt %>%
  group_by(cycle_group) %>%
  filter(Q_buf_delta > 0) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(peak_time = date, peak_q = Q_buf, cycle_group)

valleys <- dt %>%
  group_by(cycle_group) %>%
  filter(Q_buf_delta < 0) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(valley_time = date, valley_q = Q_buf, cycle_group)

# Pair peaks with the next valley using a rolling join
cycles <- data.table::as.data.table(peaks)[, .SD[1], by = peak_time] # Ensure unique peaks
valleys_dt <- data.table::as.data.table(valleys)

cycles[, next_valley_time := valleys_dt[cycles, on = .(valley_time > peak_time), x.valley_time, mult = "first"]]
cycles <- cycles[!is.na(next_valley_time)]

# Calculate time_to_discharge and time_extended
dt <- dt %>%
  mutate(
    time_to_discharge = NA,
    time_extended = NA
  )

for (i in 1:nrow(cycles)) {
  charge_start <- if (i > 1) cycles$next_valley_time[i-1] else NA
  charge_end <- cycles$peak_time[i]
  discharge_end <- cycles$next_valley_time[i]
  
  # time_extended: from previous valley to peak
  if (!is.na(charge_start)) {
    dt$time_extended[dt$date >= charge_start & dt$date <= charge_end] <- 
      as.numeric(difftime(charge_end, charge_start, units = "hours"))
  }
  
  # time_to_discharge: from peak to next valley
  dt$time_to_discharge[dt$date >= charge_end & dt$date <= discharge_end] <- 
      as.numeric(difftime(discharge_end, charge_end, units = "hours"))
  }
  
  # Define thresholds and identify cycles
  stove_threshold <- 66
  radiator_threshold <- 28
  
  dt <- dt %>%
    mutate(
      heating_cycle = ifelse(temp_piec > stove_threshold, 1, 0),
      discharging_cycle = ifelse(temp_co > radiator_threshold, 1, 0)
    )

  # Calculate heat demand
  dt <- dt %>%
    mutate(heat_demand_kwh = ifelse(discharging_cycle == 1 & heating_cycle == 0, -Q_buf_delta, 0))
  
  # Create lagged features
  dt <- dt %>%
    mutate(
      temp_lag1 = lag(temp, 1),
      heat_demand_kwh_lag1 = lag(heat_demand_kwh, 1)
    )

  return(dt)
}

# Plotting functions (restored original versions)
plot_energy_buffer <- function(dt) {
  qp <- dt %>% filter(Q_buf > 0 & temp_co > 20) %>%  select(date,Q_buf,deltaT,temp_co, Q_buf_delta) %>% 
    ggplot(., aes(x=date)) +
    geom_point(aes(y=Q_buf,color=Q_buf_delta*12), size=2) +
    scale_color_viridis_c() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs(x="data",y="Energia bufora [kWh]")
  plotly::ggplotly(qp)
}

plot_temperatures <- function(dt) {
  plot_temp <-  
    dt %>% filter(temp > -50 & temp_home > -50 & temp_co > 20) %>%
    select(date,temp,temp_co,temp_home, temp_piec) %>% 
    reshape2::melt(id.vars=c("date"))  %>% 
    ggplot(., aes(x=date)) + 
    geom_point(aes(y=value, color=variable), size=1)+
    scale_color_viridis_d() +
    scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
    theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top") + 
    facet_grid(variable~., scales = "free_y")
  plotly::ggplotly(plot_temp)
}

plot_buffer_profile <- function(dt) {
  plot_buffer <-  
    dt %>% filter(bufor_mid2 > 20 & bufor_top > 20 & bufor_mid1 > 20 &
                                  bufor_bottom > 20) %>% 
    filter(date > (Sys.Date()-1)) %>%
    select(date,bufor_top,bufor_mid1,bufor_mid2,bufor_bottom, WABT) %>% 
    reshape2::melt(id.vars=c("date"))  %>% 
    ggplot(., aes(x=date)) + 
    geom_point(aes(y=value, color=variable), size=2)+
    scale_color_viridis_d() +
    scale_x_datetime(breaks = "4 hours", date_labels = "%F %H:%m") + 
    theme(axis.text.x = element_text(angle = 70, hjust = 1),legend.position = "top")
  plotly::ggplotly(plot_buffer)
}

plot_heating_curve <- function(dt) {
  krzywe<- dt %>% filter(temp_co > 0 & temp > -50 ) %>% 
    ggplot(., aes(x=temp,y=temp_co, color=deltaT/4.4)) + 
    geom_point() + scale_color_viridis_c() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))
  plotly::ggplotly(krzywe)
}
