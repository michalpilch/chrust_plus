library(dplyr)
library(feather)
library(ggplot2)
library(lubridate)

# Load the processed data from the specified file
data_file <- "/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data.feather"
if (!file.exists(data_file)) {
  stop(paste("Data file not found at:", data_file))
}
dt <- read_feather(data_file)

# --- 1. Aggregate to daily averages ---
daily_avg_data <- dt %>%
  mutate(day = floor_date(date, "day")) %>%
  group_by(day) %>%
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    avg_temp_co = mean(temp_co, na.rm = TRUE)
  ) %>%
  filter(
    !is.na(avg_temp) & !is.na(avg_temp_co),
    avg_temp_co > 28 # Filter for days where heating was likely active
  )

# --- 2. Filter for training data (up to 14°C) ---
training_data <- daily_avg_data %>%
  filter(avg_temp <= 14)

# Ensure there is data to train on
if (nrow(training_data) < 5) {
  stop("Not enough distinct heating days under 14°C to build a reliable model.")
}

# --- 3. Train a linear model on the filtered data ---
model <- lm(avg_temp_co ~ avg_temp, data = training_data)

# --- 4. Predict/Extrapolate for the full range ---
prediction_data <- data.frame(avg_temp = -20:20)
prediction_data$predicted_radiator_temp <- predict(model, newdata = prediction_data)

# --- 5. Create and Save the Plot ---
final_table <- prediction_data %>%
  rename(ambient_temperature_C = avg_temp,
         radiator_temperature_C = predicted_radiator_temp)

heating_curve_plot <- ggplot(final_table, aes(x = ambient_temperature_C, y = radiator_temperature_C)) +
  geom_line(color = "red", linewidth = 1.2) +
  # Also plot the raw daily average points to see the fit
  geom_point(data = daily_avg_data, aes(x = avg_temp, y = avg_temp_co), color = "grey", alpha = 0.6) +
  labs(
    title = "Linear Heating Curve (Extrapolated from data <= 14°C)",
    subtitle = "Red line is a linear model fit to data up to 14°C. Grey points are all daily averages.",
    x = "Average Daily Ambient Temperature (°C)",
    y = "Predicted Radiator Temperature (°C)"
  ) +
  theme_minimal() +
  # Set x-axis ticks to be every 1 degree
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 1)) +
  scale_y_continuous(breaks = seq(20, 70, by = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Rotate x-axis labels for clarity

# Save the plot as a PNG file
output_file <- "heating_curve.png"
ggsave(output_file, plot = heating_curve_plot, width = 11, height = 7, units = "in")

print(paste("Linear extrapolated heating curve plot saved to:", output_file))