library(dplyr)
library(lubridate)
library(scales)
library(feather)
library(ggplot2)
library(data.table)
library(IAPWS95)
library(tidyr)

# Source the functions script which contains plotting functions
source("/home/dsl/gemini/chrust_plus/gemini_upgrade/functions.R")

# Load the pre-processed data that run_etl.R generates
dt <- feather::read_feather("/home/dsl/gemini/chrust_plus/gemini_upgrade/ml_model_data_for_app.feather")

# Generate the plots using functions from functions.R
qp <- plot_energy_buffer(dt)
plot_temp <- plot_temperatures(dt)
plot_buffer <- plot_buffer_profile(dt)
krzywe <- plot_heating_curve(dt)