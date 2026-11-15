#!/bin/bash
# This script updates the application data by running run_etl.R

# Ensure the script runs from the correct directory
cd "$(dirname "$0")"

# Run the ETL script
Rscript run_etl.R

# Sync latest sensor readings to BigQuery
/home/dsl/miniforge3/bin/mamba run -n gemini-lab python sync_domoticz_to_bigquery.py
