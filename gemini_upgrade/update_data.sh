#!/bin/bash
# This script updates the application data by running run_etl.R

# Ensure the script runs from the correct directory
cd "$(dirname "$0")"

# Run the ETL script
Rscript run_etl.R
