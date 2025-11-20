#!/bin/bash
# This script renders the flexdashboard R Markdown file to index.html

# Ensure the script runs from the correct directory (gemini_upgrade)
cd "$(dirname "$0")"

# Render the R Markdown to HTML
# The output file will be index.html in the parent directory (repository root)
Rscript -e 'rmarkdown::render("app_flex.Rmd", output_file = "../index.html")'

echo "index.html generated successfully from app_flex.Rmd in repository root"
