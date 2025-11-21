#!/bin/bash
# This script commits and pushes the locally generated index.html and app_flex.Rmd to GitHub.

# Navigate to the repository root
cd /home/dsl/gemini/chrust_plus/

# Force-add the generated index.html and its source Rmd.
# The --force flag ensures they are added even if ignored elsewhere.
git add --force index.html
git add gemini_upgrade/app_flex.Rmd

# Check if there are any changes to commit
if ! git diff --cached --exit-code --quiet; then
  # Commit the changes using your preferred timestamp format
  timestamp(){
     date +"%d.%m.%Y %H:%M"
  }
  git commit -m "Auto-update flexdashboard $(timestamp)"

  # Push the changes to main. This will trigger the GitHub Action.
  git push origin main
else
  echo "No changes detected in index.html or app_flex.Rmd. Skipping commit and push."
fi