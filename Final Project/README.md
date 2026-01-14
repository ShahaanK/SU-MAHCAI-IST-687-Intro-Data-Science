# IST 687 Final Project - Residential Energy Modeling + Shiny App

Live Shiny App: https://0lbx1g-ben-e.shinyapps.io/687projectfinal/

## Project Summary
This project analyzes residential electricity consumption using building characteristics and weather data, then delivers an interactive Shiny app to explore usage drivers and scenario impacts. The full methodology, results, and narrative are documented in `Final Project/687 Report.pdf`. Two analysis paths were developed in parallel (Approach 1 and Approach 2) and then summarized in the final report.

## Problem Statement
We aimed to understand and predict household energy usage and quantify how factors like weather, insulation, HVAC efficiency, and home size affect demand. This supports both prediction (expected usage given conditions) and interpretation (which drivers matter most).

## Data
The analysis integrates multiple data sources hosted as parquet/CSV files:
- Static home attributes (size, construction, insulation, HVAC, etc.)
- Hourly household electricity usage
- County-level hourly weather observations

The data is filtered to July 2018 and sampled using a stratified approach to preserve county representation.

## Approach (High Level)
- Data assembly: pulled static home data, hourly energy usage, and weather data; joined by home ID, county, and timestamp.
- Feature engineering: ordered categorical insulation fields, converted to numeric scales, and created interaction terms (e.g., temperature x insulation).
- Modeling: fit and compared multiple linear regression variants (baseline, filtered variables, insulation-focused, interaction models).
- Evaluation: used train/test splits and metrics like RMSE and MAE to compare models.
- Scenario analysis: simulated how changes in temperature, insulation, HVAC efficiency, and home size shift predicted demand.
- Visualization: produced plots and summaries that feed the Shiny app for interactive exploration.

## My Contribution
- Approach 2 was completed end-to-end by me, including data preparation, feature engineering, model building, evaluation, and the written narrative in `Final Project/Approach 2.Rmd`.

## Team Contribution
- The Shiny app (`Final Project/app.R`) was built collaboratively by the team.
- The final report (`Final Project/687 Report.pdf`) was written as a team effort.

## Repository Contents
- `Final Project/687 Report.pdf`: Full project report with methods, findings, and conclusions.
- `Final Project/Approach1.Rmd`: Team approach 1 analysis.
- `Final Project/Approach 2.Rmd`: My approach 2 analysis and write-up.
- `Final Project/app.R`: Shiny app for interactive exploration.

## Run the Shiny App Locally
1. Open `Final Project/app.R` in RStudio.
2. Set the working directory to `Final Project`.
3. Install required packages if needed (e.g., tidyverse, caret, kernlab, imputeTS, arrow).
4. Click Run App.
