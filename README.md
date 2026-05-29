# Northgate Insurance — Ontario Personal Auto Actuarial Analysis

## Project Overview
An actuarial rate indication and loss reserve analysis for a fictional Ontario 
personal auto insurer, built in R, PostgreSQL, and Excel.

## Status
🔄 In Progress — Started May 29, 2026

## Tools
- **PostgreSQL** — raw data storage and querying
- **R** — actuarial calculations, trending, reserving
- **Excel** — final professional dashboard and indication

## Scripts (run in order)
1. `scripts/simulate_data.R` — generates and loads all data into PostgreSQL
2. `scripts/loss_development.R` — chain-ladder and BF reserve estimates
3. `scripts/trend_analysis.R` — frequency, severity, premium trend regressions
4. `scripts/indication.R` — final rate indication calculation

## Coverage
Ontario Third Party Liability | Accident Years 2020–2024 | 40,000 insured vehicles