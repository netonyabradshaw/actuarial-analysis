library(DBI)
library(RPostgres)
library(tidyverse)

setwd("/Users/owner/northgate-auto")

# --- CONNECTION --------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "northgate",
  host     = "localhost",
  port     = 5434,
  user     = "northgate",
  password = "northgate123"
)
cl_results <- read_csv("output/chain_ladder_results.csv")
trend_summary <- read_csv("output/trend_summary.csv")

expenses <- dbGetQuery(con, "
  SELECT category, type, pct_provision, dollar_provision
  FROM expenses
")
dbDisconnect(con)

print(cl_results)
print(trend_summary)
print(expenses)

# Pure premium trend factor
pp_trend_factor <- trend_summary %>%
  filter(metric == "Pure Premium") %>%
  pull(trend_factor)

# Premium trend factor
prem_trend_factor <- trend_summary %>%
  filter(metric == "Avg Premium") %>%
  pull(trend_factor)

# Total ultimate losses (chain-ladder)
total_ultimate <- sum(cl_results$cl_ultimate)

# Total earned cars
total_cars <- 40000 * 5

# Variable expense provision (commissions)
variable_expense <- expenses %>%
  filter(type == "Variable") %>%
  pull(pct_provision) %>%
  sum(na.rm = TRUE)

# Fixed expense provision per car
fixed_expense_per_car <- expenses %>%
  filter(type == "Fixed") %>%
  pull(dollar_provision) %>%
  sum(na.rm = TRUE)

cat("Pure premium trend factor:", pp_trend_factor, "\n")
cat("Premium trend factor:     ", prem_trend_factor, "\n")
cat("Total ultimate losses:    $", format(total_ultimate, big.mark=","), "\n")
cat("Total earned cars:        ", total_cars, "\n")
cat("Variable expense:         ", variable_expense, "\n")
cat("Fixed expense per car:    $", fixed_expense_per_car, "\n")


# --- INDICATION CALCULATION --------------------------------------
profit_provision <- 0.05

# Step 1: Historical pure premium (ultimate losses / earned cars)
historical_pp <- total_ultimate / total_cars

# Step 2: Projected pure premium (apply trend factor)
projected_pp <- historical_pp * pp_trend_factor

# Step 3: Required premium
# Required = (projected PP + fixed expenses) / (1 - variable expenses - profit)
required_premium <- (projected_pp + fixed_expense_per_car) / 
  (1 - variable_expense - profit_provision)

# Step 4: Expected future premium (latest year premium * premium trend factor)
latest_premium <- cl_results %>%
  filter(accident_year == 2024) %>%
  mutate(avg_premium = earned_premium / 40000) %>%
  pull(avg_premium)

expected_future_premium <- latest_premium * prem_trend_factor

# Step 5: Indicated change
indicated_change <- (required_premium / expected_future_premium) - 1

cat("=== NORTHGATE INSURANCE — RATE INDICATION ===\n")
cat("Historical pure premium:     $", round(historical_pp, 2), "\n")
cat("Projected pure premium:      $", round(projected_pp, 2), "\n")
cat("Fixed expense per car:       $", fixed_expense_per_car, "\n")
cat("Variable expense provision:  ", variable_expense * 100, "%\n")
cat("Profit provision:            ", profit_provision * 100, "%\n")
cat("Required premium:            $", round(required_premium, 2), "\n")
cat("Expected future premium:     $", round(expected_future_premium, 2), "\n")
cat("------------------------------------------\n")
cat("INDICATED CHANGE:            ", round(indicated_change * 100, 1), "%\n")

indication_summary <- tibble(
  metric = c("Historical Pure Premium", "Projected Pure Premium",
             "Fixed Expense per Car", "Variable Expense Provision",
             "Profit Provision", "Required Premium",
             "Expected Future Premium", "Indicated Change"),
  value  = c(round(historical_pp, 2), round(projected_pp, 2),
             fixed_expense_per_car, variable_expense,
             profit_provision, round(required_premium, 2),
             round(expected_future_premium, 2), round(indicated_change, 4))
)

write_csv(indication_summary, 
          "/Users/owner/northgate-auto/output/indication_summary.csv")

cat("\nIndication complete. Results saved.\n")