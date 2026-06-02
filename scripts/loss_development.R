setwd("/Users/owner/northgate-auto")
library(DBI)
library(RPostgres)
library(tidyverse)
library(ggplot2)

# =============================================================
# NORTHGATE INSURANCE — ONTARIO PERSONAL AUTO
# Script 2: Loss Development Triangle & Chain-Ladder Reserving
# =============================================================

# --- CONNECTION --------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "northgate",
  host     = "localhost",
  port     = 5434,
  user     = "northgate",
  password = "northgate123"
)

# --- 1. PULL TRIANGLE DATA FROM POSTGRESQL ----------------------
claims <- dbGetQuery(con, "
  SELECT accident_year, development_period, paid_losses
  FROM claims
  ORDER BY accident_year, development_period
")

policies <- dbGetQuery(con, "
  SELECT accident_year, earned_premium, expected_loss_ratio
  FROM policies
  ORDER BY accident_year
")

dbDisconnect(con)

# --- 2. BUILD THE LOSS DEVELOPMENT TRIANGLE ----------------------
# Pivot to wide format: rows = accident years, columns = dev periods
triangle <- claims %>%
  pivot_wider(
    names_from  = development_period,
    values_from = paid_losses,
    names_prefix = "dev_"
  ) %>%
  arrange(accident_year)

cat("=== PAID LOSS DEVELOPMENT TRIANGLE ===\n")
print(triangle)

# --- 3. CALCULATE AGE-TO-AGE FACTORS ----------------------------
# For each development period transition, calculate factor = next / current
dev_periods <- c(12, 24, 36, 48, 60)
transitions <- c("12-24", "24-36", "36-48", "48-60")

# Raw factors for each accident year and transition
raw_factors <- matrix(NA, nrow = 5, ncol = 4,
                      dimnames = list(2020:2024, transitions))

for (i in 1:5) {
  for (j in 1:4) {
    curr_col <- paste0("dev_", dev_periods[j])
    next_col <- paste0("dev_", dev_periods[j + 1])
    if (!is.na(triangle[[curr_col]][i]) & !is.na(triangle[[next_col]][i])) {
      raw_factors[i, j] <- triangle[[next_col]][i] / triangle[[curr_col]][i]
    }
  }
}

cat("\n=== RAW AGE-TO-AGE FACTORS ===\n")
print(round(raw_factors, 4))

# --- 4. SELECT LINK RATIOS ---------------------------------------
# Volume-weighted average: sum of numerators / sum of denominators
# Most reliable method — weights larger accident years more heavily

vw_factors <- numeric(4)
for (j in 1:4) {
  curr_col <- paste0("dev_", dev_periods[j])
  next_col <- paste0("dev_", dev_periods[j + 1])
  
  numerator   <- sum(triangle[[next_col]], na.rm = TRUE)
  denominator <- sum(triangle[[curr_col]][!is.na(triangle[[next_col]])],
                     na.rm = TRUE)
  vw_factors[j] <- numerator / denominator
}

# Simple average for comparison
simple_avg <- colMeans(raw_factors, na.rm = TRUE)

# Selected factors — use volume weighted
# Tail factor = 1.000 at 60 months (Ontario auto TPL fully developed)
selected_factors <- vw_factors

factor_summary <- tibble(
  transition      = transitions,
  vol_weighted    = round(vw_factors, 4),
  simple_avg      = round(simple_avg, 4),
  selected        = round(selected_factors, 4)
)

cat("\n=== FACTOR SELECTION SUMMARY ===\n")
print(factor_summary)

# --- 5. CALCULATE CUMULATIVE FACTORS (CDF) -----------------------
# CDF = product of all factors from current dev period to ultimate
n_trans <- length(selected_factors)
cdf <- numeric(5)
cdf[5] <- 1.000  # tail factor at 60 months
for (k in (n_trans):1) {
  cdf[k] <- cdf[k + 1] * selected_factors[k]
}

cdf_summary <- tibble(
  dev_period = dev_periods,
  cdf        = round(cdf, 4),
  pct_unreported = round(1 - 1/cdf, 4)
)

cat("\n=== CUMULATIVE DEVELOPMENT FACTORS (CDF TO ULTIMATE) ===\n")
print(cdf_summary)

# --- 6. CHAIN-LADDER IBNR CALCULATION ---------------------------
# Ultimate = Latest Paid * CDF
# IBNR = Ultimate - Latest Paid

latest_diagonal <- claims %>%
  group_by(accident_year) %>%
  filter(development_period == max(development_period)) %>%
  ungroup() %>%
  rename(latest_paid = paid_losses,
         latest_dev  = development_period)

cl_results <- latest_diagonal %>%
  left_join(cdf_summary %>% rename(latest_dev = dev_period),
            by = "latest_dev") %>%
  left_join(policies %>% select(accident_year, earned_premium),
            by = "accident_year") %>%
  mutate(
    cl_ultimate    = round(latest_paid * cdf, 0),
    cl_ibnr        = round(cl_ultimate - latest_paid, 0),
    cl_ultimate_lr = round(cl_ultimate / earned_premium, 3)
  )

cat("\n=== CHAIN-LADDER RESULTS ===\n")
print(cl_results %>% select(accident_year, latest_dev, latest_paid,
                            cdf, cl_ultimate, cl_ibnr, cl_ultimate_lr))

cat("\nTotal Chain-Ladder IBNR: $",
    format(sum(cl_results$cl_ibnr), big.mark = ","), "\n")

# --- 7. EXPORT OUTPUTS -------------------------------------------
dir.create("output", showWarnings = FALSE)

write_csv(triangle,      "output/loss_triangle.csv")
write_csv(factor_summary,"output/factor_selection.csv")
write_csv(cdf_summary,   "output/cdf_summary.csv")
write_csv(cl_results,    "output/chain_ladder_results.csv")


# --- 8. VISUALIZATIONS ------------------------------------------
# Development pattern chart
claims_plot <- claims %>%
  left_join(policies %>% select(accident_year, earned_premium),
            by = "accident_year") %>%
  mutate(paid_lr = paid_losses / earned_premium)

ggplot(claims_plot, aes(x = development_period, y = paid_lr,
                        color = factor(accident_year),
                        group = factor(accident_year))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = dev_periods) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Blues", direction = 1) +
  labs(
    title    = "Northgate Insurance — Paid Loss Development Patterns",
    subtitle = "Ontario Personal Auto TPL | Accident Years 2020-2024",
    x        = "Development Period (Months)",
    y        = "Paid Loss Ratio",
    color    = "Accident Year"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/development_patterns.png", width = 10, height = 6, dpi = 150)

cat("\nDay 2 complete. Outputs saved to /output folder.\n")
