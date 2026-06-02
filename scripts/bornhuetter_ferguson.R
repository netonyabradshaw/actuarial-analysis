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

# --- LATEST DIAGONAL ---------------------------------------------
latest_diagonal <- claims %>%
  group_by(accident_year) %>%
  filter(development_period == max(development_period)) %>%
  ungroup()

# --- CDF TABLE ---------------------------------------------------
cdf_table <- tibble(
  development_period = c(12, 24, 36, 48, 60),
  cdf  = c(1.5401, 1.1827, 1.0576, 1.0126, 1.0000),
  pct_unreported     = c(0.3507, 0.1545, 0.0545, 0.0124, 0.0000)
)

latest_diagonal <- latest_diagonal %>%
  left_join(cdf_table, by = "development_period")
# --- BF CALCULATION ----------------------------------------------
bf_results <- latest_diagonal %>%
  left_join(policies, by = "accident_year") %>%
  mutate(
    # Expected unreported = premium x expected LR x % unreported
    expected_unreported = earned_premium * expected_loss_ratio * pct_unreported,
    # BF ultimate = paid losses + expected unreported
    bf_ultimate         = round(paid_losses + expected_unreported, 0),
    # BF IBNR = BF ultimate - paid losses
    bf_ibnr             = round(bf_ultimate - paid_losses, 0),
    # BF ultimate loss ratio
    bf_ultimate_lr      = round(bf_ultimate / earned_premium, 3)
  )

cat("=== BORNHUETTER-FERGUSON RESULTS ===\n")
print(bf_results %>% select(accident_year, paid_losses, 
                            expected_unreported, bf_ultimate, 
                            bf_ibnr, bf_ultimate_lr))

cat("\nTotal BF IBNR: $", format(sum(bf_results$bf_ibnr), big.mark = ","), "\n")
print(latest_diagonal)
# --- COMPARISON: CHAIN-LADDER vs BF ------------------------------
cl_results <- read_csv("/Users/owner/northgate-auto/output/chain_ladder_results.csv")

comparison <- cl_results %>%
  select(accident_year, latest_paid, cl_ibnr, cl_ultimate_lr) %>%
  left_join(bf_results %>% select(accident_year, bf_ibnr, bf_ultimate_lr),
            by = "accident_year") %>%
  mutate(ibnr_difference = bf_ibnr - cl_ibnr)

cat("=== CHAIN-LADDER vs BORNHUETTER-FERGUSON ===\n")
print(comparison)

cat("\nTotal CL IBNR: $", format(sum(comparison$cl_ibnr), big.mark = ","), "\n")
cat("Total BF IBNR: $", format(sum(comparison$bf_ibnr), big.mark = ","), "\n")
cat("Difference:    $", format(sum(comparison$ibnr_difference), big.mark = ","), "\n")

# --- EXPORT ------------------------------------------------------
write_csv(bf_results,  "/Users/owner/northgate-auto/output/bf_results.csv")
write_csv(comparison,  "/Users/owner/northgate-auto/output/reserve_comparison.csv")

# --- CHART -------------------------------------------------------
comparison_long <- comparison %>%
  select(accident_year, cl_ibnr, bf_ibnr) %>%
  pivot_longer(cols = c(cl_ibnr, bf_ibnr),
               names_to  = "method",
               values_to = "ibnr") %>%
  mutate(method = case_when(
    method == "cl_ibnr" ~ "Chain-Ladder",
    method == "bf_ibnr" ~ "Bornhuetter-Ferguson"
  ))

ggplot(comparison_long, aes(x = factor(accident_year), 
                            y = ibnr / 1e6,
                            fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Chain-Ladder" = "#2E75B6", 
                               "Bornhuetter-Ferguson" = "#1F4E79")) +
  labs(
    title    = "Northgate Insurance — IBNR Comparison by Method",
    subtitle = "Ontario Personal Auto TPL | Accident Years 2020-2024",
    x        = "Accident Year",
    y        = "IBNR ($ Millions)",
    fill     = "Method"
  ) +
  theme_minimal(base_size = 13)

ggsave("/Users/owner/northgate-auto/output/ibnr_comparison.png", 
       width = 10, height = 6, dpi = 150)

cat("\nDay 3 complete. BF results and comparison chart saved.\n")