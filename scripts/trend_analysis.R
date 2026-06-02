library(DBI)
library(RPostgres)
library(tidyverse)
library(ggplot2)
library(broom)

setwd("/Users/owner/northgate-auto")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "northgate",
  host     = "localhost",
  port     = 5434,
  user     = "northgate",
  password = "northgate123"
)

trend_data <- dbGetQuery(con, "
  SELECT *
  FROM trend_data
  ORDER BY quarter
")

dbDisconnect(con)

head(trend_data)

freq_model <- lm(log(frequency) ~ time_index, data = trend_data)
summary(freq_model)
quarterly_trend <- -0.0029935
annual_trend <- (1 + quarterly_trend)^4 - 1
annual_trend
sev_model <- lm(log(severity) ~ time_index, data = trend_data)
summary(sev_model)
quarterly_sev <- 0.0215575
annual_sev <- (1 + quarterly_sev)^4 - 1
annual_sev
prem_model <- lm(log(avg_premium) ~ time_index, data = trend_data)
summary(prem_model)
quarterly_prem <- 0.0098940
annual_prem <- (1 + quarterly_prem)^4 - 1

annual_pure_prem <- (1 + annual_trend) * (1 + annual_sev) - 1

trend_period <- 4

freq_factor  <- (1 + annual_trend)^trend_period
sev_factor   <- (1 + annual_sev)^trend_period
pp_factor    <- (1 + annual_pure_prem)^trend_period
prem_factor  <- (1 + annual_prem)^trend_period

cat("Frequency trend factor: ", round(freq_factor, 4), "\n")
cat("Severity trend factor:  ", round(sev_factor, 4), "\n")
cat("Pure premium factor:    ", round(pp_factor, 4), "\n")

trend_summary <- tibble(
  metric         = c("Frequency", "Severity", "Pure Premium", "Avg Premium"),
  quarterly_coef = c(-0.0029935, 0.0215575, NA, 0.0098940),
  annual_trend   = c(annual_trend, annual_sev, annual_pure_prem, annual_prem),
  trend_period   = trend_period,
  trend_factor   = c(freq_factor, sev_factor, pp_factor, prem_factor)
) %>%
  mutate(
    annual_trend  = round(annual_trend, 4),
    trend_factor  = round(trend_factor, 4)
  )

cat("=== TREND SUMMARY ===\n")
print(trend_summary)

# --- CHARTS ------------------------------------------------------
# Severity trend chart
ggplot(trend_data, aes(x = as.Date(quarter), y = severity)) +
  geom_point(color = "#2E75B6", size = 2) +
  geom_smooth(method = "lm", color = "#1F4E79", se = TRUE) +
  labs(
    title    = "Northgate Insurance — Severity Trend",
    subtitle = "Ontario Personal Auto TPL | 2018Q1-2024Q4",
    x        = "Quarter",
    y        = "Average Severity ($)"
  ) +
  theme_minimal(base_size = 13)

ggsave("/Users/owner/northgate-auto/output/severity_trend.png", 
       width = 10, height = 6, dpi = 150)

# Frequency trend chart
ggplot(trend_data, aes(x = as.Date(quarter), y = frequency)) +
  geom_point(color = "#2E75B6", size = 2) +
  geom_smooth(method = "lm", color = "#1F4E79", se = TRUE) +
  labs(
    title    = "Northgate Insurance — Frequency Trend",
    subtitle = "Ontario Personal Auto TPL | 2018Q1-2024Q4",
    x        = "Quarter",
    y        = "Claim Frequency (Claims per Car)"
  ) +
  theme_minimal(base_size = 13)

ggsave("/Users/owner/northgate-auto/output/frequency_trend.png", 
       width = 10, height = 6, dpi = 150)

# Premium trend chart
ggplot(trend_data, aes(x = as.Date(quarter), y = avg_premium)) +
  geom_point(color = "#2E75B6", size = 2) +
  geom_smooth(method = "lm", color = "#1F4E79", se = TRUE) +
  labs(
    title    = "Northgate Insurance — Average Premium Trend",
    subtitle = "Ontario Personal Auto TPL | 2018Q1-2024Q4",
    x        = "Quarter",
    y        = "Average Premium ($)"
  ) +
  theme_minimal(base_size = 13)

ggsave("/Users/owner/northgate-auto/output/premium_trend.png",
       width = 10, height = 6, dpi = 150)

cat("\nDay 4 complete. Trend analysis and charts saved.\n")

