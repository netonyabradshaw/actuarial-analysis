library(DBI)
library(RPostgres)
library(tidyverse)

# =============================================================
# NORTHGATE INSURANCE — ONTARIO PERSONAL AUTO
# Script 1: Data Simulation & Database Load
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

# --- 1. POLICIES TABLE -------------------------------------------
set.seed(42)

policies <- tibble(
  accident_year       = 2020:2024,
  car_count           = 40000,
  avg_premium         = 1850 * (1.04 ^ (0:4)),
  earned_premium      = car_count * avg_premium,
  expected_loss_ratio = 0.72
)

# --- 2. CLAIMS TABLE (LOSS DEVELOPMENT TRIANGLE) -----------------
# Development periods: 1
