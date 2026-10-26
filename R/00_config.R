# 00_config.R
# Project-wide settings. Every other script sources this file first,
# so paths, sample window and country list are defined in one place.

suppressPackageStartupMessages({
  library(here)        # project-relative paths, no setwd()
  library(data.table)  # fast reading and reshaping
})

set.seed(5200)

# ---- Paths ---------------------------------------------------------------
PATH_RAW       <- here("data", "raw")
PATH_PROCESSED <- here("data", "processed")
PATH_TABLES    <- here("output", "tables")
PATH_FIGURES   <- here("output", "figures")
for (p in c(PATH_RAW, PATH_PROCESSED, PATH_TABLES, PATH_FIGURES)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ---- Sample --------------------------------------------------------------
SAMPLE_START <- as.Date("2021-01-01")
SAMPLE_END   <- as.Date("2026-09-30")

# Emerging-market currencies with a USDT/local-currency spot pair on Binance.
# iso3 is used to merge with the IMF crypto-premium dataset.
COUNTRIES <- data.table(
  iso3     = c("ARG",     "TUR",     "BRA",     "UKR"),
  currency = c("ARS",     "TRY",     "BRL",     "UAH"),
  binance  = c("USDTARS", "USDTTRY", "USDTBRL", "USDTUAH"),
  yahoo    = c("ARS=X",   "TRY=X",   "BRL=X",   "UAH=X")
)

# Event dates for the event-study extension (documented in README).
EVENTS <- data.table(
  iso3  = c("ARG", "ARG"),
  date  = as.Date(c("2023-12-13", "2025-04-14")),
  label = c("Official devaluation", "Easing of FX controls for individuals")
)
