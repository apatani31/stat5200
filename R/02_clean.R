# 02_clean.R
# Builds the analysis panel data/processed/panel_daily.csv
#   usdt_premium = log(USDT price in local currency / official USD rate)
#   A positive value means dollars bought through the stablecoin market cost
#   more than at the official rate: the "access premium" for dollars.

source(here::here("R", "00_config.R"))

binance <- fread(file.path(PATH_RAW, "binance_usdt_klines.csv"))
fx      <- fread(file.path(PATH_RAW, "official_fx_yahoo.csv"))

# Prefer Refinitiv (R/01b_pull_refinitiv.py) when that file exists; Yahoo is
# the fallback so the pipeline still runs for anyone without a licence.
ref_file <- file.path(PATH_RAW, "refinitiv_fx.csv")
ref <- NULL
if (file.exists(ref_file)) {
  ref <- fread(ref_file)
  ref[, date := as.Date(date)]
  ref <- dcast(ref, date + iso3 ~ series, value.var = "mid")
  fx  <- ref[!is.na(spot), .(date, iso3, official_rate = spot)]
  message("Official rates: Refinitiv")
} else {
  message("Official rates: Yahoo fallback")
}

binance <- merge(binance, COUNTRIES[, .(iso3, pair = binance)], by = "pair")
usdt <- binance[, .(date = as.Date(date), iso3,
                    usdt_close = close,
                    usdt_volume_usd = volume)]   # base asset is USDT

fx[, date := as.Date(date)]
panel <- merge(usdt, fx, by = c("date", "iso3"), all.x = TRUE)

# Official rates are not quoted on weekends: carry the last quote forward.
setorder(panel, iso3, date)
panel[, official_rate := nafill(official_rate, type = "locf"), by = iso3]

panel[, usdt_premium := log(usdt_close / official_rate)]
panel[, official_dep := c(NA, diff(log(official_rate))), by = iso3]
panel[, log_volume   := log1p(usdt_volume_usd)]

# Market-implied expected depreciation over one month, log(F_1M / S), and
# the traditional parallel-market gap for Argentina, when available.
if (!is.null(ref)) {
  extra <- intersect(c("fwd1m", "parallel"), names(ref))
  panel <- merge(panel, ref[, c("date", "iso3", extra), with = FALSE],
                 by = c("date", "iso3"), all.x = TRUE)
  panel[, (extra) := lapply(.SD, nafill, type = "locf"), by = iso3,
        .SDcols = extra]
  if ("fwd1m" %in% extra)
    panel[, exp_dep_1m := log(fwd1m / official_rate)]
  if ("parallel" %in% extra)
    panel[, parallel_gap := log(parallel / official_rate)]
}

# ---- Merge IMF Bitcoin-based premium (replication data) -------------------
imf_file <- file.path(PATH_RAW, "imf_wpcper.csv")
if (file.exists(imf_file)) {
  imf <- fread(imf_file)
  # SDMX-CSV exports name columns differently across vintages; map the
  # usual candidates to a common schema and stop loudly if none match.
  pick <- function(cands) {
    hit <- intersect(cands, names(imf))
    if (length(hit) == 0) stop("Column not found; inspect names(imf): ",
                               paste(names(imf), collapse = ", "))
    hit[1]
  }
  imf <- imf[, .(iso3 = get(pick(c("COUNTRY", "REF_AREA", "Country Code",
                                   "country"))),
                 date = get(pick(c("TIME_PERIOD", "Time Period", "date"))),
                 btc_premium = as.numeric(get(pick(c("OBS_VALUE", "Value",
                                                     "value")))))]
  imf[, date := as.Date(date)]
  fwrite(imf, file.path(PATH_PROCESSED, "imf_btc_premium.csv"))
  panel <- merge(panel, imf, by = c("iso3", "date"), all.x = TRUE)
}

panel <- panel[date >= SAMPLE_START & date <= SAMPLE_END &
               !is.na(usdt_premium)]
fwrite(panel, file.path(PATH_PROCESSED, "panel_daily.csv"))
message("Panel: ", nrow(panel), " country-days, ",
        uniqueN(panel$iso3), " countries")
