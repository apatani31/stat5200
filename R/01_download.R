# 01_download.R
# Downloads raw data into data/raw/. Raw files are never edited by hand.
#   (a) Binance daily klines for USDT/local-currency pairs (public bulk files)
#   (b) Official USD exchange rates from Yahoo Finance
#   (c) IMF Crypto-based Parallel Exchange Rates (WP-CPER): see note below

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({
  library(quantmod)
})

# ---- (a) Binance klines --------------------------------------------------
# Source: https://data.binance.vision  (monthly zip files, no API key needed)
binance_cols <- c("open_time", "open", "high", "low", "close", "volume",
                  "close_time", "quote_volume", "n_trades",
                  "taker_buy_base", "taker_buy_quote", "ignore")

months <- seq(SAMPLE_START, SAMPLE_END, by = "month")

download_pair <- function(pair) {
  out <- lapply(months, function(m) {
    ym  <- format(m, "%Y-%m")
    url <- sprintf(
      "https://data.binance.vision/data/spot/monthly/klines/%s/1d/%s-1d-%s.zip",
      pair, pair, ym)
    tmp <- tempfile(fileext = ".zip")
    ok  <- tryCatch({
      download.file(url, tmp, quiet = TRUE, mode = "wb"); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) return(NULL)                     # pair not listed that month
    dt <- fread(unzip(tmp, exdir = tempdir()), header = FALSE)
    setnames(dt, binance_cols)
    dt[, pair := pair]
    dt
  })
  rbindlist(out)
}

binance <- rbindlist(lapply(COUNTRIES$binance, download_pair))
# Binance switched timestamps from milliseconds to microseconds in 2025.
binance[, open_time := as.numeric(open_time)]
binance[open_time > 1e14, open_time := open_time / 1000]
binance[, date := as.Date(as.POSIXct(open_time / 1000, origin = "1970-01-01",
                                     tz = "UTC"))]
fwrite(binance, file.path(PATH_RAW, "binance_usdt_klines.csv"))
message("Binance rows: ", nrow(binance))

# ---- (b) Official exchange rates ----------------------------------------
fx <- rbindlist(lapply(seq_len(nrow(COUNTRIES)), function(i) {
  x <- getSymbols(COUNTRIES$yahoo[i], src = "yahoo", from = SAMPLE_START,
                  to = SAMPLE_END, auto.assign = FALSE)
  data.table(date = index(x), iso3 = COUNTRIES$iso3[i],
             official_rate = as.numeric(Cl(x)))
}))
fwrite(fx, file.path(PATH_RAW, "official_fx_yahoo.csv"))
message("Official FX rows: ", nrow(fx))

# ---- (c) IMF WP-CPER -------------------------------------------------------
# Download manually (one time) from
#   https://data.imf.org/en/datasets/IMF.STA:WPCPER  ->  DOWNLOAD  ->  CSV
# and save it as data/raw/imf_wpcper.csv. The download date is recorded in
# README.md so the vintage is reproducible.
if (!file.exists(file.path(PATH_RAW, "imf_wpcper.csv"))) {
  warning("data/raw/imf_wpcper.csv not found. See note in 01_download.R.")
}
