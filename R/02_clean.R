# 02_clean.R
# Builds the analysis panels data/processed/panel_daily.csv and
# data/processed/panel_monthly.csv (monthly means plus the IMF BTC premium)
#   usdt_premium = log(USDT price in local currency / official USD rate)
#   A positive value means dollars bought through the stablecoin market cost
#   more than at the official rate: the "access premium" for dollars.

source(here::here("R", "00_config.R"))

binance <- fread(file.path(PATH_RAW, "binance_usdt_klines.csv"),
                 integer64 = "double")
fx      <- fread(file.path(PATH_RAW, "official_fx_yahoo.csv"))

# Prefer Refinitiv (R/01b_pull_refinitiv.py) when that file exists; Yahoo is
# the fallback so the pipeline still runs for anyone without a licence.
ref_file <- file.path(PATH_RAW, "refinitiv_fx.csv")
ref <- NULL
if (file.exists(ref_file)) {
  ref <- fread(ref_file)
  ref[, date := as.Date(date)]

  # Weekend rows are carry-overs (ARS1MNDF= outright fields are published on
  # weekends); drop them so all series are on trading days, then carry
  # forward in the panel like the official rate.
  n_wkend <- ref[!as.POSIXlt(date)$wday %in% 1:5, .N]
  ref <- ref[as.POSIXlt(date)$wday %in% 1:5]
  message("Refinitiv weekend rows dropped: ", n_wkend)

  # Bad-tick filter (settings in 00_config.R): set to NA any value whose log
  # deviation from its centred rolling median exceeds OUTLIER_THRESH. Flagged
  # rows are written out so the rule can be audited; raw data stay untouched.
  setorder(ref, iso3, series, date)
  ref[OUTLIER_SERIES, on = c("iso3", "series"),
      roll_med := frollapply(mid, OUTLIER_WINDOW, median, align = "center"),
      by = .EACHI]
  ref[, bad := !is.na(roll_med) & abs(log(mid / roll_med)) > OUTLIER_THRESH]
  fwrite(ref[bad == TRUE, .(iso3, series, ric, date, mid, roll_med)],
         file.path(PATH_PROCESSED, "refinitiv_outliers.csv"))
  message("Refinitiv values set to NA by outlier filter: ", ref[, sum(bad)])
  ref[bad == TRUE, mid := NA]

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
  if ("fwd1m" %in% extra) {
    # Applied after the carry-forward so the excluded window stays empty.
    panel[iso3 == "UKR" & date %between% UKR_FWD_EXCLUDE, fwd1m := NA]
    panel[, exp_dep_1m := log(fwd1m / official_rate)]
  }
  if ("parallel" %in% extra)
    panel[, parallel_gap := log(parallel / official_rate)]
}

panel <- panel[date >= SAMPLE_START & date <= SAMPLE_END &
               !is.na(usdt_premium)]
fwrite(panel, file.path(PATH_PROCESSED, "panel_daily.csv"))
message("Panel: ", nrow(panel), " country-days, ",
        uniqueN(panel$iso3), " countries")

# ---- Monthly panel with the IMF Bitcoin-based premium ----------------------
# The IMF series is monthly, so it is compared with monthly means of the
# daily variables rather than repeated over every day of the month (which
# would overstate the number of independent observations).
monthly_vars <- intersect(c("usdt_premium", "exp_dep_1m", "parallel_gap"),
                          names(panel))
monthly <- panel[, c(list(n_days = .N),
                     lapply(.SD, function(x) if (all(is.na(x))) NA_real_
                                             else mean(x, na.rm = TRUE))),
                 by = .(iso3, month = as.Date(format(date, "%Y-%m-01"))),
                 .SDcols = monthly_vars]

imf_file <- file.path(PATH_RAW, "imf_wpcper.csv")
if (file.exists(imf_file)) {
  imf <- fread(imf_file)
  # Wide IMF export (dataset version 6.0.0): one row per series, one column
  # per month named "2019-M01" etc. The ISO3 code is the second field of
  # SERIES_CODE (e.g. "ARS.ARG.BIT_SHD_PT.M"); BIT_SHD_PT is "Crypto-based
  # Parallel Exchange Rate Premium (in %)". COUNTRY holds names, not codes.
  imf[, c("iso3", "indicator") :=
        tstrsplit(SERIES_CODE, ".", fixed = TRUE)[2:3]]
  month_cols <- grep("^[0-9]{4}-M[0-9]{2}$", names(imf), value = TRUE)
  if (length(month_cols) == 0 || !"BIT_SHD_PT" %in% imf$indicator)
    stop("Unexpected IMF file layout; inspect names(imf)")
  imf <- melt(imf[indicator == "BIT_SHD_PT" & iso3 %in% COUNTRIES$iso3,
                  c("iso3", month_cols), with = FALSE],
              id.vars = "iso3", variable.name = "month",
              value.name = "btc_premium_pct", na.rm = TRUE,
              variable.factor = FALSE)
  imf[, month := as.Date(paste0(sub("-M", "-", month), "-01"))]
  # Same units as usdt_premium: log(local BTC price / (US BTC price * S)).
  imf[, btc_premium := log1p(btc_premium_pct / 100)]
  fwrite(imf, file.path(PATH_PROCESSED, "imf_btc_premium.csv"))
  message("IMF premium: ", nrow(imf), " country-months for ",
          paste(sort(unique(imf$iso3)), collapse = ", "))
  monthly <- merge(monthly, imf[, .(iso3, month, btc_premium)],
                   by = c("iso3", "month"), all.x = TRUE)
}

setorder(monthly, iso3, month)
fwrite(monthly, file.path(PATH_PROCESSED, "panel_monthly.csv"))
message("Monthly panel: ", nrow(monthly), " country-months")
