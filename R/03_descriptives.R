# 03_descriptives.R
# Summary statistics (LaTeX table read by paper/proposal.tex), persistence
# and comparison statistics with LaTeX macros for the proposal text, and a
# time-series figure of the USDT premium by country.

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({
  library(ggplot2)
  library(tseries)   # adf.test
})

panel <- fread(file.path(PATH_PROCESSED, "panel_daily.csv"))
panel[, date := as.Date(date)]

# The IMF premium is monthly, so its statistics come from the monthly panel.
monthly <- fread(file.path(PATH_PROCESSED, "panel_monthly.csv"))

vars <- c(usdt_premium = "USDT premium (log pts)",
          official_dep = "Official depreciation (daily, log)",
          log_volume   = "log(1 + USDT volume)")
if ("exp_dep_1m" %in% names(panel)) {
  vars <- c(vars, exp_dep_1m = "1M forward-implied depreciation")
}
if ("parallel_gap" %in% names(panel)) {
  vars <- c(vars, parallel_gap = "Parallel-market gap (log pts)")
}

stat_row <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(N = 0))   # callers skip empty variables
  c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x),
    Median = median(x), Max = max(x))
}

rows <- list()
for (cc in COUNTRIES$iso3) {
  for (v in names(vars)) {
    s <- stat_row(panel[iso3 == cc, get(v)])
    if (s["N"] == 0) next
    rows[[length(rows) + 1]] <- data.table(Country = cc, Variable = vars[[v]],
                                           t(s))
  }
  if ("btc_premium" %in% names(monthly)) {
    s <- stat_row(monthly[iso3 == cc, btc_premium])
    if (s["N"] > 0)
      rows[[length(rows) + 1]] <- data.table(
        Country = cc, Variable = "BTC premium, IMF WP-CPER (monthly, log pts)",
        t(s))
  }
}
if ("btc_premium" %in% names(monthly)) {
  vars <- c(vars, btc_premium = "BTC premium, IMF WP-CPER (monthly, log pts)")
}
desc <- rbindlist(rows)
fwrite(desc, file.path(PATH_TABLES, "desc_stats.csv"))

# ---- LaTeX table (booktabs) ------------------------------------------------
# Used only for LaTeX output: a math minus sign instead of a hyphen.
fmt <- function(x) sub("^-", "$-$", formatC(x, format = "f", digits = 3))
body <- desc[, sprintf("%s & %s & %d & %s & %s & %s & %s & %s \\\\",
                       Country, Variable, as.integer(N), fmt(Mean), fmt(SD),
                       fmt(Min), fmt(Median), fmt(Max))]
tex <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         "Country & Variable & $N$ & Mean & SD & Min & Median & Max \\\\",
         "\\midrule", body, "\\bottomrule", "\\end{tabular}")
writeLines(tex, file.path(PATH_TABLES, "desc_stats.tex"))

# ---- Persistence and comparison statistics ---------------------------------
# AR(1) of the daily premium: OLS of p_t on p_{t-1}, using only pairs of
# consecutive calendar days so a missing day never links two distant dates.
ar1 <- function(d) {
  d <- d[order(date), .(date, y = usdt_premium)]
  d[, `:=`(y_lag = shift(y), gap = as.integer(date - shift(date)))]
  coef(lm(y ~ y_lag, data = d[gap == 1L]))[["y_lag"]]
}
safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  cor(x[ok], y[ok])
}

eda <- list()
add <- function(cc, stat, value) {
  eda[[length(eda) + 1]] <<- data.table(iso3 = cc, statistic = stat,
                                        value = as.character(value))
}
add("ALL", "ndays", nrow(panel))
add("ALL", "eventwindow", EVENT_WINDOW)
for (cc in COUNTRIES$iso3) {
  d <- panel[iso3 == cc][order(date)]
  if (nrow(d) == 0) next
  rho <- ar1(d)
  add(cc, "ndays",    nrow(d))
  add(cc, "start",    format(min(d$date)))
  add(cc, "end",      format(max(d$date)))
  add(cc, "sharepos", mean(d$usdt_premium > 0))
  add(cc, "arone",    rho)
  add(cc, "halflife", log(0.5) / log(rho))
  # tseries interpolates p-values from a table and truncates them to
  # [0.01, 0.99]; the truncation warning is expected and suppressed.
  add(cc, "adfp", suppressWarnings(adf.test(d$usdt_premium)$p.value))
  if ("exp_dep_1m" %in% names(d))
    add(cc, "excess", mean(d$usdt_premium - d$exp_dep_1m, na.rm = TRUE))
  if ("btc_premium" %in% names(monthly)) {
    m <- monthly[iso3 == cc & is.finite(usdt_premium) & is.finite(btc_premium)]
    add(cc, "nbtc",    nrow(m))
    add(cc, "corrbtc", safe_cor(m$usdt_premium, m$btc_premium))
  }
}

# Argentina: daily correlation with the CCL gap and pre/post means around
# each event date in 00_config.R (event day excluded).
if ("parallel_gap" %in% names(panel)) {
  d <- panel[iso3 == "ARG"]
  add("ARG", "corrpar", safe_cor(d$usdt_premium, d$parallel_gap))
}
# Macro suffixes per variable, e.g. \ARGdecpre, \ARGdecparpre, \ARGdecexppre.
ev_vars <- c(usdt_premium = "", parallel_gap = "par", exp_dep_1m = "exp")
ev_vars <- ev_vars[names(ev_vars) %in% names(panel)]
ev <- EVENTS[iso3 == "ARG"]
ev[, tag := tolower(month.abb[month(date)])]   # e.g. "dec", "apr"
if (anyDuplicated(ev$tag)) stop("Two ARG events share a month; rename tags")
for (i in seq_len(nrow(ev))) {
  d <- panel[iso3 == ev$iso3[i]]
  pre  <- d[date >= ev$date[i] - EVENT_WINDOW & date < ev$date[i]]
  post <- d[date > ev$date[i] & date <= ev$date[i] + EVENT_WINDOW]
  for (v in names(ev_vars)) {
    add("ARG", paste0(ev$tag[i], ev_vars[[v]], "pre"),
        mean(pre[[v]], na.rm = TRUE))
    add("ARG", paste0(ev$tag[i], ev_vars[[v]], "post"),
        mean(post[[v]], na.rm = TRUE))
  }
}
eda <- rbindlist(eda)
fwrite(eda, file.path(PATH_TABLES, "eda_stats.csv"))

# LaTeX macros: \<ISO3><statistic>, e.g. \ARGarone; \Ncountrydays and
# \Eventwindow for panel-wide values. Counts, half-lives and the window are
# whole numbers, other values 3 decimals.
macro_value <- function(stat, value) {
  if (stat %in% c("start", "end")) return(value)
  x <- as.numeric(value)
  if (!is.finite(x)) return("--")
  if (stat %in% c("ndays", "nbtc", "halflife", "eventwindow"))
    return(formatC(round(x), format = "d", big.mark = ","))
  fmt(x)
}
all_names <- c(ndays = "Ncountrydays", eventwindow = "Eventwindow")
macros <- eda[, .(name = ifelse(iso3 == "ALL", all_names[statistic],
                                paste0(iso3, statistic)),
                  value = mapply(macro_value, statistic, value))]
stopifnot(all(grepl("^[A-Za-z]+$", macros$name)))
writeLines(c("% Generated by R/03_descriptives.R; do not edit by hand.",
             sprintf("\\newcommand{\\%s}{%s}", macros$name, macros$value)),
           file.path(PATH_TABLES, "eda_macros.tex"))

# Compact version for the one-page proposal: one row per country,
# mean (SD) for each key variable, the number of USDT days, the daily AR(1)
# and the monthly correlation with the IMF BTC premium.
key <- intersect(c("usdt_premium", "btc_premium", "exp_dep_1m",
                   "parallel_gap"), names(vars))
cell <- function(cc, v) {
  r <- desc[Country == cc & Variable == vars[[v]]]
  if (nrow(r) == 0) return("--")
  sprintf("%s (%s)", fmt(r$Mean), fmt(r$SD))
}
eda_cell <- function(cc, stat) {
  x <- as.numeric(eda[iso3 == cc & statistic == stat, value])
  if (length(x) == 0 || !is.finite(x)) "--" else fmt(x)
}
rows_c <- vapply(COUNTRIES$iso3, function(cc) {
  n <- desc[Country == cc & Variable == vars[["usdt_premium"]], N]
  paste(c(cc, if (length(n)) as.integer(n) else "--",
          vapply(key, function(v) cell(cc, v), ""),
          eda_cell(cc, "arone"), eda_cell(cc, "corrbtc")), collapse = " & ")
}, "")
tex_compact <- c(sprintf("\\begin{tabular}{lr%scc}",
                         strrep("c", length(key))),
                 "\\toprule",
                 paste(c("Country", "$N$",
                         c(usdt_premium = "USDT prem.",
                           btc_premium  = "BTC prem.\\ (IMF)",
                           exp_dep_1m   = "1M exp.\\ dep.",
                           parallel_gap = "CCL gap")[key],
                         "AR(1)", "Corr.\\ w/ BTC"),
                       collapse = " & "),
                 "\\\\ \\midrule",
                 paste0(rows_c, " \\\\"),
                 "\\bottomrule", "\\end{tabular}")
writeLines(tex_compact, file.path(PATH_TABLES, "desc_stats_proposal.tex"))

# ---- Figure ----------------------------------------------------------------
p <- ggplot(panel, aes(date, usdt_premium)) +
  geom_line(linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(data = EVENTS, aes(xintercept = date), colour = "red",
             linetype = "dotted") +
  facet_wrap(~ iso3, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "log(USDT price / official rate)") +
  theme_minimal(base_size = 10)
ggsave(file.path(PATH_FIGURES, "usdt_premium_by_country.pdf"), p,
       width = 7, height = 4.5)

print(desc)
