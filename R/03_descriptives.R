# 03_descriptives.R
# Summary statistics (LaTeX table read by paper/proposal.tex) and a
# time-series figure of the USDT premium by country.

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({
  library(ggplot2)
})

panel <- fread(file.path(PATH_PROCESSED, "panel_daily.csv"))
panel[, date := as.Date(date)]

vars <- c(usdt_premium = "USDT premium (log pts)",
          official_dep = "Official depreciation (daily, log)",
          log_volume   = "log(1 + USDT volume)")
if ("btc_premium" %in% names(panel)) {
  vars <- c(vars, btc_premium = "BTC premium, IMF WP-CPER")
}
if ("exp_dep_1m" %in% names(panel)) {
  vars <- c(vars, exp_dep_1m = "1M forward-implied depreciation")
}
if ("parallel_gap" %in% names(panel)) {
  vars <- c(vars, parallel_gap = "Parallel-market gap (log pts)")
}

stat_row <- function(x) {
  x <- x[is.finite(x)]
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
}
desc <- rbindlist(rows)
fwrite(desc, file.path(PATH_TABLES, "desc_stats.csv"))

# ---- LaTeX table (booktabs) ------------------------------------------------
fmt <- function(x) formatC(x, format = "f", digits = 3)
body <- desc[, sprintf("%s & %s & %d & %s & %s & %s & %s & %s \\\\",
                       Country, Variable, as.integer(N), fmt(Mean), fmt(SD),
                       fmt(Min), fmt(Median), fmt(Max))]
tex <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         "Country & Variable & $N$ & Mean & SD & Min & Median & Max \\\\",
         "\\midrule", body, "\\bottomrule", "\\end{tabular}")
writeLines(tex, file.path(PATH_TABLES, "desc_stats.tex"))

# Compact version for the one-page proposal: one row per country,
# mean (SD) for each key variable, plus the number of USDT days.
key <- intersect(c("usdt_premium", "btc_premium", "exp_dep_1m",
                   "parallel_gap"), names(vars))
cell <- function(cc, v) {
  r <- desc[Country == cc & Variable == vars[[v]]]
  if (nrow(r) == 0) return("--")
  sprintf("%s (%s)", fmt(r$Mean), fmt(r$SD))
}
rows_c <- vapply(COUNTRIES$iso3, function(cc) {
  n <- desc[Country == cc & Variable == vars[["usdt_premium"]], N]
  paste(c(cc, if (length(n)) as.integer(n) else "--",
          vapply(key, function(v) cell(cc, v), "")), collapse = " & ")
}, "")
tex_compact <- c(sprintf("\\begin{tabular}{lr%s}", strrep("c", length(key))),
                 "\\toprule",
                 paste(c("Country", "$N$",
                         c(usdt_premium = "USDT premium",
                           btc_premium  = "BTC premium (IMF)",
                           exp_dep_1m   = "1M exp.\\ depreciation",
                           parallel_gap = "Parallel gap")[key]),
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
