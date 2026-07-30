# =============================================================================
# 07_attrition.R
#
# Comment 5: panel matching and attrition.
#
# The stage-3 build keeps every worker employed in quarter t, whether or not
# the matching algorithm finds them again in t+1. Retention is therefore
# directly observable: `matched_next` is one exactly when the algorithm links
# the worker into t+1, which is precisely the selection that generates the
# estimation sample. (The previous vintage discarded unmatched origins and
# forced a one-step-removed proxy; that workaround is no longer needed.)
#
#   (a) Retention by quarter, education and formality    -> Table A.4, figure
#   (b) Standardised differences, matched vs unmatched origins
#   (c) Inverse-retention-probability weights and re-estimation -> Table 5
#   (d) Breakdown calculation: how differently would unmatched workers have to
#       behave to overturn the mid-pandemic reversal?
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest); library(ggplot2)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

setFixest_nthreads(max(1L, parallel::detectCores() - 2L))

ORIGINS_PQ <- file.path(DIR_DATA, "analysis_origins.parquet")
d <- as.data.table(read_parquet(ORIGINS_PQ))
d[, age_sq := age^2]

QUARTERS <- sort(unique(d$qtr))
PERIODS <- list(
  "Pre-pandemic"  = QUARTERS[QUARTERS <= 20194L],
  "Onset"         = Q_ONSET,
  "Mid-pandemic"  = Q_MID,
  "Post-pandemic" = QUARTERS[QUARTERS >= 20214L]
)
# Full ranges are stated once, in each table's notes, instead of being repeated
# in every row label: with four periods and seven columns the long labels pushed
# these tables past the right margin.
PERIOD_NOTE <- sprintf(
  paste("Periods are pre-pandemic %s--2019Q4, onset 2020Q1, mid-pandemic",
        "2020Q2--2021Q3 and post-pandemic 2021Q4--%s."),
  qlab(min(QUARTERS)), qlab(max(QUARTERS)))
ptag <- function(q) {
  for (nm in names(PERIODS)) if (q %in% PERIODS[[nm]]) return(nm)
  NA_character_
}

# -----------------------------------------------------------------------------
# (a) Retention
# -----------------------------------------------------------------------------
# A household is interviewed five times, so a worker observed in the fifth
# interview cannot be matched forward: that is a scheduled panel exit, not
# attrition. PNADC's own interview counter (V1016) identifies those rows
# exactly, so nothing has to be inferred about panel position.
msg("origins by interview number:")
print(d[, .(n = .N, matched = round(mean(matched_next), 4)),
        by = interview][order(interview)])

ret <- d[interview < 5L]
ret[, period := vapply(qtr, ptag, character(1))]
msg("retention sample: ", fmt_n(nrow(ret)), " origins in interviews 1-4")
msg("overall t -> t+1 match rate: ",
    formatC(ret[, weighted.mean(matched_next, w)], format = "f", digits = 4))

by_qe <- ret[, .(retention = weighted.mean(matched_next, w), n = .N),
             by = .(qtr, college)][order(qtr, college)]
by_qf <- ret[, .(retention = weighted.mean(matched_next, w), n = .N),
             by = .(qtr, formal)][order(qtr, formal)]
fwrite(by_qe, file.path(DIR_EST, "retention_by_quarter_education.csv"))
fwrite(by_qf, file.path(DIR_EST, "retention_by_quarter_formality.csv"))

tab_ret <- ret[, .(
  retention_nocol  = weighted.mean(matched_next[college == 0L], w[college == 0L]),
  retention_col    = weighted.mean(matched_next[college == 1L], w[college == 1L]),
  retention_formal = weighted.mean(matched_next[formal  == 1L], w[formal  == 1L]),
  retention_inf    = weighted.mean(matched_next[formal  == 0L], w[formal  == 0L]),
  overall          = weighted.mean(matched_next, w),
  n = .N
), by = period]
tab_ret[, diff_educ := retention_col - retention_nocol]
tab_ret[, diff_form := retention_formal - retention_inf]
tab_ret <- tab_ret[match(names(PERIODS), period)]
fwrite(tab_ret, file.path(DIR_EST, "retention_by_period.csv"))

# -----------------------------------------------------------------------------
# (b) Standardised differences, matched vs unmatched
# -----------------------------------------------------------------------------
BALVARS <- c("college", "female", "nonwhite", "urban", "age", "hours",
             "log_income", "formal", "signed_card", "social_security",
             "temporary")
BAL_LAB <- c(college = "College degree", female = "Woman",
             nonwhite = "Non-White", urban = "Urban area",
             age = "Age", hours = "Usual weekly hours",
             log_income = "Log labour income", formal = "Formal employment",
             signed_card = "Signed work card",
             social_security = "Social security contributor",
             temporary = "Temporary job")

std_diff <- function(dd, v) {
  x1 <- dd[matched_next == 1L][[v]]; w1 <- dd[matched_next == 1L]$w
  x0 <- dd[matched_next == 0L][[v]]; w0 <- dd[matched_next == 0L]$w
  m1 <- weighted.mean(x1, w1); m0 <- weighted.mean(x0, w0)
  v1 <- weighted.mean((x1 - m1)^2, w1); v0 <- weighted.mean((x0 - m0)^2, w0)
  c(mean_ret = m1, mean_not = m0, std_diff = (m1 - m0) / sqrt((v1 + v0) / 2))
}

bal <- rbindlist(lapply(BALVARS, function(v)
  data.table(variable = v, t(std_diff(ret, v)))))
fwrite(bal, file.path(DIR_EST, "retention_balance.csv"))

# -----------------------------------------------------------------------------
# (c) Inverse-retention-probability weights
# -----------------------------------------------------------------------------
# The propensity is estimated quarter by quarter rather than as one pooled
# logit with education-by-quarter interactions.
#
# The pooled version was killed by the operating system: ~115 dense regressors
# on 9.2M rows made the IRLS working matrices exceed the machine's memory. The
# per-quarter version is also the more flexible specification -- it lets *every*
# coefficient, not just education, vary across quarters -- and it fits in
# seconds per quarter because each quarter holds about 177k rows.
f_ret <- file.path(DIR_EST, "m_retention.rds")
if (file.exists(f_ret)) {
  msg("loading cached retention propensities")
  pr <- readRDS(f_ret)
} else {
  msg("fitting retention model, one logit per quarter ...")
  t0 <- Sys.time()
  ret[, .row := .I]
  qs <- sort(unique(ret$qtr))
  pr <- rep(NA_real_, nrow(ret))
  for (q in qs) {
    idx <- ret[qtr == q, .row]
    mq <- feglm(
      matched_next ~ college + age + age_sq + female + nonwhite + urban +
        formal + signed_card + social_security + temporary + hours +
        log_income | state + occupation + sector,
      family = binomial(), data = ret[qtr == q], weights = ~ w,
      warn = FALSE, notes = FALSE
    )
    # Predicting on newdata returns a value for every row, including any the fit
    # dropped for separation or singleton fixed-effect cells. Whatever still
    # comes back missing falls back to the quarter's own retention rate.
    dq  <- ret[qtr == q]
    fit <- as.numeric(predict(mq, newdata = dq, type = "response"))
    fit[!is.finite(fit)] <- dq[, weighted.mean(matched_next, w)]
    pr[idx] <- fit
  }
  ret[, .row := NULL]
  msg("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1),
      " min over ", length(qs), " quarters")
  saveRDS(pr, f_ret, compress = FALSE)
}
stopifnot(length(pr) == nrow(ret))
ret[, p_retain := pmax(pr, 0.05)]
ret[, analysis_weight := w / p_retain]
msg("retention propensity: min ", round(min(ret$p_retain), 3),
    " median ", round(median(ret$p_retain), 3),
    " max ", round(max(ret$p_retain), 3))

# The event study runs on matched origins only: the outcome exists only there.
est <- ret[matched_next == 1L]

f_base <- file.path(DIR_EST, "m_event_study_retsample.rds")
if (file.exists(f_base)) {
  m_base <- readRDS(f_base)
} else {
  msg("event study on the retention subsample ...")
  m_base <- fit_es(est, cluster = ~ psu + qtr)
  saveRDS(m_base, f_base, compress = FALSE)
}

f_ipw <- file.path(DIR_EST, "m_event_study_ipw.rds")
if (file.exists(f_ipw)) {
  m_ipw <- readRDS(f_ipw)
} else {
  msg("event study with inverse-retention weights ...")
  m_ipw <- feols(es_formula("exit"), data = est,
                 weights = ~ analysis_weight, cluster = ~ psu + qtr,
                 mem.clean = TRUE)
  saveRDS(m_ipw, f_ipw, compress = FALSE)
}

pm_base <- period_margins(m_base, est, PERIODS)
est_ipw <- copy(est)[, w := analysis_weight]
pm_ipw  <- period_margins(m_ipw, est_ipw, PERIODS)

cmp <- merge(pm_base[, .(period, gap_base = gap, se_base = gap_se)],
             pm_ipw [, .(period, gap_ipw  = gap, se_ipw  = gap_se)],
             by = "period", sort = FALSE)
fwrite(cmp, file.path(DIR_EST, "attrition_ipw_comparison.csv"))

# -----------------------------------------------------------------------------
# (d) Breakdown calculation
# -----------------------------------------------------------------------------
# With rho the share of employed workers matched into t+1, and m_g, mtilde_g the
# exit rates of matched and unmatched workers in education group g,
#     Delta = rho * Delta_hat + (1 - rho) * (mtilde_C - mtilde_N).
# Setting Delta = 0 gives the differential the unmatched would have to display.
RHO <- ret[, weighted.mean(matched_next, w)]
mid_gap <- pm_base[grepl("^Mid", period), gap]
breakdown <- -RHO * mid_gap / (1 - RHO)

writeLines(c(
  sprintf("Overall t -> t+1 match rate (rho): %.4f", RHO),
  "Source: directly observed in analysis_origins.parquet (stage-3 build).",
  sprintf("Mid-pandemic adjusted gap among matched workers: %+.4f", mid_gap),
  sprintf("Unmatched-worker exit-rate differential needed to zero out the gap: %+.4f",
          breakdown),
  "",
  "Interpretation: to overturn the mid-pandemic reversal, unmatched college",
  "workers would have to exit employment by the amount above relative to",
  "unmatched non-college workers -- opposite in sign to what is observed among",
  "matched workers, and several times larger."
), file.path(DIR_LOGS, "07_attrition_breakdown.txt"))

# -----------------------------------------------------------------------------
# Tables
# -----------------------------------------------------------------------------
f3 <- function(x) fmt_num(x, 3)

rows_ret <- vapply(seq_len(nrow(tab_ret)), function(i) {
  z <- tab_ret[i]
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", z$period,
          f3(z$retention_nocol), f3(z$retention_col), f3(z$diff_educ),
          f3(z$retention_formal), f3(z$retention_inf), f3(z$diff_form))
}, character(1))

rows_bal <- vapply(seq_len(nrow(bal)), function(i) {
  z <- bal[i]
  sprintf("%s & %s & %s & %s & & & \\\\", BAL_LAB[[z$variable]],
          f3(z$mean_ret), f3(z$mean_not), f3(z$std_diff))
}, character(1))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Panel retention and selection into the matched sample}",
  "\\label{tab:attrition}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccccc}",
  "\\multicolumn{7}{l}{\\textit{Panel A. Share of employed workers matched into $t+1$}} \\\\",
  "\\toprule",
  "& \\multicolumn{3}{c}{By education} & \\multicolumn{3}{c}{By formality} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "Period & No college & College & Diff. & Formal & Informal & Diff. \\\\",
  "\\midrule",
  rows_ret,
  "\\bottomrule",
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\textit{Panel B. Matched vs.\\ unmatched origins, all quarters}} \\\\",
  "\\toprule",
  "& Matched & Unmatched & Std.\\ diff. & & & \\\\",
  "\\midrule",
  rows_bal,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: The sample is every worker employed at the interview date ",
         "in quarter $t$ who is not in the fifth and final interview of their ",
         "household's rotation, so that a further interview is scheduled. ",
         "Retention equals one when the stage-3 algorithm links the worker into ",
         "$t+1$. Panel B reports survey-weighted means and the standardised ",
         "difference, $(\\bar{x}_1-\\bar{x}_0)/\\sqrt{(s_1^2+s_0^2)/2}$. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_attrition.tex"))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{The adjusted college gap under inverse-retention-probability weighting}",
  "\\label{tab:ipw}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "& Survey weights & Survey $\\times$ inverse- & Difference \\\\",
  "& & retention weights & (2) $-$ (1) \\\\",
  "Period & (1) & (2) & (3) \\\\",
  "\\midrule",
  # Four decimals, not the three used elsewhere. Re-weighting moves the gap by
  # about 2e-4, so at three decimals the two columns print identically and the
  # table looks like a copy-paste error rather than the intended null result.
  unlist(lapply(seq_len(nrow(cmp)), function(i) {
    z <- cmp[i]
    c(sprintf("%s & %s & %s & %s \\\\", z$period,
              fmt_est(z$gap_base, z$se_base, 4), fmt_est(z$gap_ipw, z$se_ipw, 4),
              fmt_num(z$gap_ipw - z$gap_base, 4)),
      sprintf(" & %s & %s & \\\\", fmt_se(z$se_base, 4), fmt_se(z$se_ipw, 4)))
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Both columns are estimated on the matched origins of the ",
         "retention sample (", fmt_n(nrow(est)), " person-quarters) with the full ",
         "specification, so the two differ only in the weights. Column (2) ",
         "multiplies the survey weight by the inverse of a predicted probability ",
         "of being matched into $t+1$, trimmed at 0.05, from a separate logit ",
         "per year-quarter on education, demographics, job characteristics and ",
         "state, occupation and sector fixed effects. Column (3) reports the ",
         "difference between the two point estimates. Entries are given to four ",
         "decimals because re-weighting moves the gap by less than half a ",
         "thousandth. Standard errors are two-way ",
         "clustered by primary sampling unit and year-quarter. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_ipw.tex"))

# -----------------------------------------------------------------------------
# Figure
# -----------------------------------------------------------------------------
by_qe[, date := qdate(qtr)]
by_qe[, group := factor(college, levels = c(0, 1), labels = EDU_LEVELS)]

p_ret <- ggplot(by_qe, aes(date, retention, colour = group, linetype = group)) +
  pandemic_layers() +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = EDU_COLOURS) +
  scale_linetype_manual(values = EDU_LINES) +
  x_years(by_qe$date) + y_prob() +
  labs(x = NULL, y = "Share of employed workers matched into t+1") +
  theme_paper()

save_fig(p_ret, "fig_retention")

msg("07_attrition.R done.")
