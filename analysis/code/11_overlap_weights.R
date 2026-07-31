# =============================================================================
# 11_overlap_weights.R
#
# The reversal appears only after conditioning on job characteristics, so the
# obvious worry is that it is produced by extrapolation: if graduates and
# non-graduates barely overlap on those characteristics, a linear model will
# happily report a contrast in a region where one of the two groups is hardly
# observed.
#
# This script prices that in. For each quarter it estimates the propensity to
# hold a degree given the same covariates the specification conditions on, and
# then re-estimates the event study under overlap weights,
#
#     omega_i = w_i * (1 - e_i)   if college,      omega_i = w_i * e_i   if not,
#
# which put the most weight where the two groups actually coexist and none where
# a cell is effectively single-group. Fitting the propensity quarter by quarter
# lets every coefficient move over time, not only the intercept.
#
# Outputs
#   estimates/overlap_diagnostics.csv   support and effective sample size
#   estimates/overlap_margins.csv       gaps under overlap weights
#   tables/tab_overlap.tex              the exhibit
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

d_all <- as.data.table(read_parquet(ANALYSIS_PQ))
SEGMENTS <- list(all = NULL, informal = quote(formal == 0L))
SEG_LAB  <- c(all = "All employment", informal = "Informal employment")
seg_out  <- list(); seg_diag <- list()

for (SEG in names(SEGMENTS)) {
d <- if (is.null(SEGMENTS[[SEG]])) copy(d_all) else d_all[eval(SEGMENTS[[SEG]])]
msg("=== segment: ", SEG, " -- ", fmt_n(nrow(d)), " observations")

# -----------------------------------------------------------------------------
# Propensity score, quarter by quarter
# -----------------------------------------------------------------------------
# feglm absorbs state, occupation and sector rather than expanding them into
# dummies; a plain glm on ~150,000 rows with those factors, 52 times over, is
# the difference between minutes and hours.
PS_RHS <- c("age", "age_sq", "female", "hours", "log_income", "income_missing",
            "unpaid_family", "formal", "temporary", "social_security")
PS_FE  <- c("state", "urban", "race5", "tenure", "occupation", "sector")
ps_fml <- stats::as.formula(sprintf("college ~ %s | %s",
                                    paste(PS_RHS, collapse = " + "),
                                    paste(PS_FE,  collapse = " + ")))

d[, age_sq := age^2]
qs <- sort(unique(d$qtr))

d[, ps := NA_real_]
for (q in qs) {
  idx <- d[, which(qtr == q)]
  m <- try(feglm(ps_fml, data = d[idx], family = binomial(), weights = ~ w,
                 warn = FALSE, notes = FALSE), silent = TRUE)
  if (inherits(m, "try-error")) {
    warning("propensity failed in ", qlab(q), ": ", conditionMessage(attr(m, "condition")))
    next
  }
  # Observations dropped by separation keep an NA score and are reported below
  # rather than being quietly assigned one.
  fit <- stats::predict(m, type = "response")
  d[idx[obs(m)], ps := fit]
  if (match(q, qs) %% 10 == 0) msg("  ", qlab(q))
}

n_na <- d[is.na(ps), .N]
msg("  no propensity score (separation or missing covariate): ", fmt_n(n_na))

# -----------------------------------------------------------------------------
# Support diagnostics and overlap weights
# -----------------------------------------------------------------------------
dd <- d[!is.na(ps)]
dd[, overlap_w := w * fifelse(college == 1L, 1 - ps, ps)]

# Effective sample size under a weight vector: the Kish measure, which says how
# many equally weighted observations carry the same information.
ess <- function(x) sum(x)^2 / sum(x^2)

PERIODS <- list(
  "Pre-pandemic"  = qs[qs <= 20194L],
  "Onset"         = Q_ONSET,
  "Mid-pandemic"  = Q_MID,
  "Post-pandemic" = qs[qs >= 20214L]
)

diag <- rbindlist(lapply(names(PERIODS), function(nm) {
  z <- dd[qtr %in% PERIODS[[nm]]]
  data.table(period       = nm,
             n            = z[, .N],
             off_support  = z[, weighted.mean(ps < 0.02 | ps > 0.98, w)],
             ess_survey   = ess(z$w),
             ess_overlap  = ess(z$overlap_w),
             ess_ratio    = ess(z$overlap_w) / ess(z$w))
}))
print(diag)

# -----------------------------------------------------------------------------
# Re-estimate under overlap weights
# -----------------------------------------------------------------------------
# fit_es() weights by `w` by name, so the overlap weight is swapped into `w`
# on a copy rather than threading a weights argument through the helper.
dd[, w_survey := w][, w := overlap_w]

msg("re-estimating the event study under overlap weights ...")
m_ov <- fit_es(dd)
am_ov <- adjusted_margins(m_ov, dd)

pm_ov <- rbindlist(lapply(names(PERIODS), function(nm) {
  z <- am_ov[qtr %in% PERIODS[[nm]]]
  data.table(period = nm,
             gap    = weighted.mean(z$gap, z$n),
             gap_se = sqrt(mean(z$gap_se^2)))
}))

# The same periods under the survey weights, on the same rows, so the comparison
# is about the weighting and not about which observations survive scoring.
dd[, w := w_survey]
m_base  <- fit_es(dd)
am_base <- adjusted_margins(m_base, dd)
pm_base <- rbindlist(lapply(names(PERIODS), function(nm) {
  z <- am_base[qtr %in% PERIODS[[nm]]]
  data.table(period = nm,
             gap    = weighted.mean(z$gap, z$n),
             gap_se = sqrt(mean(z$gap_se^2)))
}))

# Trimming, as a third and more interpretable reading. Overlap weights change
# the estimand; dropping the tails of the propensity distribution keeps the
# survey-weighted estimand and only removes observations with no counterpart.
# If the reversal survives trimming but not overlap weighting, it lives inside
# the common-support region and is merely sensitive to how much weight that
# region carries -- a much milder statement than extrapolation.
TRIM <- c(0.10, 0.90)
dt_ <- dd[ps >= TRIM[1] & ps <= TRIM[2]]
msg("  trimmed to ps in [", TRIM[1], ", ", TRIM[2], "]: ",
    fmt_n(nrow(dt_)), " of ", fmt_n(nrow(dd)), " observations")
m_tr  <- fit_es(dt_)
am_tr <- adjusted_margins(m_tr, dt_)
pm_tr <- rbindlist(lapply(names(PERIODS), function(nm) {
  z <- am_tr[qtr %in% PERIODS[[nm]]]
  data.table(period = nm,
             gap    = if (nrow(z)) weighted.mean(z$gap, z$n) else NA_real_,
             gap_se = if (nrow(z)) sqrt(mean(z$gap_se^2)) else NA_real_)
}))

out <- merge(pm_base[, .(period, gap_base = gap, se_base = gap_se)],
             pm_ov  [, .(period, gap_ov   = gap, se_ov   = gap_se)],
             by = "period", sort = FALSE)
out <- merge(out, pm_tr[, .(period, gap_trim = gap, se_trim = gap_se)],
             by = "period", sort = FALSE)
out[, trim_share := nrow(dt_) / nrow(dd)]
out[, shift := gap_ov - gap_base]
out[, segment := SEG]; diag[, segment := SEG]
seg_out[[SEG]] <- out; seg_diag[[SEG]] <- diag
print(out)
}

out  <- rbindlist(seg_out)
diag <- rbindlist(seg_diag)
fwrite(out,  file.path(DIR_EST, "overlap_margins.csv"))
fwrite(diag, file.path(DIR_EST, "overlap_diagnostics.csv"))

# -----------------------------------------------------------------------------
# Table
# -----------------------------------------------------------------------------
f4 <- function(x) fmt_num(x, 4)
f1 <- function(x) fmt_num(x, 1)

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Common support and overlap-weighted estimates}",
  "\\label{tab:overlap}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Support} & \\multicolumn{3}{c}{Adjusted gap} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}",
  "Period & Off support (\\%) & ESS ratio (\\%) & Survey w. & Trimmed & Overlap w. \\\\",
  "\\midrule",
  unlist(lapply(names(SEGMENTS), function(sg) c(
    sprintf("\\multicolumn{6}{l}{\\textit{%s}} \\\\", SEG_LAB[[sg]]),
    unlist(lapply(which(out$segment == sg), function(i) {
      g <- diag[period == out$period[i] & segment == sg]
      sprintf("\\quad %s & %s & %s & %s & %s & %s \\\\", out$period[i],
              f1(100 * g$off_support), f1(100 * g$ess_ratio),
              f4(out$gap_base[i]), f4(out$gap_trim[i]), f4(out$gap_ov[i]))
    })),
    if (sg != tail(names(SEGMENTS), 1)) "\\addlinespace" else NULL))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: The propensity to hold a college degree is estimated ",
         "separately in each quarter, on the covariates and fixed effects of ",
         "equation~\\eqref{eq:es}, so that every coefficient may move over time. ",
         "Overlap weights are $w_i(1-e_i)$ for graduates and $w_i e_i$ for ",
         "non-graduates, where $e_i$ is that propensity; they weight each cell ",
         "by how evenly the two education groups populate it and vanish where a ",
         "cell is effectively single-group. Off support is the survey-weighted ",
         "share of observations with $e_i$ outside $[0.02, 0.98]$. The ESS ratio ",
         "is the Kish effective sample size under overlap weights as a share of ",
         "the same measure under survey weights. The trimmed column keeps the ",
         "survey-weighted estimand and drops only observations whose propensity ",
         "falls outside $[0.10, 0.90]$, separating the removal of unsupported ",
         "comparisons from the re-centring of the estimand that the overlap ",
         "column performs. Both columns of gaps are ",
         "estimated on the observations that receive a ",
         "propensity score, so the two differ only in the weighting. The lower ",
         "block repeats the exercise inside informal employment, where the two ",
         "education groups overlap on characteristics that they do not overlap on ",
         "across the labour market as a whole."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_overlap.tex"))

msg("11_overlap_weights.R done.")
