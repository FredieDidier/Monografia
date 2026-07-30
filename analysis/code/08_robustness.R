# =============================================================================
# 08_robustness.R
#
#   (a) Prime-age (25-54) sample.
#   (b) Unweighted estimates.
#   (c) Alternative base quarter for the event study.
#   (d) A complementary margin: transitions into informal employment.
#   (e) Numerical validation of the closed-form margins against
#       marginaleffects::avg_predictions() on a random subsample.
#
# Output: Table A.6 and a validation log.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

setFixest_nthreads(max(1L, parallel::detectCores() - 2L))

d <- as.data.table(read_parquet(ANALYSIS_PQ))
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

run <- function(tag, dd, ...) {
  f <- file.path(DIR_EST, sprintf("rob_%s.rds", tag))
  if (file.exists(f)) { msg("  cached: ", tag); return(readRDS(f)) }
  msg("  fitting ", tag, " (", fmt_n(nrow(dd)), " obs) ...")
  t0 <- Sys.time()
  m  <- fit_es(dd, ...)
  pm <- period_margins(m, dd, PERIODS, ...)
  msg("    done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
  out <- list(tag = tag, n = nrow(dd), pm = pm)
  saveRDS(out, f, compress = FALSE)
  out
}

specs <- list()

# (a) baseline, for reference in the same table
specs$baseline <- run("baseline", d)

# (b) prime age
specs$prime <- run("prime_age",
                   d[age >= AGE_PRIME[1] & age <= AGE_PRIME[2]])

# (c) unweighted -----------------------------------------------------------
f_unw <- file.path(DIR_EST, "rob_unweighted.rds")
if (file.exists(f_unw)) {
  specs$unweighted <- readRDS(f_unw)
} else {
  msg("  fitting unweighted ...")
  du <- copy(d)[, w := 1]
  m  <- fit_es(du, cluster = ~ psu + qtr)
  specs$unweighted <- list(tag = "unweighted", n = nrow(du),
                           pm = period_margins(m, du, PERIODS))
  saveRDS(specs$unweighted, f_unw, compress = FALSE)
  rm(du, m); invisible(gc())
}

# (d) alternative base quarter --------------------------------------------
f_ref <- file.path(DIR_EST, "rob_ref2019q1.rds")
if (file.exists(f_ref)) {
  specs$ref2019q1 <- readRDS(f_ref)
} else {
  msg("  fitting with 2019Q1 as the base quarter ...")
  m <- fit_es(d, ref = 20191L)
  specs$ref2019q1 <- list(tag = "ref2019q1", n = nrow(d),
                          pm = period_margins(m, d, PERIODS, ref = 20191L))
  saveRDS(specs$ref2019q1, f_ref, compress = FALSE)
  rm(m); invisible(gc())
}

# (e) transitions into informal employment --------------------------------
specs$to_informal <- run("to_informal", d, outcome = "exit_to_informal")

LABS <- c(baseline    = "Baseline",
          prime       = "Prime age 25--54",
          unweighted  = "Unweighted",
          ref2019q1   = "Base quarter 2019Q1",
          to_informal = "Outcome: informal employment in $t+1$")

allpm <- rbindlist(lapply(names(specs), function(k)
  cbind(spec = LABS[[k]], n_spec = specs[[k]]$n, specs[[k]]$pm)), fill = TRUE)
fwrite(allpm, file.path(DIR_EST, "robustness_period_margins.csv"))

wide <- dcast(allpm, spec + n_spec ~ period, value.var = c("gap", "gap_se"))
pn <- names(PERIODS)
wide <- wide[match(unname(LABS), spec)]

rows <- unlist(lapply(seq_len(nrow(wide)), function(i) {
  r <- wide[i]
  est <- vapply(pn, function(p) r[[paste0("gap_", p)]],    numeric(1))
  se  <- vapply(pn, function(p) r[[paste0("gap_se_", p)]], numeric(1))
  c(sprintf("%s & %s & %s \\\\", r$spec,
            paste(fmt_est(est, se), collapse = " & "), fmt_n(r$n_spec)),
    sprintf(" & %s & \\\\", paste(fmt_se(se), collapse = " & ")))
}))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Robustness of the adjusted college gap}",
  "\\label{tab:robustness}",
  "\\begin{threeparttable}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lccccr}",
  "\\toprule",
  "Specification & Pre-pandemic & Onset & Mid-pandemic & Post-pandemic & Obs. \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Each row reports survey-weighted average predictive ",
         "differences in the outcome between workers with and without a college ",
         "degree, from the full specification of equation~(\\ref{eq:es}). The ",
         "last row replaces the outcome by an indicator for being informally ",
         "employed in $t+1$ -- a destination state, not a transition, so it ",
         "includes workers who were already informal in $t$. Standard ",
         "errors in parentheses are two-way clustered by primary sampling unit and ",
         "year-quarter. Stars denote $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
         PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_robustness.tex"))

# -----------------------------------------------------------------------------
# (f) Validation of the closed-form margins
# -----------------------------------------------------------------------------
# adjusted_margins() computes the average predictive margins analytically. This
# checks it against marginaleffects on a subsample small enough for the generic
# routine to run.
msg("validating margins against marginaleffects ...")
val_msg <- tryCatch({
  suppressPackageStartupMessages(library(marginaleffects))
  set.seed(SEED)
  val_sub <- d[sample(.N, 300000L)]
  ms <- fit_es(val_sub)
  am <- adjusted_margins(ms, val_sub)
  ap <- as.data.table(avg_predictions(ms, by = c("qtr", "college"),
                                      variables = list(college = 0:1), wts = "w"))
  cm <- merge(am[, .(qtr, m_nocol, m_col)],
              dcast(ap, qtr ~ college, value.var = "estimate"), by = "qtr")
  setnames(cm, c("0", "1"), c("me_nocol", "me_col"))
  sprintf(paste0("Maximum absolute discrepancy between the closed-form margins ",
                 "and marginaleffects::avg_predictions() over %d quarters:\n",
                 "  no college: %.3e\n  college   : %.3e"),
          nrow(cm), max(abs(cm$m_nocol - cm$me_nocol)),
          max(abs(cm$m_col - cm$me_col)))
}, error = function(e) paste("validation could not be run:", conditionMessage(e)))

writeLines(c(val_msg, "",
  "The closed-form result used throughout the paper is",
  "    m_gq = ybar_q + (g - p_q) * Delta_q,",
  "which follows from the WLS normal equations because quarter is among the",
  "absorbed fixed effects, so weighted residuals sum to zero within a quarter."),
  file.path(DIR_LOGS, "08_margins_validation.txt"))
cat(val_msg, "\n")

msg("08_robustness.R done.")
