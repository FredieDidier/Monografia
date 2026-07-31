# =============================================================================
# 12_tipping_point.R
#
# How differently would the workers we never see again have to behave before the
# mid-pandemic reversal disappeared?
#
# Section 6 answers the raw-rate version of this with a frontier. That frontier
# is an accounting identity, so it cannot speak to the covariate-adjusted
# contrast, which is the object the paper actually reports. This script closes
# that gap directly: it imputes an outcome for every unmatched origin under a
# stated departure from the matched workers of the same education group, and
# re-estimates the specification on the completed sample.
#
# The departure is a shift in log-odds. Writing p_i for the probability an
# unmatched worker would have exited, predicted from the matched workers of
# their own group,
#
#     logit(p_i^delta) = logit(p_i) + gamma_g ,     g in {college, non-college},
#
# so gamma_g = 0 is missing-at-random within group. Only the difference
# gamma_C - gamma_N moves the education gap, so we fix gamma_N = 0 and search
# over delta = gamma_C. The adjusted gap is monotone in delta, which is what
# makes a bisection legitimate rather than a grid.
#
# Runtime is dominated by the re-estimations, each a fixed-effects fit on the
# full origins file. The bisection needs roughly a dozen of them.
#
# Outputs
#   estimates/tipping_point.csv   the search path and the tipping value
#   tables/tab_tipping.tex        the exhibit
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

ORIGINS_PQ <- file.path(DIR_DATA, "analysis_origins.parquet")
stopifnot(file.exists(ORIGINS_PQ))
o <- as.data.table(read_parquet(ORIGINS_PQ))

# Interview 5 is a scheduled panel exit, not attrition: no t+1 exists inside the
# panel for those rows, so they are not part of the selection being probed.
o <- o[interview < 5L]
o[, age_sq := age^2]
msg("origins with a scheduled t+1: ", fmt_n(nrow(o)),
    " (unmatched: ", fmt_n(o[matched_next == 0L, .N]), ")")

# -----------------------------------------------------------------------------
# Predicted exit probability for the unmatched
# -----------------------------------------------------------------------------
# Fitted on matched workers, separately by education group, so the imputation
# inherits each group's own relationship between characteristics and exit rather
# than a pooled one. delta then measures departure from that, not from a common
# baseline.
IMP_RHS <- c("age", "age_sq", "female", "hours", "log_income", "income_missing",
             "unpaid_family", "formal", "temporary", "social_security")
IMP_FE  <- c("qtr", "state", "urban", "race5", "tenure", "occupation", "sector")
imp_fml <- stats::as.formula(sprintf("exit ~ %s | %s",
                                     paste(IMP_RHS, collapse = " + "),
                                     paste(IMP_FE,  collapse = " + ")))

o[, p_hat := NA_real_]
for (g in c(0L, 1L)) {
  msg("  imputation model, college = ", g)
  mg <- feglm(imp_fml, data = o[matched_next == 1L & college == g],
              family = binomial(), weights = ~ w, warn = FALSE, notes = FALSE)
  idx <- o[, which(matched_next == 0L & college == g)]
  o[idx, p_hat := stats::predict(mg, newdata = o[idx], type = "response")]
}
# Rows the imputation model cannot score (an unseen fixed-effect level) fall
# back to their group's matched exit rate; there are few and pretending
# otherwise would silently drop them.
fb <- o[matched_next == 1L, .(p_fb = weighted.mean(exit, w)), by = college]
o[fb, on = "college", p_hat := fifelse(is.na(p_hat), p_fb, p_hat)]
msg("  imputed ", fmt_n(o[matched_next == 0L, .N]), " unmatched outcomes")

# -----------------------------------------------------------------------------
# Adjusted mid-pandemic gap as a function of delta
# -----------------------------------------------------------------------------
logit  <- function(p) log(p / (1 - p))
expit  <- function(x) 1 / (1 + exp(-x))
EPS    <- 1e-6

gap_at <- function(delta) {
  z <- copy(o)
  # Expected outcomes, not simulated draws: the estimand is a weighted mean, so
  # imputing the probability removes Monte Carlo noise from the search without
  # changing what is being estimated.
  #
  # The shift is applied through a column, not a pre-sliced vector: inside a
  # data.table `j` the filter has already been applied, so indexing an outside
  # vector by the same condition misaligns it against the subset.
  z[, p_adj := pmin(pmax(p_hat, EPS), 1 - EPS)]
  # `exit` arrives as an integer column. Assigning a probability into it without
  # widening the type first truncates every imputed value to zero, silently and
  # identically for every delta -- the search then reports the same gap at every
  # point and concludes there is no tipping point.
  z[, exit := as.numeric(exit)]
  z[matched_next == 0L,
    exit := expit(logit(p_adj) + delta * college)]
  m  <- fit_es(z)
  am <- adjusted_margins(m, z)
  weighted.mean(am[qtr %in% Q_MID, gap], am[qtr %in% Q_MID, n])
}

# The bisection is ~7 fixed-effects fits on 9.2 million rows, so it is cached
# like the models in 03. Delete estimates/tipping_point.csv to force a re-search.
F_PATH <- file.path(DIR_EST, "tipping_point.csv")
CACHED <- file.exists(F_PATH)

path <- data.table(delta = numeric(), gap = numeric())
record <- function(delta) {
  g <- gap_at(delta)
  path <<- rbind(path, data.table(delta = delta, gap = g))
  msg("  delta = ", fmt_num(delta, 4), "  ->  mid gap = ", fmt_num(g, 5))
  g
}

if (CACHED) {
  msg("loading cached tipping-point search from ", basename(F_PATH))
  path <- fread(F_PATH)
  tipping <- if (min(path$gap) > 0) NA_real_ else
    stats::approx(path$gap, path$delta, xout = 0)$y
} else {
# Bracket first. A negative delta makes unmatched graduates exit less than
# missing-at-random implies, which is the direction that erodes the reversal.
g0 <- record(0)
if (g0 <= 0) {
  msg("the reversal is already absent under missing-at-random; no tipping point")
  tipping <- 0
} else {
  lo <- 0; hi <- -0.25
  while (record(hi) > 0 && hi > -8) { lo <- hi; hi <- hi * 2 }
  if (path[delta == hi, gap] > 0) {
    tipping <- NA_real_
    msg("no tipping point within delta > -8")
  } else {
    for (i in seq_len(12)) {                       # ~1e-4 on the log-odds scale
      mid <- (lo + hi) / 2
      if (record(mid) > 0) lo <- mid else hi <- mid
    }
    tipping <- (lo + hi) / 2
  }
}
}

setorder(path, -delta)
fwrite(path, file.path(DIR_EST, "tipping_point.csv"))

# When no root exists the informative number is the floor the gap approaches as
# the shift is pushed to its limit: what survives even if unmatched graduates
# are assumed never to leave employment. Defined here, with the path, because
# the table below reports it.
floor_gap <- path[which.min(delta), gap]

# What the tipping delta means in exit rates, which is the scale a reader can
# judge: the log-odds shift applied at the unmatched graduates' mean.
p_bar <- o[matched_next == 0L & college == 1L, weighted.mean(p_hat, w)]
p_tip <- if (is.na(tipping)) NA_real_ else expit(logit(p_bar) + tipping)

msg("tipping delta = ", fmt_num(tipping, 4),
    "  (unmatched graduates' exit rate ", fmt_num(p_bar, 4),
    " -> ", fmt_num(p_tip, 4), ")")

# -----------------------------------------------------------------------------
# Table
# -----------------------------------------------------------------------------
f4 <- function(x) fmt_num(x, 4)
shown <- path[order(-delta)][seq(1, .N, length.out = min(.N, 8))]

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{How far the unmatched would have to depart from missing-at-random}",
  "\\label{tab:tipping}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{cc}",
  "\\toprule",
  "$\\delta$ (log-odds shift) & Mid-pandemic adjusted gap \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(shown)), function(i)
    sprintf("%s & %s \\\\", f4(shown$delta[i]), f4(shown$gap[i])))),
  "\\midrule",
  sprintf("Tipping point & %s \\\\",
          if (is.na(tipping)) "none in $\\delta \\geq -8$" else f4(tipping)),
  sprintf("Gap in the limit & %s \\\\", f4(floor_gap)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Each row re-estimates equation~\\eqref{eq:es} on all ",
         "origins for which a further interview is scheduled, with the outcome ",
         "of unmatched workers imputed from a model fitted on matched workers of ",
         "the same education group and then shifted by $\\delta$ in log-odds for ",
         "graduates only. $\\delta = 0$ is missing-at-random within education ",
         "group; negative values make unmatched graduates leave employment less ",
         "often than their observed characteristics imply, the direction that ",
         "works against the reversal. The tipping point is the value at which ",
         "the mid-pandemic adjusted gap reaches zero, located by bisection. ",
         "Because only the difference between the two groups' shifts matters, ",
         "the non-graduate shift is fixed at zero without loss."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_tipping.tex"))

# A small keyed file rather than parsing prose out of the log: 09 reads these
# three values straight into macros.
# When no root exists the informative number is the floor the gap approaches as
# the shift is pushed to its limit -- i.e. what survives even if unmatched
# graduates are assumed never to leave employment.
floor_gap <- path[which.min(delta), gap]
writeLines(c(
  paste0("delta=", if (is.na(tipping)) "none" else fmt_num(tipping, 3)),
  paste0("mar=",   fmt_num(p_bar, 3)),
  paste0("at=",    if (is.na(p_tip)) "--" else fmt_num(p_tip, 3)),
  paste0("floor=", fmt_num(floor_gap, 3)),
  paste0("deltamin=", fmt_num(path[, min(delta)], 0))
), file.path(DIR_LOGS, "12_tipping_point.txt"))

msg("12_tipping_point.R done.")
