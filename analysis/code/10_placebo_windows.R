# =============================================================================
# 10_placebo_windows.R
#
# Was the mid-pandemic window special, or is a six-quarter run of positive gaps
# the kind of thing this series produces anyway?
#
# The quarterly contrasts cannot answer that on their own: each one is supported
# by a single year-quarter, so its interval cannot absorb an education-specific
# aggregate shock arriving in that same quarter. This script therefore works on
# the series of gaps itself, treating the quarter as the unit of observation,
#
#     Delta_q = a + tau * 1{q in W} + u_q,
#
# and asks how the realised window compares with every other contiguous
# six-quarter window the sample admits. With 52 quarters the whole exercise is
# arithmetic on a 52-row table; the cost is in 03, which produced the gaps.
#
# Outputs
#   estimates/placebo_windows.csv     tau for every candidate window
#   tables/tab_placebo.tex            the exhibit
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(sandwich); library(lmtest)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

am <- fread(file.path(DIR_EST, "adjusted_margins_quarterly.csv"))
setorder(am, qtr)
NQ <- nrow(am)
W  <- length(Q_MID)                       # six quarters, by construction
msg("placebo windows: ", NQ, " quarters, window length ", W)

# -----------------------------------------------------------------------------
# tau for one window
# -----------------------------------------------------------------------------
# HAC with four lags: the gaps are a quarterly time series and the window is a
# run of consecutive quarters, so neighbouring residuals are the dependence that
# matters. Four lags is one year.
tau_for <- function(idx) {
  z <- data.table(gap = am$gap, d = 0L)
  z[idx, d := 1L]
  m  <- stats::lm(gap ~ d, data = z)
  V  <- sandwich::NeweyWest(m, lag = 4, prewhite = FALSE, adjust = TRUE)
  ct <- lmtest::coeftest(m, vcov. = V)
  list(tau = unname(coef(m)["d"]),
       se  = unname(ct["d", "Std. Error"]),
       p   = unname(ct["d", "Pr(>|t|)"]))
}

# Every contiguous window of the same length, so the observed one is compared
# with like for like rather than with an arbitrary alternative.
starts <- seq_len(NQ - W + 1L)
pl <- rbindlist(lapply(starts, function(s) {
  idx <- s:(s + W - 1L)
  r   <- tau_for(idx)
  data.table(start = am$quarter[s], end = am$quarter[s + W - 1L],
             tau = r$tau, se = r$se, p = r$p,
             is_observed = identical(am$qtr[idx], as.integer(Q_MID)))
}))

# A placebo window is one that does not overlap the pandemic at all: overlapping
# windows inherit part of the very movement being tested and would understate
# how unusual it is.
pl[, pre_pandemic := vapply(starts, function(s)
  all(am$qtr[s:(s + W - 1L)] < Q_ONSET), logical(1))]

fwrite(pl, file.path(DIR_EST, "placebo_windows.csv"))

obs <- pl[is_observed == TRUE]
if (nrow(obs) != 1L)
  stop("the mid-pandemic window was not matched exactly among the candidates")

pre <- pl[pre_pandemic == TRUE]
# Rank of the realised window among the placebos, on the same one-sided scale as
# the claim being tested: how many pre-pandemic windows reach a tau this large?
n_ge     <- pre[tau >= obs$tau, .N]
p_placebo <- (1 + n_ge) / (1 + nrow(pre))

msg("  observed tau = ", fmt_num(obs$tau, 4),
    " (HAC se ", fmt_num(obs$se, 4), ", p = ", fmt_num(obs$p, 4), ")")
msg("  placebo windows: ", nrow(pre), "; reaching that tau: ", n_ge,
    "  -> placebo p = ", fmt_num(p_placebo, 4))

# -----------------------------------------------------------------------------
# Alternative window lengths, since 2020Q2--2021Q3 was read off the series
# -----------------------------------------------------------------------------
alt_windows <- list(
  "2020Q2--2021Q2" = 20202:20212,
  "2020Q2--2021Q3" = 20202:20213,
  "2020Q2--2021Q4" = 20202:20214
)
alt <- rbindlist(lapply(names(alt_windows), function(nm) {
  qs  <- am$qtr[am$qtr %in% alt_windows[[nm]]]
  idx <- match(qs, am$qtr)
  r   <- tau_for(idx)
  data.table(window = nm, nq = length(idx),
             tau = r$tau, se = r$se, p = r$p)
}))
fwrite(alt, file.path(DIR_EST, "placebo_alt_windows.csv"))

# -----------------------------------------------------------------------------
# Table
# -----------------------------------------------------------------------------
f4 <- function(x) fmt_num(x, 4)

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{The mid-pandemic window against placebo windows of the same length}",
  "\\label{tab:placebo}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Window & Quarters & $\\tau$ & HAC s.e. & $p$ \\\\",
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel A. Alternative end points for the window}} \\\\",
  unlist(lapply(seq_len(nrow(alt)), function(i)
    sprintf("\\quad %s & %d & %s & %s & %s \\\\", alt$window[i], alt$nq[i],
            f4(alt$tau[i]), f4(alt$se[i]), f4(alt$p[i])))),
  "\\addlinespace",
  "\\multicolumn{5}{l}{\\textit{Panel B. Placebo distribution}} \\\\",
  sprintf("\\quad Observed window & %d & %s & %s & %s \\\\",
          W, f4(obs$tau), f4(obs$se), f4(obs$p)),
  sprintf("\\quad Pre-pandemic placebos & %d & %s & %s & \\\\",
          nrow(pre), f4(pre[, mean(tau)]), f4(pre[, stats::sd(tau)])),
  sprintf("\\quad Placebos reaching $\\tau_{\\text{obs}}$ & %d of %d & & & %s \\\\",
          n_ge, nrow(pre), f4(p_placebo)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: $\\tau$ is the coefficient on a window indicator in ",
         "$\\Delta_q = a + \\tau\\mathbf{1}\\{q \\in W\\} + u_q$, estimated on the ",
         sprintf("%d ", NQ), "quarterly adjusted gaps of Figure~\\ref{fig:gap} ",
         "with the quarter as the unit of observation. Standard errors are ",
         "Newey--West with four lags. Panel A varies the end point of the window, ",
         "which was read off the estimated series rather than fixed in advance. ",
         "Panel B compares the realised window with every contiguous window of ",
         "the same length that lies entirely before 2020Q1; the reported $p$ is ",
         "$(1+\\#\\{\\tau_{\\text{placebo}} \\geq \\tau_{\\text{obs}}\\})/(1+n)$. ",
         "This exercise treats the quarter, not the worker, as the unit of ",
         "observation, and so is the aggregate-time counterpart of the ",
         "worker-level inference in Table~\\ref{tab:main_margins}."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_placebo.tex"))

msg("10_placebo_windows.R done.")
