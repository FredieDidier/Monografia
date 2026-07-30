# =============================================================================
# 03_main_estimation.R
#
# Core estimation for the paper.
#
#   (a) Event-study model: college x quarter interactions, 2019Q4 base.
#   (b) Survey-weighted average predictive margins by education x quarter, and
#       the college / non-college differences  (Comment 2).
#   (c) Two-way clustered inference, alternative variance matrices, and
#       wild cluster bootstrap p-values by year-quarter  (Comment 3).
#   (d) Table 1  period-level adjusted levels, gaps and relative magnitudes.
#       Table 2  specification ladder for the two pandemic windows.
#       Table A.3 sensitivity of the two windows to the variance estimator.
#
# Fitted models are cached under analysis/output/estimates so that the figure
# and heterogeneity scripts do not refit them.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})

if (!exists("ROOT"))   source(file.path("analysis", "code", "_config.R"))
if (!exists("fit_es")) source(file.path("analysis", "code", "_functions.R"))

setFixest_nthreads(max(1L, parallel::detectCores() - 2L))

d <- as.data.table(read_parquet(ANALYSIS_PQ))
d[, age_sq  := age^2]
d[, d_onset := as.integer(qtr == Q_ONSET)]
d[, d_mid   := as.integer(qtr %in% Q_MID)]
d[, col_onset := college * d_onset]
d[, col_mid   := college * d_mid]

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

# =============================================================================
# (a) Event study, for the aggregate exit and for each destination
# =============================================================================
# The stage-3 build records the destination state at t+1, so the aggregate
# employment exit can be split into its two economically distinct margins:
# a move into unemployment (still searching) and a move out of the labour force.
# The referee report is right that these differ in mechanism and in policy
# reading, so each gets its own event study and its own panel in Table 1.
OUTCOMES <- c(exit                 = "Employment exit",
              exit_to_unemployment = "Exit into unemployment",
              exit_to_nonpart      = "Exit into non-participation")

fit_outcome <- function(y) {
  f <- file.path(DIR_EST, sprintf("m_event_study_%s.rds", y))
  if (file.exists(f)) { msg("loading cached event study: ", y); return(readRDS(f)) }
  msg("fitting event study for ", y, " on ", fmt_n(nrow(d)), " observations ...")
  t0 <- Sys.time()
  m <- fit_es(d, outcome = y, cluster = ~ psu + qtr)
  msg("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
  saveRDS(m, f, compress = FALSE)
  m
}

models <- lapply(names(OUTCOMES), fit_outcome)
names(models) <- names(OUTCOMES)
m_es <- models[["exit"]]

am <- adjusted_margins(m_es, d)
fwrite(am, file.path(DIR_EST, "adjusted_margins_quarterly.csv"))
msg("sup-t critical value for the ", nrow(am), " quarterly contrasts: ",
    round(am$crit_sim[1], 3), " (pointwise 1.960)")

pm <- period_margins(m_es, d, PERIODS)
fwrite(pm, file.path(DIR_EST, "adjusted_margins_periods.csv"))

# =============================================================================
# (b) Table 1: adjusted levels, gaps and relative magnitudes, by destination
# =============================================================================
f3 <- function(x) fmt_num(x, 3)
f4 <- function(x) fmt_num(x, 4)
f1 <- function(x) fmt_num(x, 1)

panel_block <- function(y, title) {
  pmy <- period_margins(models[[y]], d, PERIODS, outcome = y)
  pre_n <- pmy[grepl("^Pre", period), m_nocol]
  pre_c <- pmy[grepl("^Pre", period), m_col]
  pmy[, `:=`(rel_nocol = 100 * (m_nocol / pre_n - 1),
             rel_col   = 100 * (m_col   / pre_c - 1))]
  fwrite(pmy, file.path(DIR_EST, sprintf("adjusted_margins_periods_%s.csv", y)))

  rows <- character(0)
  for (i in seq_len(nrow(pmy))) {
    r <- pmy[i]
    base <- r$period == names(PERIODS)[1]
    # Stars go on the difference only: columns (1) and (2) are adjusted
    # probabilities, and a test of a probability against zero says nothing.
    rows <- c(rows,
      sprintf("\\quad %s & %s & %s & %s & [%s, %s] & %s & %s \\\\",
              r$period, f3(r$m_nocol), f3(r$m_col), fmt_est(r$gap, r$gap_se),
              f3(r$gap_lo), f3(r$gap_hi),
              if (base) "--" else f1(r$rel_nocol),
              if (base) "--" else f1(r$rel_col)),
      sprintf(" & (%s) & (%s) & (%s) & & & \\\\",
              f4(r$gap_se * r$p_col), f4(r$gap_se * (1 - r$p_col)), f4(r$gap_se)))
  }
  c(sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", title), rows)
}

blocks <- unlist(lapply(seq_along(OUTCOMES), function(i) {
  b <- panel_block(names(OUTCOMES)[i],
                   sprintf("Panel %s. %s", LETTERS[i], OUTCOMES[[i]]))
  if (i < length(OUTCOMES)) c(b, "\\addlinespace") else b
}))

# Kept for downstream scripts that expect the aggregate margins.
pm <- fread(file.path(DIR_EST, "adjusted_margins_periods_exit.csv"))
fwrite(pm, file.path(DIR_EST, "adjusted_margins_periods.csv"))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Adjusted employment-exit probabilities and the college gap, by destination and period}",
  "\\label{tab:main_margins}",
  "\\begin{threeparttable}",
  # Twenty-six body rows plus a ten-line note make this table taller than the
  # text block at \small: the page number ended up printed over the notes. One
  # size down for the body and two for the notes brings it inside the block.
  "\\footnotesize",
  "\\renewcommand{\\arraystretch}{0.96}",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lccccrr}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Adjusted level} & \\multicolumn{3}{c}{College $-$ no college} & \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}",
  "& No college & College & Difference & 95\\% CI & \\multicolumn{2}{c}{\\% change vs.\\ pre-pandemic} \\\\",
  "\\cmidrule(lr){6-7}",
  "Period & (1) & (2) & (3) & (4) & No college & College \\\\",
  "\\midrule",
  blocks,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\scriptsize",
  paste0("\\item Notes: Entries in columns (1)--(3) are survey-weighted average ",
         "predictive margins from the event-study specification of ",
         "equation~(\\ref{eq:es}), which includes year-quarter, state, urban, ",
         "gender, ",
         "race, tenure, occupation and sector fixed effects and controls for age, ",
         "age squared, usual weekly hours, log labour income, formality, temporary ",
         "status and social security contribution. Margins are computed by holding ",
         "the covariate distribution fixed and setting the college indicator to ",
         "zero and to one in turn; they are not regression intercepts. ",
         "Panel A is any exit from employment; Panels B and C split it by ",
         "destination, so B and C sum to A by construction. Standard errors in ",
         "parentheses are two-way clustered by primary sampling unit and ",
         "year-quarter. Stars on the difference in column (3) denote ",
         "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$; the adjusted levels are ",
         "probabilities and carry none. ",
         "The last two columns report the percentage change in each ",
         "group's adjusted level relative to its own pre-pandemic average."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_main_margins.tex"))

# =============================================================================
# (c) Table 2: specification ladder for the two pandemic windows
# =============================================================================
BASE <- c("college", "col_onset", "col_mid")

SPECS <- list(
  list(lab = "(1)", x = c(BASE, "d_onset", "d_mid"), fe = character(0)),
  list(lab = "(2)", x = BASE, fe = c("qtr")),
  list(lab = "(3)", x = BASE, fe = c("qtr", "state", "urban")),
  list(lab = "(4)", x = c(BASE, "age", "age_sq"),
       fe = c("qtr", "state", "urban", "female", "race5")),
  list(lab = "(5)", x = c(BASE, CONTROLS),
       fe = c("qtr", "state", "urban", "female", "race5", "tenure")),
  list(lab = "(6)", x = c(BASE, CONTROLS),
       fe = c("qtr", "state", "urban", "female", "race5", "tenure",
              "occupation", "sector"))
)

f_win <- file.path(DIR_EST, "m_window_specs.rds")
if (file.exists(f_win)) {
  msg("loading cached specification ladder")
  fits <- readRDS(f_win)
} else {
  fits <- lapply(SPECS, function(s) {
    msg("fitting spec ", s$lab, " ...")
    fml <- if (length(s$fe))
      as.formula(sprintf("exit ~ %s | %s", paste(s$x, collapse = " + "),
                         paste(s$fe, collapse = " + ")))
    else
      as.formula(sprintf("exit ~ %s", paste(s$x, collapse = " + ")))
    feols(fml, data = d, weights = ~ w, cluster = ~ psu + qtr, mem.clean = TRUE)
  })
  saveRDS(fits, f_win, compress = FALSE)
}

#' Format a bootstrap p-value. With B replications the smallest attainable
#' non-zero value is 1/(B+1), so an exact zero is reported as a bound rather
#' than as "0.000", which would overstate the resolution of the test.
fmt_boot_p <- function(p, B = B_WILD) {
  ifelse(is.na(p), "--",
    ifelse(p < 1 / (B + 1), sprintf("$<$%.4f", 1 / (B + 1)),
           formatC(p, format = "f", digits = 3)))
}

#' Point estimate, s.e. and p-value for a linear combination of coefficients.
lincom <- function(m, terms) {
  b <- coef(m); V <- vcov(m)
  R <- rep(0, length(b)); names(R) <- names(b)
  miss <- setdiff(terms, names(b))
  if (length(miss)) return(c(est = NA, se = NA, p = NA))
  R[terms] <- 1
  est <- sum(R * b)
  # Two-way clustering is not guaranteed positive semi-definite; fixest already
  # applies the Cameron-Gelbach-Miller eigenvalue fix to the diagonal, but a
  # linear combination can still land marginally below zero.
  vr  <- as.numeric(t(R) %*% V %*% R)
  se  <- sqrt(max(vr, 0))
  c(est = est, se = se, p = 2 * pnorm(-abs(est / se)))
}

# The wild cluster bootstrap is expensive on 7.9 million observations (roughly
# 40 minutes per parameter at B_WILD replications), so it is run only for the
# preferred specification -- the last column -- and cached. That is also exactly
# the object of interest: the two pandemic contrasts under the full
# conditioning set, with uncertainty driven by the number of quarterly shocks
# rather than by the number of records.
f_wcb <- file.path(DIR_EST, "wcb_pvalues.rds")
if (file.exists(f_wcb)) {
  msg("loading cached wild cluster bootstrap p-values")
  wcb_pref <- readRDS(f_wcb)
} else {
  msg("wild cluster bootstrap p-values (", fmt_n(B_WILD), " replications, ",
      "clustered by year-quarter) for the preferred specification ...")
  t0 <- Sys.time()
  wcb_pref <- c(onset = wcb_pvalue(fits[[length(fits)]], "col_onset"),
                mid   = wcb_pvalue(fits[[length(fits)]], "col_mid"))
  msg("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
  saveRDS(wcb_pref, f_wcb, compress = FALSE)
}
wcb <- c(rep(list(c(onset = NA_real_, mid = NA_real_)), length(fits) - 1L),
         list(wcb_pref))

grab <- function(m, nm) {
  b <- coef(m); V <- vcov(m)
  if (!nm %in% names(b)) return(c(NA, NA))
  c(b[[nm]], sqrt(V[nm, nm]))
}

#' Stars for a coefficient, taken from the wild cluster bootstrap wherever one
#' was run and from the asymptotic two-way clustered s.e. otherwise. Reporting
#' a bootstrap p of 0.177 next to three asymptotic stars on the same number, as
#' an earlier vintage of this table did, states two contradictory things about
#' the onset deviation: with one treated year-quarter the asymptotic variance is
#' the one that is wrong, so the bootstrap takes precedence where it exists.
fmt_est_boot <- function(b, se, p_boot, digits = 3) {
  p    <- ifelse(is.na(p_boot), 2 * stats::pnorm(-abs(b / se)), p_boot)
  star <- ifelse(is.na(p), "",
          ifelse(p < 0.01, "$^{***}$",
          ifelse(p < 0.05, "$^{**}$",
          ifelse(p < 0.10, "$^{*}$", ""))))
  sprintf("%s%s", formatC(b, format = "f", digits = digits), star)
}

#' A coefficient row. `pkey`, when given, names the entry of `wcb` holding the
#' bootstrap p-value for this parameter; the p-value is printed underneath the
#' s.e., directly below the coefficient it tests.
row_coef <- function(label, nm, pkey = NULL) {
  est <- sapply(fits, function(m) grab(m, nm))
  pv  <- if (is.null(pkey)) rep(NA_real_, length(fits)) else sapply(wcb, `[[`, pkey)
  c(sprintf("%s & %s \\\\", label,
            paste(fmt_est_boot(est[1, ], est[2, ], pv), collapse = " & ")),
    sprintf(" & %s \\\\", paste(fmt_se(est[2, ]), collapse = " & ")),
    if (is.null(pkey)) NULL else
      sprintf("\\quad Wild cluster bootstrap $p$ & %s \\\\",
              paste(fmt_boot_p(pv), collapse = " & ")))
}

# The implied gaps delta + omega are not bootstrapped: the bootstrap imposes the
# null on a single coefficient. They keep their asymptotic stars, which is
# defensible because delta is identified off all quarters.
row_lincom <- function(label, terms) {
  lc <- sapply(fits, lincom, terms = terms)
  c(sprintf("%s & %s \\\\", label,
            paste(fmt_est(lc["est", ], lc["se", ]), collapse = " & ")),
    sprintf(" & %s \\\\", paste(fmt_se(lc["se", ]), collapse = " & ")))
}

yesno <- function(idx) sapply(SPECS, function(s)
  if (all(idx %in% c(s$fe, s$x))) "Yes" else "--")

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{The college gap in employment exits across specifications}",
  "\\label{tab:spec_ladder}",
  "\\begin{threeparttable}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  paste0("& ", paste(sapply(SPECS, `[[`, "lab"), collapse = " & "), " \\\\"),
  "\\midrule",
  row_coef("College, other quarters ($\\delta$)", "college"),
  "\\addlinespace",
  row_coef("College $\\times$ onset ($\\omega_1$)", "col_onset", "onset"),
  "\\addlinespace",
  row_coef("College $\\times$ mid-pandemic ($\\omega_2$)", "col_mid", "mid"),
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Implied college gap}} \\\\",
  row_lincom("\\quad Onset ($\\delta+\\omega_1$)", c("college", "col_onset")),
  "\\addlinespace",
  row_lincom("\\quad Mid-pandemic ($\\delta+\\omega_2$)", c("college", "col_mid")),
  "\\midrule",
  paste0("Year-quarter FE & ", paste(yesno("qtr"), collapse = " & "), " \\\\"),
  paste0("State and urban FE & ", paste(yesno(c("state", "urban")), collapse = " & "), " \\\\"),
  paste0("Gender and race FE & ", paste(yesno(c("female", "race5")), collapse = " & "), " \\\\"),
  paste0("Tenure FE & ", paste(yesno("tenure"), collapse = " & "), " \\\\"),
  paste0("Occupation and sector FE & ", paste(yesno(c("occupation", "sector")), collapse = " & "), " \\\\"),
  paste0("Time-varying job controls & ",
         paste(sapply(SPECS, function(s) if ("hours" %in% s$x) "Yes" else "--"),
               collapse = " & "), " \\\\"),
  "\\addlinespace",
  paste0("Observations & ", paste(fmt_n(sapply(fits, nobs)), collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(formatC(sapply(fits, r2, type = "r2"),
                                   format = "f", digits = 3), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Survey-weighted linear probability models. The dependent ",
         "variable is an indicator for exiting employment between $t$ and $t+1$. ",
         "$\\delta$ is the college gap outside the two pandemic windows; ",
         "$\\omega_1$ and $\\omega_2$ shift it at the onset and during the ",
         "mid-pandemic window. Standard errors in parentheses are two-way clustered ",
         "by primary sampling unit and year-quarter. For the two pandemic ",
         "deviations we also report, for the preferred specification in column ",
         "(6), wild cluster bootstrap $p$-values clustered by year-quarter (",
         uniqueN(d$qtr), " clusters), using Rademacher weights with the null ",
         "imposed and ", fmt_n(B_WILD), " replications; they ",
         "test $\\omega_1$ and $\\omega_2$, the shifts in the gap, which is ",
         "where the number of quarterly shocks rather than the number of records ",
         "determines uncertainty. ",
         "Stars denote $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$; they are ",
         "based on the wild cluster bootstrap $p$-value wherever one is ",
         "reported, and on the asymptotic two-way clustered standard error ",
         "otherwise. The onset deviation $\\omega_1$ therefore carries no stars ",
         "in column (6): identified from a single year-quarter, it cannot be ",
         "told apart from an ordinary quarterly shock. The implied gaps keep ",
         "their asymptotic stars because they are dominated by $\\delta$, which ",
         "is identified across all ", uniqueN(d$qtr), " quarters. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_spec_ladder.tex"))

# =============================================================================
# (d) Table A.3: sensitivity to the variance estimator
# =============================================================================
m_full <- fits[[length(fits)]]
VCOVS <- list(
  "Two-way: PSU and year-quarter" = ~ psu + qtr,
  "PSU"                           = ~ psu,
  "Household"                     = ~ household,
  "Individual"                    = ~ pid,
  "Two-way: individual and year-quarter" = ~ pid + qtr,
  "Year-quarter"                  = ~ qtr
)

vrows <- rbindlist(lapply(names(VCOVS), function(nm) {
  V <- vcov(m_full, cluster = VCOVS[[nm]])
  b <- coef(m_full)
  lc <- function(terms) {
    R <- rep(0, length(b)); names(R) <- names(b); R[terms] <- 1
    est <- sum(R * b); se <- sqrt(max(as.numeric(t(R) %*% V %*% R), 0))
    c(est, se)
  }
  o <- lc(c("college", "col_onset")); m <- lc(c("college", "col_mid"))
  data.table(vcov = nm,
             n_clusters = length(unique(m_full$fixef_id[[1]])),
             base_se  = sqrt(V["college", "college"]),
             onset_se = o[2], mid_se = m[2],
             onset = o[1], mid = m[1],
             base = b[["college"]])
}))
fwrite(vrows, file.path(DIR_EST, "vcov_sensitivity.csv"))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Sensitivity of the college gap to the variance estimator}",
  "\\label{tab:vcov}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "& Other quarters & Onset 2020Q1 & Mid-pandemic \\\\",
  "Clustering & $\\delta$ & $\\delta+\\omega_1$ & $\\delta+\\omega_2$ \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(vrows)), function(i) {
    r <- vrows[i]
    c(sprintf("%s & %s & %s & %s \\\\", r$vcov,
              fmt_est(r$base, r$base_se), fmt_est(r$onset, r$onset_se),
              fmt_est(r$mid, r$mid_se)),
      sprintf(" & %s & %s & %s \\\\", fmt_se(r$base_se), fmt_se(r$onset_se),
              fmt_se(r$mid_se)))
  })),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: All rows report the same point estimates from the full ",
         "specification, column (6) of Table~\\ref{tab:spec_ladder}; only the ",
         "variance estimator changes. Number of clusters: ",
         fmt_n(uniqueN(d$psu)), " primary sampling units, ",
         fmt_n(uniqueN(d$household)), " households, ",
         fmt_n(uniqueN(d$pid)), " individuals, ",
         uniqueN(d$qtr), " year-quarters. The onset contrast is a case in which ",
         "no asymptotic cluster-robust variance is adequate: the deviation is ",
         "identified from a single year-quarter cluster, and the ",
         "cluster-robust variance is then known to be severely downward biased. ",
         "The wild cluster bootstrap in Table~\\ref{tab:spec_ladder} is the ",
         "relevant benchmark for it."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_vcov_sensitivity.tex"))

msg("03_main_estimation.R done.")
