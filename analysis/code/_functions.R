# =============================================================================
# _functions.R
# Estimation, inference and reporting helpers shared by the analysis scripts.
# Sourced by 00_master_analysis.R; never run on its own.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# -----------------------------------------------------------------------------
# Specification
# -----------------------------------------------------------------------------

# Continuous / binary controls entering X'gamma.
CONTROLS <- c("age", "age_sq", "hours", "log_income",
              "formal", "temporary", "social_security")

# Absorbed fixed effects.
FE_FULL <- c("qtr", "state", "urban", "female", "race5", "tenure",
             "occupation", "sector")

#' Event-study formula: college main effect + college x quarter interactions.
#'
#' The reference quarter is dropped from the interaction set, so the college
#' coefficient is the education gap in that quarter and each interaction is the
#' deviation of the gap from it.
es_formula <- function(outcome = "exit",
                       controls = CONTROLS,
                       fe       = FE_FULL,
                       ref      = REF_QUARTER) {
  rhs <- paste(
    c("college",
      sprintf("i(qtr, college, ref = %d)", ref),
      controls),
    collapse = " + "
  )
  stats::as.formula(sprintf("%s ~ %s | %s", outcome, rhs, paste(fe, collapse = " + ")))
}

#' Compact interaction formula (onset / mid-pandemic windows).
win_formula <- function(outcome = "exit",
                        controls = CONTROLS,
                        fe       = FE_FULL) {
  rhs <- paste(c("college", "college:d_onset", "college:d_mid", controls),
               collapse = " + ")
  stats::as.formula(sprintf("%s ~ %s | %s", outcome, rhs, paste(fe, collapse = " + ")))
}

#' Fit the event-study model.
#'
#' @param d        data.table with the analysis sample
#' @param outcome  dependent variable name
#' @param cluster  one-sided formula for the cluster structure
#' @param fe,controls specification overrides
fit_es <- function(d, outcome = "exit",
                   cluster  = ~ psu + qtr,
                   controls = CONTROLS,
                   fe       = FE_FULL,
                   ref      = REF_QUARTER) {
  feols(es_formula(outcome, controls, fe, ref),
        data    = d,
        weights = ~ w,
        cluster = cluster,
        lean    = FALSE,
        mem.clean = TRUE)
}

# -----------------------------------------------------------------------------
# Adjusted margins (Comment 2)
# -----------------------------------------------------------------------------
#
# With the outcome linear in the parameters and `college` entering only through
# its main effect and its quarter interactions, the individual-level difference
# between the two counterfactuals (college = 1 vs college = 0) is constant
# within a quarter. Writing b for the estimated coefficient vector:
#
#     Delta_q = m_1q - m_0q = delta + beta_q            (beta_ref := 0)
#
# exactly, with no contribution from the covariates or the absorbed effects.
#
# The survey-weighted average predictive margins themselves follow from the
# WLS normal equations. Quarter is among the absorbed fixed effects, so the
# weighted residuals sum to zero within every quarter, hence the weighted mean
# fitted value in quarter q equals the weighted mean outcome ybar_q. Splitting
# that mean between the two education groups gives
#
#     m_gq = ybar_q + (g - p_q) * Delta_q ,
#
# where p_q is the survey-weighted college share in quarter q. This reproduces
# what marginaleffects::avg_predictions() returns for this model (verified in
# 08_robustness.R on a random subsample) without materialising two 7.9-million
# row counterfactual design matrices.

#' Extract the quarter-by-quarter adjusted education gaps and their covariance.
#'
#' @return list(q, est, se, V) where `V` is the full covariance matrix of the
#'   51 gaps, needed for the simultaneous bands.
es_gaps <- function(m, quarters, ref = REF_QUARTER) {
  b  <- stats::coef(m)
  V  <- stats::vcov(m)
  nm <- names(b)

  i_col <- match("college", nm)
  if (is.na(i_col)) stop("`college` coefficient not found.")

  # fixest names interactions "qtr::20201:college"
  int_name <- function(q) sprintf("qtr::%d:college", q)

  R <- matrix(0, nrow = length(quarters), ncol = length(b),
              dimnames = list(qlab(quarters), nm))
  R[, i_col] <- 1
  for (k in seq_along(quarters)) {
    q <- quarters[k]
    if (q == ref) next
    j <- match(int_name(q), nm)
    if (is.na(j)) stop("Interaction not found for quarter ", q)
    R[k, j] <- 1
  }

  est <- as.vector(R %*% b)
  VD  <- R %*% V %*% t(R)
  list(q = quarters, est = est, se = sqrt(diag(VD)), V = VD)
}

#' sup-t simultaneous critical value via the multiplier bootstrap.
#'
#' Draws from N(0, V) and takes the (1 - alpha) quantile of max_q |z_q| / se_q.
#' Used instead of 51 separate pointwise intervals (Comment 3).
supt_crit <- function(V, se, alpha = 0.05, B = B_SUPT, seed = SEED) {
  set.seed(seed)
  ev <- eigen((V + t(V)) / 2, symmetric = TRUE)
  ev$values[ev$values < 0] <- 0
  A  <- ev$vectors %*% diag(sqrt(ev$values), nrow = length(ev$values))
  Z  <- A %*% matrix(stats::rnorm(ncol(A) * B), nrow = ncol(A))
  stats::quantile(apply(abs(Z / se), 2, max), 1 - alpha, names = FALSE)
}

#' Survey-weighted mean outcome and college share by quarter.
quarter_means <- function(d, outcome = "exit") {
  d[, .(ybar   = stats::weighted.mean(get(outcome), w),
        p_col  = stats::weighted.mean(college, w),
        n      = .N),
    by = qtr][order(qtr)]
}

#' Adjusted levels m_gq and gaps Delta_q as one tidy table.
adjusted_margins <- function(m, d, outcome = "exit", ref = REF_QUARTER,
                             alpha = 0.05) {
  qm <- quarter_means(d, outcome)
  g  <- es_gaps(m, qm$qtr, ref)

  crit_pt <- stats::qnorm(1 - alpha / 2)
  crit_st <- supt_crit(g$V, g$se, alpha)

  data.table(
    qtr        = qm$qtr,
    quarter    = qlab(qm$qtr),
    qnum       = qnum(qm$qtr),
    n          = qm$n,
    ybar       = qm$ybar,
    p_col      = qm$p_col,
    m_nocol    = qm$ybar - qm$p_col * g$est,          # g = 0
    m_col      = qm$ybar + (1 - qm$p_col) * g$est,    # g = 1
    gap        = g$est,
    gap_se     = g$se,
    gap_lo     = g$est - crit_pt * g$se,
    gap_hi     = g$est + crit_pt * g$se,
    gap_lo_sim = g$est - crit_st * g$se,
    gap_hi_sim = g$est + crit_st * g$se,
    # Levels inherit the gap's sampling error scaled by the group share; the
    # covariate distribution and ybar_q are held fixed, as in avg_predictions().
    m_nocol_se = qm$p_col * g$se,
    m_col_se   = (1 - qm$p_col) * g$se,
    crit_sim   = crit_st
  )[, `:=`(
    m_nocol_lo = m_nocol - crit_pt * m_nocol_se,
    m_nocol_hi = m_nocol + crit_pt * m_nocol_se,
    m_col_lo   = m_col   - crit_pt * m_col_se,
    m_col_hi   = m_col   + crit_pt * m_col_se
  )][]
}

#' Aggregate the quarterly margins into period averages.
#'
#' Quarters are weighted by their share of the total survey weight inside the
#' period, so a period figure is the survey-weighted average predictive margin
#' over the pooled person-quarters of that period. Standard errors follow from
#' the same linear combination applied to the covariance of the quarterly gaps.
#'
#' @param periods named list of integer vectors of quarters
period_margins <- function(m, d, periods, outcome = "exit", ref = REF_QUARTER,
                           alpha = 0.05) {
  qm <- quarter_means(d, outcome)
  sw <- d[, .(sw = sum(w)), by = qtr]
  qm <- merge(qm, sw, by = "qtr")[order(qtr)]
  g  <- es_gaps(m, qm$qtr, ref)

  crit <- stats::qnorm(1 - alpha / 2)
  out <- rbindlist(lapply(names(periods), function(nm) {
    idx <- which(qm$qtr %in% periods[[nm]])
    if (!length(idx)) return(NULL)
    om <- qm$sw[idx] / sum(qm$sw[idx])
    gp <- sum(om * g$est[idx])
    vp <- max(as.numeric(t(om) %*% g$V[idx, idx, drop = FALSE] %*% om), 0)
    ybar_p <- sum(om * qm$ybar[idx])
    p_p    <- sum(om * qm$p_col[idx])
    data.table(
      period   = nm,
      quarters = length(idx),
      n        = sum(qm$n[idx]),
      m_nocol  = sum(om * (qm$ybar[idx] - qm$p_col[idx] * g$est[idx])),
      m_col    = sum(om * (qm$ybar[idx] + (1 - qm$p_col[idx]) * g$est[idx])),
      ybar     = ybar_p,
      p_col    = p_p,
      gap      = gp,
      gap_se   = sqrt(vp),
      gap_lo   = gp - crit * sqrt(vp),
      gap_hi   = gp + crit * sqrt(vp)
    )
  }))
  out[]
}

# -----------------------------------------------------------------------------
# Wild cluster bootstrap (Comment 3)
# -----------------------------------------------------------------------------

#' Wild cluster bootstrap p-value for a single coefficient, clustered by
#' year-quarter. Rademacher weights, null imposed (WCR).
wcb_pvalue <- function(m, param, B = B_WILD, seed = SEED, cluster = "qtr") {
  if (!requireNamespace("fwildclusterboot", quietly = TRUE)) return(NA_real_)
  # From fwildclusterboot 0.13 the bootstrap draws come from the ordinary R
  # stream and `boottest()` no longer takes a `seed` argument, so the seed has
  # to be set here for the replication to be exact.
  set.seed(seed)
  if (requireNamespace("dqrng", quietly = TRUE)) dqrng::dqset.seed(seed)
  out <- try(
    fwildclusterboot::boottest(
      m, param = param, clustid = cluster, B = B,
      type = "rademacher", impose_null = TRUE
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    warning("wild cluster bootstrap failed for '", param, "': ",
            conditionMessage(attr(out, "condition")), call. = FALSE)
    return(NA_real_)
  }
  fwildclusterboot::pval(out)
}

# -----------------------------------------------------------------------------
# Composition vs within-cell decomposition (Comment 6)
# -----------------------------------------------------------------------------
#
#   Gap_q = m_Cq - m_Nq
#         = sum_k (s_Ckq - s_Nkq) * m_Nkq      <- composition
#         + sum_k  s_Ckq * (m_Ckq - m_Nkq)     <- within-cell
#
# where k indexes cells defined by formality x sector x occupation, s_gkq is the
# survey-weighted share of education group g in cell k in quarter q, and m_gkq
# is the group's weighted exit rate in that cell.

#' Pre-aggregate the microdata to PSU x quarter x education x cell totals.
#'
#' Everything the decomposition needs is a sum of weights and a sum of weighted
#' outcomes, so the 7.9-million row sample collapses once and every bootstrap
#' replication then works on the collapsed table.
decomp_cells <- function(d, cell_vars = c("formal", "sector", "occupation"),
                         outcome = "exit") {
  keep <- unique(c("qtr", "psu", "college", "w", outcome, cell_vars))
  x <- d[, ..keep]
  x[, cell := do.call(paste, c(lapply(.SD, as.character), sep = "|")),
    .SDcols = cell_vars]
  x[, y_ := as.numeric(get(outcome))]
  x[, .(sw = sum(w), swy = sum(w * y_)), by = .(qtr, psu, college, cell)]
}

#' Decomposition of the raw education gap from pre-aggregated cell totals.
#'
#' @param agg  output of decomp_cells(), optionally with re-weighted totals
decompose_from_cells <- function(agg) {
  a <- agg[, .(sw = sum(sw), swy = sum(swy)), by = .(qtr, college, cell)]
  a[, m := swy / sw]
  a[, s := sw / sum(sw), by = .(qtr, college)]

  wide <- merge(
    a[college == 1L, .(qtr, cell, s_C = s, m_C = m)],
    a[college == 0L, .(qtr, cell, s_N = s, m_N = m)],
    by = c("qtr", "cell"), all = TRUE
  )
  # A cell absent for one group carries zero share there; its (undefined) mean
  # must not propagate NA into the sums.
  wide[is.na(s_C), `:=`(s_C = 0, m_C = 0)]
  wide[is.na(s_N), `:=`(s_N = 0, m_N = 0)]

  wide[, .(
    composition = sum((s_C - s_N) * m_N),
    within      = sum(s_C * (m_C - m_N)),
    gap_raw     = sum(s_C * m_C) - sum(s_N * m_N)
  ), by = qtr][order(qtr)]
}

#' Aggregate quarterly decomposition components into periods.
decomp_periods <- function(dec, qweight, periods) {
  dt <- merge(dec, qweight, by = "qtr")
  rbindlist(lapply(names(periods), function(nm) {
    z <- dt[qtr %in% periods[[nm]]]
    if (!nrow(z)) return(NULL)
    om <- z$sw / sum(z$sw)
    data.table(period      = nm,
               composition = sum(om * z$composition),
               within      = sum(om * z$within),
               gap_raw     = sum(om * z$gap_raw))
  }))
}

#' Cluster bootstrap for the decomposition, resampling PSUs within quarter.
#'
#' Uses exponential (Bayesian bootstrap) multipliers at the PSU-quarter level so
#' that no resampled copy of the microdata ever has to be materialised.
decompose_boot <- function(agg, qweight, periods, B = B_DECOMP, seed = SEED) {
  set.seed(seed)
  gk <- unique(agg[, .(qtr, psu)])
  gk[, gid := .I]
  a  <- merge(agg, gk, by = c("qtr", "psu"))
  ng <- nrow(gk)

  keep <- rbindlist(lapply(seq_len(B), function(b) {
    mult <- stats::rexp(ng)
    ab <- a[, .(qtr, college, cell,
                sw  = sw  * mult[gid],
                swy = swy * mult[gid])]
    qw <- ab[, .(sw = sum(sw)), by = qtr]
    dd <- decomp_periods(decompose_from_cells(ab), qw, periods)
    dd[, rep := b][]
  }))

  keep[, .(composition_se = stats::sd(composition),
           within_se      = stats::sd(within),
           gap_se         = stats::sd(gap_raw),
           composition_lo = stats::quantile(composition, 0.025, names = FALSE),
           composition_hi = stats::quantile(composition, 0.975, names = FALSE),
           within_lo      = stats::quantile(within, 0.025, names = FALSE),
           within_hi      = stats::quantile(within, 0.975, names = FALSE)),
       by = period]
}

# -----------------------------------------------------------------------------
# LaTeX helpers
# -----------------------------------------------------------------------------

#' Round to a fixed number of decimals without ever printing a signed zero.
#' A gap of -0.0002 formats to "-0.000", which reads as a negative number that
#' happens to round to zero; at three decimals it is simply zero.
fmt_num <- function(x, digits = 3) {
  out <- formatC(x, format = "f", digits = digits)
  sub("^-(0\\.?0*)$", "\\1", out)
}

#' Format an estimate with its standard error underneath, plus stars.
fmt_est <- function(b, se, digits = 3) {
  p <- 2 * stats::pnorm(-abs(b / se))
  star <- ifelse(is.na(p), "",
          ifelse(p < 0.01, "$^{***}$",
          ifelse(p < 0.05, "$^{**}$",
          ifelse(p < 0.10, "$^{*}$", ""))))
  sprintf("%s%s", fmt_num(b, digits), star)
}

fmt_se <- function(se, digits = 3) {
  sprintf("(%s)", fmt_num(se, digits))
}

fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")

#' Write a character vector to a .tex fragment consumed by \input{}.
write_tex <- function(lines, file) {
  writeLines(lines, file)
  msg("wrote ", basename(file))
  invisible(file)
}

#' Escape the handful of LaTeX-special characters that appear in our labels.
tex_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x
}
