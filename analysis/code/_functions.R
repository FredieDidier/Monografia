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

# Continuous / binary controls entering X'gamma. Two indicators accompany
# `log_income`, which is zero for both of them: `unpaid_family` marks workers
# whose income is genuinely zero, `income_missing` marks non-response. Pooling
# the two, or recoding either to zero earnings, is what the earlier vintage did
# (see 01_prepare_analysis_data.R).
CONTROLS <- c("age", "age_sq", "hours", "log_income", "income_missing",
              "unpaid_family", "formal", "temporary", "social_security")

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
# Adjusted margins
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
#'   52 gaps (the reference quarter included, where Delta = delta), needed
#'   for the simultaneous bands.
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
#' Used alongside the 52 pointwise intervals, to correct their multiplicity.
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
# Wild cluster bootstrap
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
# Composition vs within-cell decomposition
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
#' outcomes, so the 8.0-million row sample collapses once and every bootstrap
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

#' Decomposition under an alternative reference, or on common support only.
#'
#' Equation (4) in the paper fixes one reference: composition is weighted by
#' non-college rates and within-cell by college shares. That choice is not
#' neutral, and a reader is entitled to ask whether the split survives the
#' opposite convention. `ref = "college"` weights composition by college rates
#' and within-cell by non-college shares; `ref = "symmetric"` uses the average
#' rates and shares of the two groups, which is the convention that treats
#' neither group as the counterfactual.
#'
#' `common_support = TRUE` drops cells that only one education group occupies.
#' Those cells have no rate for the absent group, so the split between the two
#' components there is a convention rather than a measurement; restricting to
#' common support shows how much of the answer rests on it. The total gap then
#' changes too, because the dropped cells carried some of it.
decompose_variant <- function(agg, ref = c("noncollege", "college", "symmetric"),
                              common_support = FALSE) {
  ref <- match.arg(ref)
  a <- agg[, .(sw = sum(sw), swy = sum(swy)), by = .(qtr, college, cell)]
  a[, m := swy / sw]

  wide <- merge(
    a[college == 1L, .(qtr, cell, sw_C = sw, m_C = m)],
    a[college == 0L, .(qtr, cell, sw_N = sw, m_N = m)],
    by = c("qtr", "cell"), all = TRUE
  )
  if (common_support) {
    wide <- wide[!is.na(sw_C) & !is.na(sw_N)]
  } else {
    wide[is.na(sw_C), `:=`(sw_C = 0, m_C = 0)]
    wide[is.na(sw_N), `:=`(sw_N = 0, m_N = 0)]
  }
  # Shares are renormalised after any restriction, so each group's shares still
  # sum to one within the quarter and the two components remain comparable.
  wide[, `:=`(s_C = sw_C / sum(sw_C), s_N = sw_N / sum(sw_N)), by = qtr]

  wide[, {
    comp <- switch(ref,
      noncollege = sum((s_C - s_N) * m_N),
      college    = sum((s_C - s_N) * m_C),
      symmetric  = sum((s_C - s_N) * (m_C + m_N) / 2))
    with_ <- switch(ref,
      noncollege = sum(s_C * (m_C - m_N)),
      college    = sum(s_N * (m_C - m_N)),
      symmetric  = sum((s_C + s_N) / 2 * (m_C - m_N)))
    .(composition = comp, within = with_,
      gap_raw = sum(s_C * m_C) - sum(s_N * m_N))
  }, by = qtr][order(qtr)]
}

#' Reallocation index: did a group move towards cells that were riskier before?
#'
#' The composition component shrinking during the window does not say which
#' group moved, nor in which direction: it mixes both groups' shares with rates
#' that are themselves changing. This index holds the rates fixed at their
#' pre-pandemic level, pooled across the two groups, and lets only group g's
#' allocation move:
#'
#'     R_g = sum_k (s_gk,mid - s_gk,pre) * mbar_k,pre .
#'
#' R_g > 0 means group g moved towards cells that were riskier before the
#' pandemic. It is the statement "graduates did not reallocate into riskier
#' cells" made checkable, which the decomposition on its own is not.
reallocation_index <- function(agg, q_pre, q_mid) {
  a <- agg[, .(sw = sum(sw), swy = sum(swy)), by = .(qtr, college, cell)]

  base <- a[qtr %in% q_pre, .(sw = sum(sw), swy = sum(swy)), by = cell]
  base[, mbar_pre := swy / sw]

  shares <- function(qs) {
    z <- a[qtr %in% qs, .(sw = sum(sw)), by = .(college, cell)]
    z[, s := sw / sum(sw), by = college][, .(college, cell, s)]
  }
  s_pre <- shares(q_pre); s_mid <- shares(q_mid)

  m <- merge(merge(s_pre, s_mid, by = c("college", "cell"), all = TRUE,
                   suffixes = c("_pre", "_mid")),
             base[, .(cell, mbar_pre)], by = "cell", all.x = TRUE)
  m[is.na(s_pre), s_pre := 0][is.na(s_mid), s_mid := 0]
  m <- m[!is.na(mbar_pre)]
  m[, .(R = sum((s_mid - s_pre) * mbar_pre)), by = college]
}

#' Aggregate quarterly decomposition components into periods.
decomp_periods <- function(dec, qweight, periods) {
  dt <- merge(dec, qweight, by = "qtr")
  rbindlist(lapply(names(periods), function(nm) {
    z <- dt[qtr %in% periods[[nm]]]
    # Under the two-way bootstrap a quarter can be left out of the resample
    # entirely. Returning NULL would silently shorten the replication, so the
    # period is returned as NA and dropped at the aggregation step instead.
    if (!nrow(z) || sum(z$sw) <= 0)
      return(data.table(period = nm, composition = NA_real_,
                        within = NA_real_, gap_raw = NA_real_))
    om <- z$sw / sum(z$sw)
    data.table(period      = nm,
               composition = sum(om * z$composition),
               within      = sum(om * z$within),
               gap_raw     = sum(om * z$gap_raw))
  }))
}

#' Two-way cluster bootstrap for the decomposition.
#'
#' An earlier version drew one exponential multiplier per PSU-by-quarter
#' intersection. That is the one thing this bootstrap must not do: it treats the
#' same PSU in different quarters, and every PSU within a quarter, as
#' independent, which removes exactly the two dependences that motivate the
#' two-way clustering of the main specification. The intervals then looked as if
#' they priced in aggregate quarterly shocks while the procedure had assumed
#' them away.
#'
#' We instead use the two-way pigeonhole bootstrap: PSUs and quarters are
#' resampled independently with replacement and an observation's weight is
#' multiplied by both multiplicities. A quarter drawn twice scales every PSU in
#' it, which is what preserves the common time shock; a PSU drawn twice scales
#' it in every quarter it appears in. Multiplicities are used as weights so no
#' resampled copy of the microdata is ever materialised.
decompose_boot <- function(agg, qweight, periods, B = B_DECOMP, seed = SEED) {
  set.seed(seed)
  psu_ids <- unique(agg$psu)
  qtr_ids <- unique(agg$qtr)
  a <- copy(agg)

  keep <- rbindlist(lapply(seq_len(B), function(b) {
    mp <- tabulate(match(sample(psu_ids, length(psu_ids), replace = TRUE),
                         psu_ids), nbins = length(psu_ids))
    mt <- tabulate(match(sample(qtr_ids, length(qtr_ids), replace = TRUE),
                         qtr_ids), nbins = length(qtr_ids))
    mult <- mp[match(a$psu, psu_ids)] * mt[match(a$qtr, qtr_ids)]
    ab <- a[mult > 0, .(qtr, college, cell,
                        sw  = sw  * mult[mult > 0],
                        swy = swy * mult[mult > 0])]
    qw <- ab[, .(sw = sum(sw)), by = qtr]
    dd <- decomp_periods(decompose_from_cells(ab), qw, periods)
    dd[, rep := b][]
  }))

  # A single-quarter period (the onset) is a degenerate object under a bootstrap
  # that resamples quarters: it survives only in the draws that happen to
  # include that one quarter, and its spread then reflects the PSU draw alone.
  # n_rep records this so 06_decomposition.R can suppress the interval.
  qq <- function(x, p) if (all(is.na(x))) NA_real_ else
    stats::quantile(x, p, names = FALSE, na.rm = TRUE)
  keep[, .(n_rep          = sum(!is.na(gap_raw)),
           composition_se = stats::sd(composition, na.rm = TRUE),
           within_se      = stats::sd(within, na.rm = TRUE),
           gap_se         = stats::sd(gap_raw, na.rm = TRUE),
           composition_lo = qq(composition, 0.025),
           composition_hi = qq(composition, 0.975),
           within_lo      = qq(within, 0.025),
           within_hi      = qq(within, 0.975)),
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
