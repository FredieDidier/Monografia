# =============================================================================
# 05_heterogeneity.R
#
# Re-estimates the event study inside labour market segments and demographic
# groups, and reports the adjusted college gap for each.
#
#   Figure 3  Formal vs informal employment: adjusted levels by education.
#   Figure 4  Adjusted college gap by gender, race and labour market position.
#   Table 3   Period-level adjusted gaps by segment.
#
# Every subgroup is estimated on its own subsample, so the covariates and fixed
# effects are re-balanced within the segment rather than imposed from the
# pooled model. Fixed effects and controls that are collinear inside a segment
# are dropped automatically.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest); library(ggplot2)
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

# -----------------------------------------------------------------------------
# Segment definitions
# -----------------------------------------------------------------------------
# `drop` lists regressors that are constant (hence collinear) inside a segment.
SEGMENTS <- list(
  list(key = "formal",   dim = "Formality",  lab = "Formal",
       sub = quote(formal == 1L),  drop = "formal"),
  list(key = "informal", dim = "Formality",  lab = "Informal",
       sub = quote(formal == 0L),  drop = "formal"),

  list(key = "men",   dim = "Gender", lab = "Men",
       sub = quote(female == 0L), drop_fe = "female"),
  list(key = "women", dim = "Gender", lab = "Women",
       sub = quote(female == 1L), drop_fe = "female"),

  # Race is split white vs non-white (everyone who does not report "branca"),
  # rather than white vs black-or-brown, so the two panels partition the sample.
  list(key = "white",    dim = "Race", lab = "White",
       sub = quote(white == 1L), drop_fe = "race5"),
  list(key = "nonwhite", dim = "Race", lab = "Non-White",
       sub = quote(white == 0L), drop_fe = "race5"),

  list(key = "pos_formal_private", dim = "Position",
       lab = "Formal private employee",
       sub = quote(position_grp == "Formal private employee"), drop = "formal"),
  list(key = "pos_informal_private", dim = "Position",
       lab = "Informal private employee",
       sub = quote(position_grp == "Informal private employee"), drop = "formal"),
  list(key = "pos_formal_public", dim = "Position",
       lab = "Formal public sector",
       sub = quote(position_grp == "Formal public sector"), drop = "formal"),
  list(key = "pos_formal_self", dim = "Position",
       lab = "Formal self-employed",
       sub = quote(position_grp == "Formal self-employed"), drop = "formal"),
  list(key = "pos_informal_self", dim = "Position",
       lab = "Informal self-employed",
       sub = quote(position_grp == "Informal self-employed"), drop = "formal")
)

fit_segment <- function(seg) {
  f <- file.path(DIR_EST, sprintf("seg_%s.rds", seg$key))
  if (file.exists(f)) { msg("  cached: ", seg$lab); return(readRDS(f)) }

  s <- d[eval(seg$sub)]
  ctrl <- setdiff(CONTROLS, seg$drop %||% character(0))
  fe   <- setdiff(FE_FULL,  seg$drop_fe %||% character(0))
  msg("  fitting ", seg$lab, " (", fmt_n(nrow(s)), " obs) ...")
  t0 <- Sys.time()
  m  <- fit_es(s, outcome = "exit", cluster = ~ psu + qtr,
               controls = ctrl, fe = fe)
  am <- adjusted_margins(m, s)
  pm <- period_margins(m, s, PERIODS)
  out <- list(key = seg$key, dim = seg$dim, lab = seg$lab,
              n = nrow(s), am = am, pm = pm,
              crit = am$crit_sim[1])
  msg("    done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
  saveRDS(out, f, compress = FALSE)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

res <- lapply(SEGMENTS, fit_segment)
names(res) <- vapply(res, `[[`, character(1), "key")

am_all <- rbindlist(lapply(res, function(r)
  cbind(segment = r$lab, dim = r$dim, r$am)))
fwrite(am_all, file.path(DIR_EST, "adjusted_margins_by_segment.csv"))

pm_all <- rbindlist(lapply(res, function(r)
  cbind(segment = r$lab, dim = r$dim, n_seg = r$n, r$pm)))
fwrite(pm_all, file.path(DIR_EST, "period_margins_by_segment.csv"))

# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
am_all[, date := qdate(qtr)]

#' Two education series (levels) for a set of segments, as a facetted figure.
levels_plot <- function(dd, labs) {
  lv <- rbind(
    dd[, .(panel = segment, date, group = "No college degree",
           est = m_nocol, lo = m_nocol_lo, hi = m_nocol_hi)],
    dd[, .(panel = segment, date, group = "College degree",
           est = m_col,   lo = m_col_lo,   hi = m_col_hi)]
  )
  lv[, group := factor(group, levels = EDU_LEVELS)]
  lv[, panel := factor(panel, levels = labs)]

  ggplot(lv, aes(date, est, colour = group, fill = group, linetype = group)) +
    pandemic_layers(linewidth = 0.35) +
    geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.20) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(values = EDU_COLOURS) +
    scale_fill_manual(values = EDU_COLOURS) +
    scale_linetype_manual(values = EDU_LINES) +
    x_years(lv$date, step = 2L) + y_prob() +
    labs(x = NULL, y = "Adjusted probability of employment exit (t to t+1)") +
    theme_paper()
}

# --- Formality: two panels side by side --------------------------------------
FORMALITY <- c("Formal", "Informal")
p_form <- levels_plot(am_all[dim == "Formality"], FORMALITY) +
  facet_wrap(~ panel, nrow = 1, scales = "free_y")
save_fig(p_form, "fig_formal_informal")

# --- Demographics: (a) Men (b) Women (c) White (d) Non-White -----------------
DEMOG <- c("Men", "Women", "White", "Non-White")
p_demog <- grid_2x2(levels_plot(am_all[segment %in% DEMOG], DEMOG), DEMOG)
save_fig(p_demog, "fig_by_demographics", h = FIG_H_GRID)

# --- Position: (a)-(d) employees and self-employed, by formality -------------
POSITION <- c("Formal private employee", "Informal private employee",
              "Formal self-employed", "Informal self-employed")
p_pos <- grid_2x2(levels_plot(am_all[segment %in% POSITION], POSITION), POSITION)
save_fig(p_pos, "fig_by_position", h = FIG_H_GRID)

# -----------------------------------------------------------------------------
# Table 3: period gaps by segment
# -----------------------------------------------------------------------------
f3 <- function(x) fmt_num(x, 3)
wide <- dcast(pm_all, dim + segment + n_seg ~ period,
              value.var = c("gap", "gap_se"))
setcolorder(wide, c("dim", "segment", "n_seg"))

pnames <- names(PERIODS)
rows <- character(0)
last_dim <- ""
for (i in seq_len(nrow(wide))) {
  r <- wide[i]
  if (r$dim != last_dim) {
    rows <- c(rows, sprintf("\\addlinespace\\multicolumn{6}{l}{\\textit{%s}} \\\\",
                            r$dim))
    last_dim <- r$dim
  }
  est <- vapply(pnames, function(p) r[[paste0("gap_", p)]],    numeric(1))
  se  <- vapply(pnames, function(p) r[[paste0("gap_se_", p)]], numeric(1))
  rows <- c(rows,
    sprintf("\\quad %s & %s & %s \\\\", r$segment,
            paste(fmt_est(est, se), collapse = " & "), fmt_n(r$n_seg)),
    sprintf(" & %s & \\\\", paste(fmt_se(se), collapse = " & ")))
}

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Adjusted college gap in employment exits, by labour market segment}",
  "\\label{tab:heterogeneity}",
  "\\begin{threeparttable}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lccccr}",
  "\\toprule",
  "Segment & Pre-pandemic & Onset & Mid-pandemic & Post-pandemic & Obs. \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Each cell is the survey-weighted average predictive ",
         "difference in the probability of exiting employment between workers ",
         "with and without a college degree, estimated on the indicated subsample ",
         "with the full specification of equation~(\\ref{eq:es}). Fixed effects ",
         "and controls that are collinear within a segment are dropped. Standard ",
         "errors in parentheses are two-way clustered by primary sampling unit and ",
         "year-quarter. Stars denote $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. ",
         PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_heterogeneity.tex"))

msg("05_heterogeneity.R done.")
