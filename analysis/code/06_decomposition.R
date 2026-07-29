# =============================================================================
# 06_decomposition.R
#
# Comment 6. Splits the raw education gap in employment exits into
#
#   Gap_q = sum_k (s_Ckq - s_Nkq) m_Nkq  +  sum_k s_Ckq (m_Ckq - m_Nkq)
#           \_______ composition _______/    \_______ within-cell _______/
#
# where k indexes cells defined by formality x sector x occupation, s_gkq is
# the survey-weighted share of education group g in cell k in quarter q and
# m_gkq is that group's exit rate in the cell.
#
# The question the decomposition answers is the one the paper needs: is the
# mid-pandemic reversal of the education gradient driven by where the two
# groups work (composition), or by the two groups facing different exit risks
# inside the same kind of job (within-cell)?
#
#   Figure 5  Quarterly decomposition.
#   Table 4   Period-level decomposition, with PSU cluster bootstrap intervals.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(ggplot2)
})

if (!exists("ROOT"))         source(file.path("analysis", "code", "_config.R"))
if (!exists("decomp_cells")) source(file.path("analysis", "code", "_functions.R"))

d <- as.data.table(read_parquet(ANALYSIS_PQ))

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

CELL_MAIN <- c("formal", "sector", "occupation")
CELL_ALT  <- c("formal", "position_grp", "sector", "occupation")

run_decomp <- function(cell_vars, tag) {
  msg("decomposition over cells: ", paste(cell_vars, collapse = " x "))
  agg <- decomp_cells(d, cell_vars, "exit")
  msg("  ", fmt_n(nrow(agg)), " PSU-quarter-education-cell totals; ",
      fmt_n(uniqueN(agg$cell)), " distinct cells")

  qw  <- agg[, .(sw = sum(sw)), by = qtr]
  dq  <- decompose_from_cells(agg)
  dp  <- decomp_periods(dq, qw, PERIODS)

  msg("  bootstrapping (", B_DECOMP, " PSU-level replications) ...")
  t0 <- Sys.time()
  bs <- decompose_boot(agg, qw, PERIODS, B = B_DECOMP, seed = SEED)
  msg("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")

  out <- merge(dp, bs, by = "period", sort = FALSE)
  fwrite(dq, file.path(DIR_EST, sprintf("decomposition_quarterly_%s.csv", tag)))
  fwrite(out, file.path(DIR_EST, sprintf("decomposition_periods_%s.csv", tag)))
  list(quarterly = dq, periods = out, ncell = uniqueN(agg$cell))
}

main <- run_decomp(CELL_MAIN, "main")
alt  <- run_decomp(CELL_ALT,  "alt")

# -----------------------------------------------------------------------------
# Figure 5
# -----------------------------------------------------------------------------
dq <- copy(main$quarterly)
dq[, date := qdate(qtr)]

long <- melt(dq, id.vars = "date",
             measure.vars = c("composition", "within", "gap_raw"),
             variable.name = "component", value.name = "value")
long[, component := factor(component,
      levels = c("gap_raw", "within", "composition"),
      labels = c("Total gap", "Within-cell", "Composition"))]

p_dec <- ggplot(long, aes(date, value, colour = component, linetype = component)) +
  pandemic_layers() +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey30") +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = c("Total gap"   = "black",
                                 "Within-cell" = COL_COLLEGE,
                                 "Composition" = COL_NOCOLLEGE)) +
  scale_linetype_manual(values = c("Total gap"   = "solid",
                                   "Within-cell" = "21",
                                   "Composition" = "41")) +
  x_years(long$date) + y_prob() +
  labs(x = NULL,
       y = "College minus non-college gap\nin employment-exit probability") +
  theme_paper()

save_fig(p_dec, "fig_decomposition")

# -----------------------------------------------------------------------------
# Table 4
# -----------------------------------------------------------------------------
f3 <- function(x) fmt_num(x, 3)
# The within-cell share is a percentage; three decimals on it claimed a precision
# the bootstrap intervals do not support and clashed with the whole numbers the
# text quotes for the same quantity.
f1 <- function(x) fmt_num(x, 1)

blk <- function(res, title) {
  r <- res$periods
  c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", title),
    unlist(lapply(seq_len(nrow(r)), function(i) {
      z <- r[i]
      c(sprintf("\\quad %s & %s & %s & %s & %s \\\\", z$period,
                fmt_est(z$gap_raw, z$gap_se),
                fmt_est(z$composition, z$composition_se),
                fmt_est(z$within, z$within_se),
                f1(100 * z$within / z$gap_raw)),
        sprintf(" & & [%s, %s] & [%s, %s] & \\\\",
                f3(z$composition_lo), f3(z$composition_hi),
                f3(z$within_lo), f3(z$within_hi)))
    })))
}

write_tex(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Composition versus within-cell decomposition of the education gap}",
  "\\label{tab:decomposition}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& Total gap & Composition & Within-cell & Within-cell \\\\",
  "Period & $\\Delta_q$ & component & component & share (\\%) \\\\",
  "\\midrule",
  blk(main, sprintf("Cells: formality $\\times$ sector $\\times$ occupation (%d cells)",
                    main$ncell)),
  "\\addlinespace",
  blk(alt, sprintf("Cells: formality $\\times$ position $\\times$ sector $\\times$ occupation (%d cells)",
                   alt$ncell)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: The total gap is the survey-weighted difference in the ",
         "raw employment-exit rate between workers with and without a college ",
         "degree. The composition component holds each cell's non-college exit ",
         "rate fixed and varies only the two groups' allocation across cells; the ",
         "within-cell component holds the college allocation fixed and varies only ",
         "the exit rates inside cells. Period figures are averages of the quarterly ",
         "components weighted by the quarter's share of the total survey weight. ",
         "Brackets report 95\\% percentile intervals from ", B_DECOMP,
         " bootstrap replications that draw exponential multipliers at the ",
         "primary-sampling-unit-by-quarter level (seed ", SEED, "). Stars denote ",
         "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$ on the bootstrap standard ",
         "error of the entry. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_decomposition.tex"))

msg("06_decomposition.R done.")
