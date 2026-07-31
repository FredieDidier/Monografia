# =============================================================================
# 06_decomposition.R
#
# Splits the unconditional education gap in employment exits into
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

# Size of the full crossing, for the table note. `formal` is nested inside
# `position_grp` (every position is already labelled formal or informal), so
# multiplying the two would count each combination twice.
n_cross <- function(vars) {
  if (all(c("formal", "position_grp") %in% vars)) vars <- setdiff(vars, "formal")
  prod(vapply(vars, function(v) {
    x <- d[[v]]
    if (is.factor(x)) nlevels(x) else uniqueN(x)
  }, numeric(1)))
}

run_decomp <- function(cell_vars, tag) {
  msg("decomposition over cells: ", paste(cell_vars, collapse = " x "))
  agg <- decomp_cells(d, cell_vars, "exit")
  msg("  ", fmt_n(nrow(agg)), " PSU-quarter-education-cell totals; ",
      fmt_n(uniqueN(agg$cell)), " distinct cells")

  qw  <- agg[, .(sw = sum(sw)), by = qtr]
  dq  <- decompose_from_cells(agg)
  dp  <- decomp_periods(dq, qw, PERIODS)

  # Cells occupied by only one education group have no counterfactual rate for
  # the other, and decompose_from_cells() carries the missing rate as zero. The
  # gap identity still closes, because the absent group's share is zero, but the
  # split between the two components is arbitrary in those cells: the whole
  # contribution lands on the within-cell term. Report how much weight that is,
  # so the reader can see the decomposition is not resting on it.
  sup <- agg[, .(sw = sum(sw)), by = .(qtr, college, cell)]
  sup <- dcast(sup, qtr + cell ~ college, value.var = "sw", fill = 0)
  setnames(sup, c("0", "1"), c("sw_N", "sw_C"), skip_absent = TRUE)
  one_group <- sup[sw_N == 0 | sw_C == 0, sum(sw_N + sw_C)] /
               sup[, sum(sw_N + sw_C)]
  msg("  weight in cells observed for one education group only: ",
      formatC(100 * one_group, format = "f", digits = 2), "%")

  msg("  two-way bootstrap (", B_DECOMP, " replications, PSU x quarter) ...")
  t0 <- Sys.time()
  bs <- decompose_boot(agg, qw, PERIODS, B = B_DECOMP, seed = SEED)
  msg("  done in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")

  out <- merge(dp, bs, by = "period", sort = FALSE)
  fwrite(dq, file.path(DIR_EST, sprintf("decomposition_quarterly_%s.csv", tag)))
  fwrite(out, file.path(DIR_EST, sprintf("decomposition_periods_%s.csv", tag)))
  list(quarterly = dq, periods = out, ncell = uniqueN(agg$cell),
       one_group = one_group)
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

# The onset period is a single quarter. Resampling quarters cannot say anything
# about the uncertainty of a one-quarter average -- the draws that retain 2020Q1
# differ only in their PSU draw -- so its point estimates are reported without
# an interval or stars rather than with ones that would overstate what is known.
SINGLE_Q <- names(PERIODS)[lengths(PERIODS) == 1L]

blk <- function(res, title) {
  r <- res$periods
  c(sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", title),
    unlist(lapply(seq_len(nrow(r)), function(i) {
      z <- r[i]
      if (z$period %in% SINGLE_Q)
        return(sprintf("\\quad %s & %s & %s & %s & %s \\\\", z$period,
                       f3(z$gap_raw), f3(z$composition), f3(z$within),
                       f1(100 * z$within / z$gap_raw)))
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
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Composition versus within-cell decomposition of the education gap}",
  "\\label{tab:decomposition}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& Total gap & Composition & Within-cell & Within-cell \\\\",
  "Period & $\\Delta^{\\mathrm{raw}}_q$ & component & component & share (\\%) \\\\",
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
         "A cell is a combination of the listed variables that some worker ",
         "occupies: formality is the two-way formal / informal split, sector the ",
         "five broad activity groups, occupation the ten COD major groups and ",
         "position the eight labour-market categories of ",
         "Table~\\ref{tab:heterogeneity} (private employee, self-employed, ",
         "employer and public sector, each formal or informal). The cell counts ",
         "are the combinations actually observed; the remaining ",
         sprintf("%s and %s", fmt_n(n_cross(CELL_MAIN) - main$ncell),
                              fmt_n(n_cross(CELL_ALT)  - alt$ncell)),
         " combinations the crossings allow are empty. ",
         sprintf("Cells occupied by only one education group hold %.1f%% and %.1f%% ",
                 100 * main$one_group, 100 * alt$one_group),
         "of the survey weight under the two definitions; they have no ",
         "counterfactual rate for the absent group, so their whole contribution ",
         "falls on the within-cell component. Brackets report 95\\% percentile ",
         "intervals from ", B_DECOMP, " replications of a two-way pigeonhole ",
         "bootstrap that resamples primary sampling units and quarters ",
         "independently. Stars denote ",
         "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$ on the bootstrap standard ",
         "error of the entry. The onset is a single quarter, for which a ",
         "bootstrap over quarters carries no information, so it is reported ",
         "without an interval. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_decomposition.tex"))

# -----------------------------------------------------------------------------
# Reference choice, common support and reallocation
# -----------------------------------------------------------------------------
# The headline decomposition fixes one reference and keeps every cell. Three
# questions follow, and the paper should answer them rather than leave them to a
# referee: does the split survive the opposite convention, does it survive
# dropping the cells only one group occupies, and -- the claim the decomposition
# cannot make on its own -- which group actually reallocated.
agg_main <- decomp_cells(d, CELL_MAIN, "exit")
qw_main  <- agg_main[, .(sw = sum(sw)), by = qtr]

VARIANTS <- list(
  "Baseline (non-college rates, college shares)" =
    list(ref = "noncollege", cs = FALSE),
  "College rates as reference"                   =
    list(ref = "college",    cs = FALSE),
  "Symmetric (average rates and shares)"         =
    list(ref = "symmetric",  cs = FALSE),
  "Common support only"                          =
    list(ref = "noncollege", cs = TRUE)
)

var_tab <- rbindlist(lapply(names(VARIANTS), function(nm) {
  v  <- VARIANTS[[nm]]
  dq <- decompose_variant(agg_main, ref = v$ref, common_support = v$cs)
  dp <- decomp_periods(dq, qw_main, PERIODS)
  dp[, variant := nm][]
}))
fwrite(var_tab, file.path(DIR_EST, "decomposition_variants.csv"))

reloc <- reallocation_index(agg_main,
                            q_pre = QUARTERS[QUARTERS <= 20194L],
                            q_mid = Q_MID)
fwrite(reloc, file.path(DIR_EST, "reallocation_index.csv"))
msg("reallocation index  R_college = ", fmt_num(reloc[college == 1L, R], 5),
    "   R_noncollege = ", fmt_num(reloc[college == 0L, R], 5))

ROWS <- c("Pre-pandemic", "Mid-pandemic")
write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Decomposition under alternative references and on common support}",
  "\\label{tab:decomp_variants}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Pre-pandemic} & \\multicolumn{2}{c}{Mid-pandemic} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Convention & Composition & Within-cell & Composition & Within-cell \\\\",
  "\\midrule",
  unlist(lapply(names(VARIANTS), function(nm) {
    z <- var_tab[variant == nm]
    g <- function(p, col) f3(z[period == p][[col]])
    sprintf("%s & %s & %s & %s & %s \\\\", nm,
            g("Pre-pandemic", "composition"), g("Pre-pandemic", "within"),
            g("Mid-pandemic", "composition"), g("Mid-pandemic", "within"))
  })),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Reallocation index, pre-pandemic to mid-pandemic}} \\\\",
  sprintf("\\quad Graduates ($R_C$) & \\multicolumn{4}{c}{%s} \\\\",
          fmt_num(reloc[college == 1L, R], 5)),
  sprintf("\\quad Non-graduates ($R_N$) & \\multicolumn{4}{c}{%s} \\\\",
          fmt_num(reloc[college == 0L, R], 5)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Cells are formality $\\times$ sector $\\times$ ",
         "occupation throughout. The baseline row is the convention of ",
         "equation~\\eqref{eq:decomp}; the next two replace it with the opposite ",
         "and the symmetric convention. The last row keeps only cells that both ",
         "education groups occupy, renormalising each group's shares within the ",
         "quarter, so its total gap differs from the others by the amount the ",
         "dropped cells carried. The reallocation index is ",
         "$R_g = \\sum_k (s_{gk,\\text{mid}} - s_{gk,\\text{pre}})\\bar m_{k,\\text{pre}}$, ",
         "with $\\bar m_{k,\\text{pre}}$ the pre-pandemic exit rate of cell $k$ ",
         "pooled across groups: it holds rates fixed and lets only allocation ",
         "move, so $R_g > 0$ means group $g$ shifted towards cells that were ",
         "riskier before the pandemic. ", PERIOD_NOTE),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_decomp_variants.tex"))

msg("06_decomposition.R done.")
