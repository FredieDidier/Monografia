# =============================================================================
# 04_figures_main.R
#
# fig_levels  Adjusted employment-exit probability by education.
# fig_gap     College minus non-college gap, with pointwise and simultaneous
#             (sup-t) confidence bands.
#
# Both are built from the survey-weighted average predictive margins saved by
# 03_main_estimation.R, not from raw regression coefficients.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))
if (!exists("qlab")) source(file.path("analysis", "code", "_functions.R"))

am <- fread(file.path(DIR_EST, "adjusted_margins_quarterly.csv"))
am[, date := qdate(qtr)]

# -----------------------------------------------------------------------------
# Adjusted levels
# -----------------------------------------------------------------------------
lv <- rbind(
  am[, .(date, group = "No college degree", est = m_nocol,
         lo = m_nocol_lo, hi = m_nocol_hi)],
  am[, .(date, group = "College degree", est = m_col,
         lo = m_col_lo, hi = m_col_hi)]
)
lv[, group := factor(group, levels = EDU_LEVELS)]

p1 <- ggplot(lv, aes(date, est, colour = group, fill = group, linetype = group)) +
  pandemic_layers() +
  geom_ribbon(aes(ymin = lo, ymax = hi), colour = NA, alpha = 0.20) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = EDU_COLOURS) +
  scale_fill_manual(values = EDU_COLOURS) +
  scale_linetype_manual(values = EDU_LINES) +
  x_years(lv$date) + y_prob() +
  labs(x = NULL, y = "Adjusted probability of employment exit (t to t+1)") +
  theme_paper()

save_fig(p1, "fig_levels")

# -----------------------------------------------------------------------------
# The gap, pointwise vs simultaneous bands
# -----------------------------------------------------------------------------
p2 <- ggplot(am, aes(date, gap)) +
  pandemic_layers() +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey30") +
  geom_ribbon(aes(ymin = gap_lo_sim, ymax = gap_hi_sim),
              fill = COL_NOCOLLEGE, alpha = 0.13) +
  geom_ribbon(aes(ymin = gap_lo, ymax = gap_hi),
              fill = COL_NOCOLLEGE, alpha = 0.28) +
  geom_line(linewidth = 0.85, colour = COL_NOCOLLEGE) +
  x_years(am$date) + y_prob() +
  labs(x = NULL,
       y = "College minus non-college gap\nin employment-exit probability") +
  theme_paper() +
  theme(legend.position = "none")

save_fig(p2, "fig_gap")

# -----------------------------------------------------------------------------
# Numbers quoted in the text
# -----------------------------------------------------------------------------
fwrite(am[, .(quarter, m_nocol, m_col, gap, gap_se, gap_lo, gap_hi,
              gap_lo_sim, gap_hi_sim)],
       file.path(DIR_EST, "fig_data_main.csv"))

sig_pt  <- am[gap_lo > 0 | gap_hi < 0, quarter]
sig_sim <- am[gap_lo_sim > 0 | gap_hi_sim < 0, quarter]

writeLines(c(
  "Quarters with a positive gap (college exits more), point estimate:",
  paste("  ", paste(am[gap > 0, quarter], collapse = ", ")),
  "",
  "Quarters where the gap is positive and the simultaneous band excludes zero:",
  paste("  ", paste(am[gap_lo_sim > 0, quarter], collapse = ", ")),
  "",
  sprintf("Pointwise-significant quarters: %d of %d", length(sig_pt), nrow(am)),
  sprintf("Simultaneously significant quarters: %d of %d", length(sig_sim), nrow(am)),
  sprintf("sup-t critical value: %.3f", am$crit_sim[1])
), file.path(DIR_LOGS, "04_figure_notes.txt"))

msg("04_figures_main.R done.")
