# =============================================================================
# _figures.R
# Shared plotting theme and helpers, so every figure in the paper uses the same
# colours, legend placement and panel conventions.
# Sourced by _config.R; never run on its own.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
})

# Education series: blue for no college, red for college, consistent everywhere.
EDU_COLOURS <- c("No college degree" = COL_NOCOLLEGE,
                 "College degree"    = COL_COLLEGE)
EDU_LINES   <- c("No college degree" = "solid",
                 "College degree"    = "21")
EDU_LEVELS  <- c("No college degree", "College degree")

# Pandemic annotations: shaded mid-pandemic window, dashed onset and end.
D_ONSET <- as.Date("2020-03-01")
D_MID_0 <- as.Date("2020-06-01")
D_MID_1 <- as.Date("2021-09-01")

pandemic_layers <- function(linewidth = 0.4) {
  list(
    annotate("rect", xmin = D_MID_0, xmax = D_MID_1, ymin = -Inf, ymax = Inf,
             fill = COL_SHADE, alpha = 0.55),
    geom_vline(xintercept = D_ONSET, linetype = "22",
               linewidth = linewidth, colour = COL_NEUTRAL),
    geom_vline(xintercept = D_MID_1, linetype = "22",
               linewidth = linewidth, colour = COL_NEUTRAL)
  )
}

#' Quarter integer (YYYYQ) to a plotting date at the last month of the quarter.
qdate <- function(yq) as.Date(sprintf("%d-%02d-01", yq %/% 10L, 3L * (yq %% 10L)))

#' Year ticks that stop at the last year the data actually cover.
#'
#' date_breaks = "1 year" places a tick on 1 January of every year, including
#' the one *after* the sample ends: with data through 2024Q4 (plotted at
#' December 2024) that produced a "2025" label sitting beyond every point and
#' clipped by the panel edge. Breaks are therefore built from the data range,
#' and the January tick that opens the first year is kept only if it falls
#' close enough to the start to stay inside the panel.
x_years <- function(dates, step = 1L, fmt = "%Y") {
  rng <- range(as.Date(dates), na.rm = TRUE)
  yrs <- seq(as.integer(format(rng[1], "%Y")),
             as.integer(format(rng[2], "%Y")), by = step)
  brk <- as.Date(sprintf("%d-01-01", yrs))
  brk <- brk[brk >= rng[1] - 100 & brk <= rng[2]]
  scale_x_date(breaks = brk, date_labels = fmt,
               expand = expansion(mult = c(0.02, 0.02)))
}

y_prob <- function() scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

#' Save a figure as both PDF (for LaTeX) and PNG (for quick inspection).
save_fig <- function(p, name, w = FIG_W, h = FIG_H) {
  ggsave(file.path(DIR_FIGURES, paste0(name, ".pdf")), p,
         width = w, height = h, device = cairo_pdf)
  ggsave(file.path(DIR_FIGURES, paste0(name, ".png")), p,
         width = w, height = h, dpi = 400)
  msg("wrote ", name, ".pdf/.png")
  invisible(p)
}

#' Two-by-two panel grid, each strip carrying the segment's own name.
#'
#' The names are the ones used in the tables, so a reader moving between a
#' figure and Table~\ref{tab:heterogeneity} sees the same wording for the same
#' group.
#'
#' @param p    a ggplot whose data has an ordered `panel` column
#' @param labs the four segment names, in display order
grid_2x2 <- function(p, labs) {
  p + facet_wrap(~ panel, ncol = 2, scales = "free_y")
}
