# =============================================================================
# _config.R
# Project-wide configuration: paths, seeds, options, plotting theme.
# Sourced by every script; never run on its own.
#
# Project: Education and Employment Exits During COVID-19: Evidence from Brazil
# =============================================================================

# ---- Reproducibility --------------------------------------------------------
# Single global seed used by every stochastic routine in the project
# (wild cluster bootstrap, multiplier bootstrap for simultaneous bands,
#  cluster bootstrap for the decomposition).
SEED <- 20260615L
set.seed(SEED)

# Bootstrap replications
B_WILD  <- 9999L   # wild cluster bootstrap p-values, clustered by year-quarter
B_SUPT  <- 10000L  # multiplier bootstrap for sup-t simultaneous bands
B_DECOMP <- 2000L  # two-way pigeonhole bootstrap for the decomposition. 500
                   # was thin for 95% percentile intervals; the two-way draw
                   # also loses the replications that miss a period's quarters.

options(
  stringsAsFactors = FALSE,
  scipen           = 999,
  datatable.print.class = TRUE,
  width            = 110
)

# ---- Paths ------------------------------------------------------------------
# ROOT is the git repository. Raw/large data live in Dropbox and are never
# committed; see README.md for the expected Dropbox layout.

user <- Sys.info()[["user"]]

ROOT <- switch(
  user,
  "fredie"       = "/Users/fredie/Documents/GitHub/Monografia",
  "Francisco"    = "C:/Users/Francisco/Dropbox/Research/Monografia-Fredie",
  "f.cavalcanti" = "C:/Users/f.cavalcanti/Documents/GitHub/Monografia",
  "DELL"         = "D:/OneDrive/Documentos/GitHub/Monografia-Fredie",
  # Fallback: assume the working directory is already the repository root
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
)

DROPBOX <- switch(
  user,
  "fredie"       = "/Users/fredie/Library/CloudStorage/Dropbox/Education_and_Job_Loss_During_COVID_19",
  "Francisco"    = "C:/Users/Francisco/Dropbox/Education_and_Job_Loss_During_COVID_19",
  "f.cavalcanti" = "C:/Users/f.cavalcanti/Dropbox/Education_and_Job_Loss_During_COVID_19",
  "DELL"         = "D:/Dropbox/Education_and_Job_Loss_During_COVID_19",
  file.path(ROOT, "build", "output")
)

# ---- Micro-data (Dropbox only, never in git) --------------------------------
DIR_DATA <- file.path(DROPBOX, "build", "output")

# Person-quarter file produced by the current build (build/code/10-12), using
# the datazoom.social stage-3 ("advanced_3") panel identification.
RAW_PARQUET <- file.path(DIR_DATA, "main_data.parquet")

# Legacy person-quarter file from the earlier Stata/Data Zoom build. Retained
# for provenance and for the vintage comparison in 08_robustness.R; it is the
# source of the results circulated before the stage-3 rebuild.
RAW_DTA <- file.path(DIR_DATA, "main_data.dta")

# Which vintage the analysis reads. Set to "legacy" to reproduce the earlier
# results from RAW_DTA.
DATA_VINTAGE <- Sys.getenv("MONOGRAFIA_VINTAGE", "stage3")

# Analysis-ready data (parquet). Written by 01_prepare_analysis_data.R.
# Lives in Dropbox, not in the repository: it is large and fully derived.
ANALYSIS_PQ <- file.path(
  DIR_DATA,
  if (identical(DATA_VINTAGE, "legacy")) "analysis_sample_legacy.parquet"
  else "analysis_sample.parquet"
)

# Small, committed inputs (matching diagnostics).
DIR_INPUT <- file.path(ROOT, "analysis", "input")

# The analysis scripts themselves, so the master script can compare an output's
# timestamp against the code that produced it, not only against its input.
DIR_CODE  <- file.path(ROOT, "analysis", "code")

# Outputs consumed by latex/paper.tex
DIR_TABLES  <- file.path(ROOT, "analysis", "output", "tables")
DIR_FIGURES <- file.path(ROOT, "analysis", "output", "figures")
DIR_LOGS    <- file.path(ROOT, "analysis", "output", "logs")
DIR_EST     <- file.path(ROOT, "analysis", "output", "estimates")

for (d in c(DIR_INPUT, DIR_TABLES, DIR_FIGURES, DIR_LOGS, DIR_EST)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---- Analysis parameters ----------------------------------------------------

# Reference quarter for the event-study interactions. 2019Q4 is the last
# quarter whose t+1 outcome (2020Q1) is still measured before the pandemic
# shock hits the labour market; using it as the base makes every reported
# contrast a deviation from the immediate pre-pandemic gap.
REF_QUARTER <- 20194L

# Pandemic windows used in the compact interaction model (Table 2).
Q_ONSET <- 20201L                                   # onset
Q_MID   <- c(20202L, 20203L, 20204L,
             20211L, 20212L, 20213L)                # mid-pandemic

# Pre-pandemic benchmark period used to scale effects into relative magnitudes.
Q_PRE <- 20121L:20194L

# Prime-age robustness window
AGE_PRIME <- c(25L, 54L)

# ---- Plotting ---------------------------------------------------------------
# Colour-blind safe, prints legibly in grayscale (Elsevier prints figures in
# black and white unless the authors pay for colour).
COL_NOCOLLEGE <- "#1B6CA8"   # blue
COL_COLLEGE   <- "#D1495B"   # red
COL_NEUTRAL   <- "#4D4D4D"
COL_SHADE     <- "grey85"

FIG_W <- 7.2   # inches, full text width at Elsevier single-column scaling
FIG_H <- 4.8
FIG_H_GRID <- 6.6   # taller, for the two-by-two panel grids

# Legends sit under the plot and are set larger than the axis text: the figures
# are reduced substantially in the printed article, and the series legend is
# what a reader needs to resolve first.
theme_paper <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_line(linewidth = 0.25, colour = "grey90"),
      panel.border       = ggplot2::element_rect(colour = "grey40", linewidth = 0.4),
      panel.spacing      = ggplot2::unit(1.1, "lines"),
      strip.background   = ggplot2::element_rect(fill = "grey95", colour = "grey40",
                                                 linewidth = 0.4),
      strip.text         = ggplot2::element_text(face = "bold", size = base_size + 1),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = base_size + 2),
      legend.key         = ggplot2::element_blank(),
      legend.key.width   = ggplot2::unit(2.2, "lines"),
      legend.key.height  = ggplot2::unit(1.2, "lines"),
      legend.background  = ggplot2::element_blank(),
      legend.margin      = ggplot2::margin(t = 2, b = 0),
      legend.box.spacing = ggplot2::unit(0.5, "lines"),
      axis.text          = ggplot2::element_text(size = base_size - 1),
      axis.title         = ggplot2::element_text(size = base_size),
      plot.title         = ggplot2::element_text(face = "bold", size = base_size),
      plot.caption       = ggplot2::element_text(size = base_size - 3, colour = "grey30",
                                                 hjust = 0)
    )
}

# ---- Small helpers ----------------------------------------------------------

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# Integer YYYYQ (e.g. 20201) -> "2020Q1"
qlab <- function(yq) paste0(yq %/% 10L, "Q", yq %% 10L)

# Shared plotting theme and figure helpers.
source(file.path(ROOT, "analysis", "code", "_figures.R"))

# Integer YYYYQ -> decimal year at the *end* of the quarter, for plotting on a
# continuous axis (2020Q1 -> 2020.25).
qnum <- function(yq) (yq %/% 10L) + (yq %% 10L) / 4
