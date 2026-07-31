# =============================================================================
# 00_master_analysis.R
#
# Master script. Runs the whole analysis pipeline end to end and writes every
# table and figure consumed by latex/paper.tex.
#
# Usage, from the repository root:
#     Rscript "analysis/code/00_master_analysis.R"
#
# Prerequisites
#   * build/output/main_data.parquet in the Dropbox project folder, produced by
#     build/code/00_master_build.R; the path is resolved in _config.R.
#   * The R packages listed in README.md.
#
# Runtime is dominated by the fixed-effects regressions on 7.9 million
# observations. Every fitted model is cached under analysis/output/estimates,
# so a second run only rebuilds tables and figures. Delete that directory to
# force a full re-estimation.
#
# Project: Education and Employment Exits During COVID-19: Evidence from Brazil
#          Cavalcanti, Didier and Gonzaga
# =============================================================================

rm(list = ls())

t_start <- Sys.time()

# Resolve the repository root before anything else.
if (!file.exists(file.path("analysis", "code", "_config.R"))) {
  stop("Run this script from the repository root, ",
       "e.g. Rscript \"analysis/code/00_master_analysis.R\"")
}

source(file.path("analysis", "code", "_config.R"))
setwd(ROOT)
source(file.path("analysis", "code", "_functions.R"))

msg("ROOT    = ", ROOT)
msg("DROPBOX = ", DROPBOX)

STEPS <- c(
  # Build the analysis-ready parquet from the Stata source. Skipped when the
  # parquet already exists; delete it to force a rebuild.
  "01_prepare_analysis_data.R",
  # Descriptive statistics, sample composition, matching diagnostics table.
  "02_table_descriptive_statistics.R",
  # Event study, adjusted margins, clustered and bootstrap inference.
  "03_main_estimation.R",
  # Figures 1 and 2.
  "04_figures_main.R",
  # Segment-by-segment estimates: formality, gender, race, position.
  "05_heterogeneity.R",
  # Composition vs within-cell decomposition.
  "06_decomposition.R",
  # Panel retention, selection and inverse-retention weighting.
  "07_attrition.R",
  # Robustness table and validation of the closed-form margins.
  "08_robustness.R",
  # Aggregate-time inference: the realised window against placebo windows.
  "10_placebo_windows.R",
  # Common support between the education groups, and overlap-weighted estimates.
  "11_overlap_weights.R",
  # How far the unmatched would have to depart from missing-at-random.
  "12_tipping_point.R",
  # Every number quoted in the paper text, as LaTeX macros. Runs after 10-12 so
  # that their results are available as macros too.
  "09_paper_numbers.R",
  # Software environment for the replication package.
  "99_session_info.R"
)

# Source of the preparation step, so the skip test below can notice when the
# script itself changes and not only when its input does.
PREP_SRC <- file.path(DIR_CODE, "01_prepare_analysis_data.R")

# 01 is expensive and idempotent: skip it when its output is already there.
# Under the legacy vintage there is nothing to prepare -- the analysis file was
# produced by the superseded build and is read as-is.
if (identical(DATA_VINTAGE, "legacy")) {
  msg("legacy vintage: reading ", basename(ANALYSIS_PQ), " directly")
  STEPS <- setdiff(STEPS, "01_prepare_analysis_data.R")
} else if (file.exists(ANALYSIS_PQ) &&
           file.mtime(ANALYSIS_PQ) > file.mtime(RAW_PARQUET) &&
           file.mtime(ANALYSIS_PQ) > file.mtime(PREP_SRC)) {
  # Skip only when the analysis sample is newer than *both* the file it is
  # derived from and the script that derives it. Comparing against the data
  # alone was not enough: editing 01 (a new variable, a recoded control) left a
  # sample that predated the change looking current, and the whole analysis
  # then ran on it. Checking mere existence, the version before that, missed a
  # rebuilt main_data.parquet the same way.
  msg("analysis sample is newer than ", basename(RAW_PARQUET), " and ",
      basename(PREP_SRC), ", skipping 01 (delete ", basename(ANALYSIS_PQ),
      " to force a rebuild)")
  STEPS <- setdiff(STEPS, "01_prepare_analysis_data.R")
} else if (file.exists(ANALYSIS_PQ)) {
  msg("analysis sample is older than ", basename(RAW_PARQUET), " -- rebuilding")
}

# The loop variable is deliberately verbose: each step is source()d into the
# global environment, so a short name risks being clobbered by the step itself.
for (step_file in STEPS) {
  msg("======================================================================")
  msg("RUN  ", step_file)
  msg("======================================================================")
  step_t0 <- Sys.time()
  source(file.path(ROOT, "analysis", "code", step_file), echo = FALSE)
  msg("OK   ", step_file, "  (",
      round(as.numeric(Sys.time() - step_t0, units = "mins"), 1), " min)")
}

msg("======================================================================")
msg("Pipeline finished in ",
    round(as.numeric(Sys.time() - t_start, units = "mins"), 1), " minutes.")
msg("Tables  -> ", DIR_TABLES)
msg("Figures -> ", DIR_FIGURES)
msg("Next: cd latex && latexmk -pdf paper.tex")
msg("======================================================================")

# NOTE Matching diagnostics are no longer computed here. They come out of
# build/code/12_build_main_data.R, which sees the whole panel population rather
# than the employed subset, and are committed under analysis/input/matching.
# The superseded standalone script is in analysis/code/legacy/.
