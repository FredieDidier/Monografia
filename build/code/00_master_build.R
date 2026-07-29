# =============================================================================
# 00_master_build.R
#
# Builds the analysis micro-data from scratch, straight from IBGE.
#
# Usage, from the repository root:
#     Rscript "build/code/00_master_build.R"
#
#   10  download every PNAD Continua quarter and cache it as parquet
#   11  run the datazoom.social stage-3 panel identification per rotation group
#   12  turn the panels into the person-quarter transition file
#
# Every step is idempotent and caches to Dropbox, so the script can be
# interrupted and restarted; already-completed quarters and groups are skipped.
# Expect several hours on a first run, dominated by the ~12 GB of downloads and
# by the stage-3 fuzzy matching.
#
# The output, build/output/main_data.parquet, is what
# analysis/code/01_prepare_analysis_data.R reads.
#
# This supersedes the earlier Stata + R build (000_geracao_paineis.do and
# 01-05_*.R, kept in this directory for provenance), which used the older
# household + birth-date matching and discarded the destination state at t+1,
# the unmatched origins and the sampling unit.
# =============================================================================

rm(list = ls())
t_start <- Sys.time()

if (!file.exists(file.path("analysis", "code", "_config.R")))
  stop("Run from the repository root: Rscript \"build/code/00_master_build.R\"")

source(file.path("analysis", "code", "_config.R"))
setwd(ROOT)

for (step in c("10_download_pnadc_quarters.R",
               "11_build_panels.R",
               "12_build_main_data.R")) {
  msg("======================================================================")
  msg("RUN  ", step)
  msg("======================================================================")
  t0 <- Sys.time()
  source(file.path(ROOT, "build", "code", step), echo = FALSE)
  msg("OK   ", step, "  (",
      round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min)")
}

msg("Build finished in ",
    round(as.numeric(Sys.time() - t_start, units = "hours"), 2), " hours.")
msg("Next: Rscript \"analysis/code/00_master_analysis.R\"")
