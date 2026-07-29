# =============================================================================
# 10_download_pnadc_quarters.R
#
# Downloads PNAD Contínua quarterly microdata from IBGE, applies the
# datazoom.social quarterly cleaning, prunes to the columns this project needs
# and caches one parquet file per quarter in Dropbox.
#
#   build/input/pnadc_quarters/q_YYYY_Q.parquet
#
# WHY A PER-QUARTER CACHE
# datazoom.social::load_pnadc() downloads every quarter of a requested window,
# binds them, and only then splits by rotation panel V1014. Two problems follow.
# First, get_pnadc() always returns its full ~210-column structure regardless of
# `vars`, so a multi-year window does not fit in 16 GB. Second, the panel
# windows overlap: identifying all twelve rotation groups would re-download most
# quarters three times. Caching each quarter once, already pruned, solves both.
# 11_build_panels.R then reads from this cache.
#
# Panel identification itself is untouched: build_pnadc_panel() only ever looks
# within the rows it is given, and 11_build_panels.R feeds it exactly the rows
# of one V1014 group over that group's own window.
#
# Idempotent: quarters already cached are skipped, so the script can be
# interrupted and restarted.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  # Both packages must be attached, not merely available: get_pnadc() resolves
  # some of its own helpers through the search path and fails under `::`.
  library(PNADcIBGE)
  library(datazoom.social)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

DIR_QUARTERS <- file.path(DROPBOX, "build", "input", "pnadc_quarters")
dir.create(DIR_QUARTERS, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Columns to keep
# -----------------------------------------------------------------------------
# get_pnadc()'s `vars` argument only ever ADDS columns, it cannot restrict the
# download, so pruning happens here, immediately after each quarter is cleaned.

# Required by build_pnadc_panel()'s stage-3 identification algorithm.
COLS_PANEL_ID <- c("UPA", "V1008", "V1014", "V2007",
                   "V2008", "V20081", "V20082", "V2003", "V2009")

# Survey design: weights, strata, sampling unit, interview number.
COLS_DESIGN <- c("V1016", "V1027", "V1028", "V1029", "posest",
                 "Estrato", "UF", "V1022", "V1023")

# Demographics and education.
COLS_DEMO <- c("V2005", "V2010", "V3003A", "V3009A", "VD3004", "VD3005")

# Labour force status and job characteristics. VD4001/VD4002 give the three
# states this paper needs at t+1 (employed, unemployed, out of the labour
# force); VD4009 and the V40xx block give formality and job attributes.
COLS_LABOUR <- c(
  "VD4001", "VD4002", "VD4003", "VD4004A", "VD4005",
  "VD4007", "VD4008", "VD4009", "VD4010", "VD4012",
  "VD4017", "VD4019", "VD4020", "VD4031", "VD4035",
  "V4001", "V4009", "V4010", "V4012", "V4013", "V4014",
  "V4019", "V4025", "V4028", "V4029", "V4032", "V4034",
  "V4039", "V4040", "V4071", "V4076"
)

# Variables treat_pnadc() derives and that we keep rather than recompute.
COLS_DERIVED <- c("regiao", "sigla_uf", "faixa_idade", "faixa_educ",
                  "rendimento_habitual_real", "ocupado", "forca_trab",
                  "formal", "informal", "cnae_2dig", "cod_2dig", "Habitual")

COLS_KEEP <- c(COLS_PANEL_ID, COLS_DESIGN, COLS_DEMO, COLS_LABOUR,
               COLS_DERIVED, "Ano", "Trimestre")

# -----------------------------------------------------------------------------
# Window
# -----------------------------------------------------------------------------
# PNAD Contínua starts in 2012Q1. We download through the last published
# quarter; anything not yet released simply fails and is skipped. The sample
# runs to 2024Q4, but later quarters are still needed: rotation groups 11 and 12
# are identified over windows that extend past 2024, and truncating those
# windows would degrade their matching.
YEARS <- 2012:2026

# Retry policy for the IBGE server (see download_quarter()).
MAX_TRIES   <- 5L
RETRY_PAUSE <- 20L   # seconds, multiplied by the attempt number

download_quarter <- function(yr, q) {
  out <- file.path(DIR_QUARTERS, sprintf("q_%d_%d.parquet", yr, q))
  if (file.exists(out)) {
    message(sprintf("  %dQ%d  cached, skipping", yr, q))
    return(invisible(TRUE))
  }

  message(sprintf("  %dQ%d  downloading ...", yr, q))
  t0 <- Sys.time()

  # The IBGE server drops connections intermittently, and PNADcIBGE reports
  # every failure as "The internet connection is unavailable" regardless of
  # cause. A single attempt per quarter is therefore not enough: a burst of
  # transient failures once ended a run 29 quarters short even though the same
  # quarters downloaded fine seconds later. Retry with a growing pause.
  df <- NULL
  fatal <- FALSE
  for (attempt in 1:MAX_TRIES) {
    df <- tryCatch(
      get_pnadc(year = yr, quarter = q, vars = NULL,
                labels = FALSE, deflator = TRUE, design = FALSE),
      error = function(e) {
        m <- conditionMessage(e)
        # A quarter that IBGE has not published yet is not a transient failure;
        # retrying it just burns the backoff. Only connection errors are worth
        # repeating.
        if (grepl("unavailable for selected", m, fixed = TRUE)) {
          message("    not published yet"); fatal <<- TRUE
        } else {
          message("    attempt ", attempt, " failed: ", m)
        }
        NULL
      }
    )
    if (!is.null(df) || fatal) break
    unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE, force = TRUE)
    if (attempt < MAX_TRIES) {
      pause <- RETRY_PAUSE * attempt
      message("    retrying in ", pause, "s ...")
      Sys.sleep(pause)
    }
  }
  if (is.null(df)) {
    if (!fatal)
      message("    giving up on ", yr, "Q", q, " after ", MAX_TRIES, " attempts")
    return(invisible(FALSE))
  }

  # treat_pnadc()'s case_match() logic expects numeric PNADC codes, so coerce
  # before calling it. This mirrors load_pnadc()'s own pre-processing order.
  df <- as.data.frame(lapply(df, as.numeric))
  df <- datazoom.social:::treat_pnadc(df)
  df$Ano       <- yr
  df$Trimestre <- q

  keep <- intersect(names(df), COLS_KEEP)
  missing <- setdiff(COLS_KEEP, keep)
  if (length(missing))
    message("    not present this quarter: ", paste(missing, collapse = ", "))

  dt <- as.data.table(df)[, ..keep]
  rm(df)

  write_parquet(dt, out, compression = "zstd")
  message(sprintf("    %s rows, %d cols, %.0f MB, %.1f min",
                  format(nrow(dt), big.mark = ","), ncol(dt),
                  file.size(out) / 1024^2,
                  as.numeric(Sys.time() - t0, units = "mins")))
  rm(dt); invisible(gc())

  # get_pnadc() unzips each quarter into the session tempdir and never cleans
  # up; across 50+ quarters that silently fills the disk.
  unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

message("Caching PNADC quarters into ", DIR_QUARTERS)
for (yr in YEARS) for (q in 1:4) download_quarter(yr, q)

done <- list.files(DIR_QUARTERS, pattern = "^q_.*\\.parquet$")
message(sprintf("\nDone. %d quarters cached (%.1f GB).",
                length(done),
                sum(file.size(file.path(DIR_QUARTERS, done))) / 1024^3))

# A quarter that is genuinely unpublished is expected to be missing; one that is
# missing inside the published window is a failed download and the run should be
# repeated (it will skip everything already cached).
have <- sub("^q_(\\d{4})_(\\d)\\.parquet$", "\\1Q\\2", done)
want <- as.vector(t(outer(2012:2024, 1:4, function(y, q) sprintf("%dQ%d", y, q))))
gaps <- setdiff(want, have)
if (length(gaps))
  message("MISSING inside 2012-2024: ", paste(gaps, collapse = ", "))
