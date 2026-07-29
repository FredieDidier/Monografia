# =============================================================================
# 11_build_panels.R
#
# Runs the datazoom.social stage-3 panel identification ("advanced_3") on each
# PNAD Contínua rotation group and caches the result:
#
#   build/input/pnadc_panels/Panel_{V1014}.parquet
#
# Stage 3 links the same respondent across interviews using donated birth dates,
# household order and a graph-theory fuzzy match for fragmented interviews. It
# supersedes the household+birth-date matching used by the earlier build and, in
# particular, recovers respondents whose recorded birth date drifts between
# interviews.
#
# Two design choices matter.
#
# 1. One rotation group at a time. build_pnadc_panel() only ever looks within
#    the rows it is given, and a household belongs to exactly one V1014 group,
#    so identifying group by group is equivalent to identifying the whole file
#    at once while keeping peak memory bounded.
#
# 2. Identification runs on the identifier columns alone. The algorithm reads
#    only UPA, V1008, V1014, the birth-date fields, sex, household order, age
#    and the time variables; the remaining ~60 survey columns are merged back
#    afterwards on a row key. This roughly halves peak memory for the heaviest
#    step without touching the matching itself.
#
# Idempotent: groups already built are skipped.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(datazoom.social)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

DIR_QUARTERS <- file.path(DROPBOX, "build", "input", "pnadc_quarters")
DIR_PANELS   <- file.path(DROPBOX, "build", "input", "pnadc_panels")
dir.create(DIR_PANELS, showWarnings = FALSE, recursive = TRUE)

# Columns the stage-3 algorithm reads. Beyond the obvious identifiers, its
# birth-date donation step needs V2005 (position in the household, to decide who
# may donate a date to whom) and V1016 (interview number, so a respondent cannot
# donate to themselves in the same interview).
ID_COLS <- c("UPA", "V1008", "V1014", "V1016", "V2003", "V2005", "V2007",
             "V2008", "V20081", "V20082", "V2009",
             "Ano", "Trimestre")

qfiles <- sort(list.files(DIR_QUARTERS, pattern = "^q_\\d{4}_\\d\\.parquet$",
                          full.names = TRUE))
if (!length(qfiles)) stop("No cached quarters in ", DIR_QUARTERS,
                          " -- run 10_download_pnadc_quarters.R first.")
message(length(qfiles), " cached quarters available.")

# -----------------------------------------------------------------------------
# Which quarters does each rotation group span?
# -----------------------------------------------------------------------------
# Read only (V1014, Ano, Trimestre) so the scan is cheap, and let the data
# define the windows rather than hard-coding the published rotation calendar.
# Groups that are already in the field when the survey starts, or still running
# when it ends, are simply truncated.
f_map <- file.path(DIR_PANELS, "_group_windows.csv")
if (file.exists(f_map)) {
  windows <- fread(f_map)
} else {
  message("Scanning cached quarters for rotation-group windows ...")
  windows <- rbindlist(lapply(qfiles, function(f) {
    x <- as.data.table(read_parquet(f, col_select = c("V1014", "Ano", "Trimestre")))
    unique(x[!is.na(V1014), .(V1014, Ano, Trimestre, file = f)])
  }))
  fwrite(windows, f_map)
}

groups <- sort(unique(windows$V1014))
message("Rotation groups found: ", paste(groups, collapse = ", "))

# -----------------------------------------------------------------------------
# Build one group
# -----------------------------------------------------------------------------
build_group <- function(p) {
  out <- file.path(DIR_PANELS, sprintf("Panel_%02d.parquet", p))
  if (file.exists(out)) {
    message(sprintf("  group %2d  cached, skipping", p))
    return(invisible(TRUE))
  }

  files_p <- sort(unique(windows[V1014 == p, file]))
  qs <- windows[V1014 == p][order(Ano, Trimestre)]
  message(sprintf("  group %2d  %dQ%d-%dQ%d, %d quarters ...",
                  p, qs$Ano[1], qs$Trimestre[1],
                  qs$Ano[nrow(qs)], qs$Trimestre[nrow(qs)], nrow(qs)))
  t0 <- Sys.time()

  dat <- rbindlist(lapply(files_p, function(f) {
    x <- as.data.table(read_parquet(f))
    x[!is.na(V1014) & V1014 == p]
  }), fill = TRUE)
  message(sprintf("    %s rows loaded", format(nrow(dat), big.mark = ",")))

  # Identification on the identifier columns only; everything else is merged
  # back on the row key afterwards.
  dat[, .row_key := .I]
  id_in <- dat[, c(ID_COLS, ".row_key"), with = FALSE]

  ids <- as.data.table(build_pnadc_panel(dat = as.data.frame(id_in),
                                         panel = "advanced_3"))
  rm(id_in); invisible(gc())

  new_cols <- setdiff(names(ids), c(ID_COLS, ".row_key"))
  message("    identification added: ", paste(new_cols, collapse = ", "))

  dat <- merge(dat, ids[, c(".row_key", new_cols), with = FALSE],
               by = ".row_key", all.x = TRUE)
  stopifnot(nrow(dat) == nrow(ids))
  dat[, .row_key := NULL]
  rm(ids); invisible(gc())

  matched <- dat[, mean(!is.na(id_rs3))]
  message(sprintf("    stage-3 match rate: %.1f%% of person-quarters",
                  100 * matched))

  write_parquet(dat, out, compression = "zstd")
  message(sprintf("    saved %s rows, %.0f MB, %.1f min",
                  format(nrow(dat), big.mark = ","),
                  file.size(out) / 1024^2,
                  as.numeric(Sys.time() - t0, units = "mins")))
  rm(dat); invisible(gc())
  invisible(TRUE)
}

for (p in groups) build_group(p)

built <- list.files(DIR_PANELS, pattern = "^Panel_.*\\.parquet$")
message(sprintf("\nDone. %d rotation groups built (%.1f GB).",
                length(built),
                sum(file.size(file.path(DIR_PANELS, built))) / 1024^3))
