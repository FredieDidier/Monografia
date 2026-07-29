# =============================================================================
# 99_session_info.R
#
# Writes the exact software environment used to produce the results, for the
# replication package (Labour Economics requires code, package versions and
# seeds to be documented).
# =============================================================================

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

PKGS <- c("data.table", "arrow", "readstata13", "fixest", "marginaleffects",
          "fwildclusterboot", "ggplot2", "scales", "haven", "sandwich")

info <- utils::sessionInfo()
loaded <- vapply(PKGS, function(p)
  tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed"),
  character(1))

lines <- c(
  "Software environment",
  "====================",
  "",
  paste0("R version : ", info$R.version$version.string),
  paste0("Platform  : ", info$platform),
  paste0("Running   : ", info$running),
  paste0("BLAS      : ", info$BLAS %||% "unknown"),
  paste0("fixest threads: ", tryCatch(fixest::getFixest_nthreads(),
                                      error = function(e) NA)),
  "",
  "Package versions",
  "----------------",
  sprintf("  %-18s %s", names(loaded), loaded),
  "",
  "Random number generation",
  "------------------------",
  paste0("  RNG kind         : ", paste(RNGkind(), collapse = ", ")),
  paste0("  Global seed      : ", SEED),
  paste0("  Wild bootstrap   : ", B_WILD, " replications"),
  paste0("  sup-t multiplier : ", B_SUPT, " draws"),
  paste0("  Decomposition    : ", B_DECOMP, " replications"),
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
)

`%||%` <- function(a, b) if (is.null(a)) b else a
writeLines(lines, file.path(DIR_LOGS, "session_info.txt"))
cat(paste(lines, collapse = "\n"), "\n")

# A machine-readable copy for the replication archive
writeLines(capture.output(utils::sessionInfo()),
           file.path(DIR_LOGS, "session_info_full.txt"))

msg("99_session_info.R done.")
