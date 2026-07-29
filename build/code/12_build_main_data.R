# =============================================================================
# 12_build_main_data.R
#
# Turns the stage-3 rotation panels into the person-quarter transition file the
# analysis reads:
#
#   build/output/main_data.parquet
#
# One row per worker employed at the interview date in quarter t. Unlike the
# earlier build, this file keeps three things the referee report asked for and
# the previous vintage had discarded:
#
#   1. The destination state at t+1 in full: employed (formal or informal),
#      unemployed, or out of the labour force. The previous build collapsed the
#      last two into a single "non-employed" category, which made it impossible
#      to separate E->U from E->N.
#   2. Origins that the matching algorithm never finds again in t+1. They carry
#      matched_next = 0 and a missing outcome, which is what makes a genuine
#      t -> t+1 retention model possible instead of the one-step-removed proxy
#      the previous vintage forced.
#   3. UPA, Estrato and the household identifier as columns, so the survey
#      design is available directly rather than decoded from an ID string.
#
# Transitions never cross rotation groups -- a household belongs to exactly one
# V1014 -- so each panel is processed independently and only the (much smaller)
# origin rows are accumulated. That keeps peak memory well inside 16 GB.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

DIR_PANELS <- file.path(DROPBOX, "build", "input", "pnadc_panels")
dir.create(DIR_DATA, showWarnings = FALSE, recursive = TRUE)

panel_files <- sort(list.files(DIR_PANELS, pattern = "^Panel_.*\\.parquet$",
                               full.names = TRUE))
if (!length(panel_files))
  stop("No panels in ", DIR_PANELS, " -- run 11_build_panels.R first.")

# Last quarter of the estimation sample. Later quarters are downloaded and used
# for panel identification, but a worker is only an origin if their t+1 outcome
# falls inside the published window.
Q_LAST <- 20244L

# -----------------------------------------------------------------------------
# Labour market position
#
# Follows the classification of the previous build so the two vintages are
# comparable, with one correction. Employers were classified formal/informal by
# V4019 (does the business have a CNPJ?), but IBGE only began collecting V4019
# partway through 2015: it is missing for every employer from 2012Q1 to 2015Q1.
# Under the old rule those workers fell through every branch and were dropped
# from the sample without a trace -- 1.2% of employed person-quarters, all of
# them employers, all of them in the first three years. V4032 (contributes to
# social security) is available throughout and is the natural fallback, so
# employers are classified by CNPJ where it exists and by social security
# contribution where it does not.
#
#   VD4009 work category, V4012 job function, V4019 CNPJ, V4032 social security
# -----------------------------------------------------------------------------
add_position <- function(d) {
  # Employer formality: CNPJ when recorded, social security contribution
  # otherwise (2012Q1-2015Q1).
  d[, emp_formal := fcase(!is.na(V4019) & V4019 == 1L, TRUE,
                          !is.na(V4019) & V4019 == 2L, FALSE,
                          !is.na(V4032) & V4032 == 1L, TRUE,
                          !is.na(V4032) & V4032 == 2L, FALSE)]

  d[, position := fcase(
    VD4002 == 1L & VD4009 %in% c(1L, 3L) & V4012 %in% c(1L, 3L),          3L, # formal private employee
    VD4002 == 1L & VD4009 %in% c(2L, 4L, 10L) & V4012 %in% c(1L, 3L, 7L), 4L, # informal private employee
    VD4002 == 1L & V4012 == 6L & V4032 == 1L,                             5L, # formal self-employed
    VD4002 == 1L & V4012 == 6L & V4032 == 2L,                             6L, # informal self-employed
    VD4002 == 1L & V4012 == 5L & emp_formal,                              7L, # formal employer
    VD4002 == 1L & V4012 == 5L & !emp_formal,                             8L, # informal employer
    VD4002 == 1L & V4012 %in% c(2L, 4L) & VD4009 %in% c(5L, 7L),          9L, # formal public sector
    VD4002 == 1L & V4012 %in% c(2L, 4L) & VD4009 == 6L,                  10L, # informal public sector
    VD4002 == 1L,                                                        99L  # employed, position undetermined
  )]
  d[, emp_formal := NULL]
  d[, formal := fcase(position %in% c(3L, 5L, 7L, 9L), 1L,
                      position %in% c(4L, 6L, 8L, 10L), 0L)]
  d[]
}

# Labour-force state, from the two derived PNADC variables.
#   VD4001 = 1 in the labour force, 2 out of it
#   VD4002 = 1 employed, 2 unemployed
add_state <- function(d) {
  d[, lf_state := fcase(
    VD4001 == 1L & VD4002 == 1L, "Employed",
    VD4001 == 1L & VD4002 == 2L, "Unemployed",
    VD4001 == 2L,                "Out of labour force"
  )]
  d[]
}

KEEP_IN <- c("id_rs3", "id_dom", "UPA", "Estrato", "V1008", "V1014", "V1016",
             "V1022", "V1023", "V1028", "UF", "Ano", "Trimestre",
             "V2003", "V2005", "V2007", "V2009", "V2010",
             "VD3004", "VD3005",
             "VD4001", "VD4002", "VD4009", "VD4010", "VD4012", "VD4017", "VD4031",
             "V4010", "V4012", "V4013", "V4019", "V4025", "V4029", "V4032",
             "V4039", "V4040")

# Matching performance is measured over the whole panel population, not just
# the employed, so it is accumulated inside the per-panel loop before the
# employment restriction is applied.
spell_list <- list()
ambiguous  <- list()

process_panel <- function(f) {
  msg("  ", basename(f))
  d <- as.data.table(read_parquet(f))
  keep <- intersect(KEEP_IN, names(d))
  d <- d[, ..keep]

  d[, qtr  := Ano * 10L + Trimestre]
  d[, qidx := 4L * (Ano - 2012L) + Trimestre]
  add_state(d)
  add_position(d)

  # --- ambiguous identifications ---------------------------------------------
  # A stage-3 id must appear at most once per quarter. In a small number of
  # cases the graph pass merges two different respondents into one cluster,
  # which shows up as the same id twice in the same quarter (of the order of
  # 0.01% of rows). Any transition built from such an id is unreliable, so the
  # id is voided: those rows stay in the origin population -- they are real
  # employed people -- but count as unlinkable, exactly like any other worker
  # the algorithm fails to follow.
  bad <- d[!is.na(id_rs3), .N, by = .(id_rs3, qtr)][N > 1L, unique(id_rs3)]
  if (length(bad)) {
    n_bad <- d[id_rs3 %chin% bad, .N]
    ambiguous[[basename(f)]] <<- data.table(panel = basename(f),
                                            ids = length(bad), rows = n_bad,
                                            share = n_bad / nrow(d))
    d[id_rs3 %chin% bad, id_rs3 := NA_character_]
  }

  # --- destination at t+1 -----------------------------------------------------
  # Only matched individuals can have a destination. Unmatched rows keep
  # id_rs3 = NA and must not be joined to each other, so they are excluded from
  # the lookup but retained as origins below.
  nxt <- d[!is.na(id_rs3), .(id_rs3, qidx, lf_state, formal)]
  setnames(nxt, c("lf_state", "formal"), c("dest_state", "dest_formal"))
  nxt[, qidx := qidx - 1L]

  # --- matching performance over the full panel population --------------------
  ind <- d[!is.na(id_rs3), .(n_int = uniqueN(qtr)), by = id_rs3]
  hh  <- d[, .(n_int = uniqueN(qtr)), by = .(V1014, id_dom)]
  spell_list[[basename(f)]] <<- list(
    ind = ind[, .N, by = n_int],
    hh  = hh[, .N, by = n_int],
    n_person_quarters = nrow(d),
    n_unmatched = d[, sum(is.na(id_rs3))]
  )
  rm(ind, hh)

  # --- origins: employed at t, inside the published window --------------------
  org <- d[lf_state == "Employed" & V2009 >= 14L & qtr <= Q_LAST]
  rm(d); invisible(gc())

  org <- merge(org, nxt, by = c("id_rs3", "qidx"), all.x = TRUE)
  org[, matched_next := as.integer(!is.na(dest_state))]
  # An unmatched origin is unmatched by construction, not "still employed".
  org[is.na(id_rs3), matched_next := 0L]

  org[, `:=`(
    exit                 = fifelse(matched_next == 1L,
                                   as.integer(dest_state != "Employed"), NA_integer_),
    exit_to_unemployment = fifelse(matched_next == 1L,
                                   as.integer(dest_state == "Unemployed"), NA_integer_),
    exit_to_nonpart      = fifelse(matched_next == 1L,
                                   as.integer(dest_state == "Out of labour force"), NA_integer_),
    to_informal          = fifelse(matched_next == 1L,
                                   as.integer(dest_state == "Employed" & dest_formal == 0L),
                                   NA_integer_)
  )]
  org
}

msg("Building transitions from ", length(panel_files), " rotation groups ...")
out <- rbindlist(lapply(panel_files, process_panel), fill = TRUE)

# id_rs3 is a hexadecimal string whose leading digit encodes the rotation group,
# so it is globally unique across panels; verified below rather than assumed.
stopifnot(!anyDuplicated(out[!is.na(id_rs3), .(id_rs3, qtr)]))

amb <- rbindlist(ambiguous)
if (nrow(amb))
  msg("voided ", format(sum(amb$ids), big.mark = ","), " ambiguous ids (",
      format(sum(amb$rows), big.mark = ","), " person-quarters, ",
      formatC(100 * sum(amb$rows) / (sum(amb$rows) + nrow(out)),
              format = "f", digits = 3), "% of rows)")

msg("Origins: ", format(nrow(out), big.mark = ","),
    " | matched to t+1: ",
    formatC(out[, mean(matched_next)], format = "f", digits = 4))

# -----------------------------------------------------------------------------
# Diagnostics that matter for the paper
# -----------------------------------------------------------------------------
log_con <- file(file.path(DIR_LOGS, "12_build_main_data.txt"), open = "wt")
tee <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = log_con) }

tee("Rotation groups : ", length(panel_files))
und <- out[position == 99L, .N]
tee("Position undetermined: ", format(und, big.mark = ","),
    " (", formatC(100 * und / nrow(out), format = "f", digits = 3), "% of origins)")
if (und / nrow(out) > 0.005)
  warning("more than 0.5% of employed origins have an undetermined position")
if (nrow(amb)) {
  tee("Ambiguous ids voided: ", format(sum(amb$ids), big.mark = ","),
      " (", format(sum(amb$rows), big.mark = ","), " person-quarters)")
}
tee("Origin rows     : ", format(nrow(out), big.mark = ","))
tee("Quarters        : ", qlab(min(out$qtr)), " to ", qlab(max(out$qtr)))
tee("Matched to t+1  : ", formatC(out[, mean(matched_next)], format = "f", digits = 4))
tee("")
tee("Destination at t+1 among matched origins (weighted):")
dd <- out[matched_next == 1L, .(share = sum(V1028) ), by = dest_state]
dd[, share := share / sum(share)]
for (i in seq_len(nrow(dd)))
  tee("  ", dd$dest_state[i], ": ", formatC(dd$share[i], format = "f", digits = 4))
tee("")
tee("Weighted exit rate         : ",
    formatC(out[matched_next == 1L, weighted.mean(exit, V1028)], format = "f", digits = 4))
tee("  of which to unemployment : ",
    formatC(out[matched_next == 1L, weighted.mean(exit_to_unemployment, V1028)],
            format = "f", digits = 4))
tee("  of which to non-part.    : ",
    formatC(out[matched_next == 1L, weighted.mean(exit_to_nonpart, V1028)],
            format = "f", digits = 4))
tee("")
tee("Match rate by education (weighted):")
mr <- out[, .(match = weighted.mean(matched_next, V1028), n = .N),
          by = .(college = as.integer(VD3004 == 7L))][order(college)]
for (i in seq_len(nrow(mr)))
  tee("  college = ", mr$college[i], ": ", formatC(mr$match[i], format = "f", digits = 4),
      "  (n = ", format(mr$n[i], big.mark = ","), ")")

# -----------------------------------------------------------------------------
# Matching performance table (committed, so the paper builds without the data)
# -----------------------------------------------------------------------------
collapse_spells <- function(which) {
  x <- rbindlist(lapply(spell_list, `[[`, which))[, .(N = sum(N)), by = n_int]
  x <- x[n_int >= 1L][order(n_int)]
  x[, share := N / sum(N)]
  # Share observed in AT LEAST k interviews.
  x[, at_least := rev(cumsum(rev(share)))]
  x[]
}
perf <- rbind(
  cbind(unit = "Individuals", collapse_spells("ind")),
  cbind(unit = "Households",  collapse_spells("hh"))
)
perf[, algorithm := "stage3"]
DIR_MATCH <- file.path(ROOT, "analysis", "input", "matching")
dir.create(DIR_MATCH, showWarnings = FALSE, recursive = TRUE)
fwrite(perf, file.path(DIR_MATCH, "stage3_matching.csv"))

match_by_interview <- out[, .(n = .N,
                              match_rate = weighted.mean(matched_next, V1028)),
                          by = V1016][order(V1016)]
fwrite(match_by_interview, file.path(DIR_MATCH, "stage3_match_by_interview.csv"))
tee("")
tee("Match rate into t+1 by interview number:")
for (i in seq_len(nrow(match_by_interview)))
  tee("  interview ", match_by_interview$V1016[i], ": ",
      formatC(match_by_interview$match_rate[i], format = "f", digits = 4))
close(log_con)

# Drop data.table's secondary indices before writing.
#
# Subsetting like out[matched_next == 1L, ...] -- which the diagnostics above do
# -- makes data.table build an auto-index and store it as an attribute: an
# integer vector with one element per row. arrow serialises R attributes into
# the Parquet key-value metadata, so on 11.5M rows those indices pushed the
# table's attributes to 88 MB, the footer past Thrift's 100 MB limit, and the
# resulting 403 MB file could not be reopened -- not even its schema. Clearing
# the indices takes the file to 228 MB and makes it readable. Verified by
# reproducing both states on this exact table.
setindex(out, NULL)

write_parquet(out, RAW_PARQUET, compression = "zstd")

# Read the artifact back before declaring success: the failure above surfaced
# only when the analysis opened the file ten minutes later, where the error was
# far from its cause.
chk <- tryCatch(
  as.data.table(read_parquet(RAW_PARQUET, col_select = c("qtr", "matched_next"))),
  error = function(e) e
)
if (inherits(chk, "error"))
  stop("main_data.parquet was written but cannot be read back: ",
       conditionMessage(chk))
stopifnot(nrow(chk) == nrow(out))
msg("wrote and verified ", RAW_PARQUET, " (",
    round(file.size(RAW_PARQUET) / 1024^2, 1), " MB, ",
    format(nrow(chk), big.mark = ","), " rows)")

msg("12_build_main_data.R done.")
