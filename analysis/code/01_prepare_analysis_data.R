# =============================================================================
# 01_prepare_analysis_data.R
#
# Turns build/output/main_data.parquet (stage-3 rotation panels, one row per
# worker employed in quarter t) into the two analysis files:
#
#   analysis_sample.parquet   matched origins -- the estimation sample
#   analysis_origins.parquet  every origin, matched or not -- for the attrition
#                             analysis, which needs the unmatched ones
#
# Both live in Dropbox, never in git.
#
# Splitting the file here rather than filtering inside each analysis script
# keeps the estimation sample unambiguous: anything read from
# analysis_sample.parquet has an observed t+1 outcome.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

log_con <- file(file.path(DIR_LOGS, "01_prepare_diagnostics.txt"), open = "wt")
tee <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = log_con) }

stopifnot(file.exists(RAW_PARQUET))
msg("reading ", RAW_PARQUET)
d <- as.data.table(read_parquet(RAW_PARQUET))
tee("Source          : ", RAW_PARQUET)
tee("Origin rows     : ", format(nrow(d), big.mark = ","))

# -----------------------------------------------------------------------------
# Identifiers and survey design
# -----------------------------------------------------------------------------
# id_rs3 is the stage-3 individual key: a hexadecimal STRING whose leading digit
# encodes the rotation group, so it is globally unique and must not be coerced
# to numeric ("1ffff" would become NA). It is NA for origins the algorithm never
# links; those rows only ever enter the attrition analysis, where they are the
# object of interest, so each gets its own id rather than sharing one.
d[, pid := fifelse(!is.na(id_rs3), as.character(id_rs3), paste0("u", .I))]

# id_dom is unique only within a rotation group; prefix it to make it global.
d[, household := paste0(V1014, "_", id_dom)]
d[, psu       := as.integer(UPA)]
d[, strata    := as.integer(Estrato)]
d[, panel_grp := as.integer(V1014)]
d[, interview := as.integer(V1016)]      # survey's own interview counter, 1-5
d[, w         := as.numeric(V1028)]
d[, state     := as.integer(UF)]

# -----------------------------------------------------------------------------
# Outcomes
# -----------------------------------------------------------------------------
setnames(d, "to_informal", "exit_to_informal", skip_absent = TRUE)
d[, exit                 := as.integer(exit)]
d[, exit_to_unemployment := as.integer(exit_to_unemployment)]
d[, exit_to_nonpart      := as.integer(exit_to_nonpart)]
d[, exit_to_informal     := as.integer(exit_to_informal)]

# -----------------------------------------------------------------------------
# Education, demographics
# -----------------------------------------------------------------------------
d[, college     := as.integer(VD3004 == 7L)]      # completed tertiary
d[, female      := as.integer(V2007 == 2L)]
d[, race5       := factor(V2010, levels = c(1, 2, 3, 4, 5, 9),
                          labels = c("White", "Black", "Asian", "Brown",
                                     "Indigenous", "Not reported"))]
d[, white       := as.integer(V2010 == 1L)]
d[, nonwhite    := as.integer(!is.na(V2010) & V2010 != 1L)]
d[, black_brown := as.integer(V2010 %in% c(2L, 4L))]
d[, urban       := as.integer(V1022 == 1L)]
d[, age         := as.integer(V2009)]

# -----------------------------------------------------------------------------
# Job characteristics
# -----------------------------------------------------------------------------
d[, hours           := as.numeric(V4039)]
d[, income          := fifelse(is.na(VD4017), 0, as.numeric(VD4017))]
d[, log_income      := log1p(income)]
d[, temporary       := as.integer(!is.na(V4025) & V4025 == 1L)]
d[, social_security := as.integer(!is.na(V4032) & V4032 == 1L)]
d[, signed_card     := as.integer(!is.na(V4029) & V4029 == 1L)]

# V4040: time in the current job, grouped.
d[, tenure := factor(V4040, levels = 1:4,
                     labels = c("<1 month", "1-11 months", "1-2 years",
                                "2+ years"))]
d[is.na(tenure), tenure := "2+ years"]

# VD4010: main activity grouping, collapsed to the five broad sectors used by
# the previous vintage so the two are comparable.
d[, sector := factor(fcase(
  VD4010 == 1L, "Agriculture",
  VD4010 == 2L, "Industries",
  VD4010 == 3L, "Construction",
  VD4010 == 4L, "Trade",
  VD4010 %in% 5:12, "Services",
  default = "Not reported"),
  levels = c("Agriculture", "Industries", "Construction", "Trade", "Services",
             "Not reported"))]

# V4010 is the four-digit code of IBGE's Classificacao de Ocupacoes para
# Pesquisas Domiciliares (COD); its leading digit is the major group. The
# previous build enumerated every four-digit code by hand into these same ten
# groups.
OCC_LAB <- c("Armed forces, police and military firefighters",
             "Managers", "Professionals",
             "Technicians and associate professionals",
             "Clerical support workers", "Service and sales workers",
             "Skilled agricultural, forestry and fishery workers",
             "Craft and related trades workers",
             "Plant and machine operators and assemblers",
             "Elementary occupations")
d[, occ_major := V4010 %/% 1000L]
d[, occupation := factor(fifelse(!is.na(occ_major) & occ_major %in% 0:9,
                                 OCC_LAB[occ_major + 1L], "Not reported"),
                         levels = c(OCC_LAB, "Not reported"))]

# -----------------------------------------------------------------------------
# Labour market position
# -----------------------------------------------------------------------------
POS_LAB <- c("3"  = "Formal private employee",
             "4"  = "Informal private employee",
             "5"  = "Formal self-employed",
             "6"  = "Informal self-employed",
             "7"  = "Formal employer",
             "8"  = "Informal employer",
             "9"  = "Formal public sector",
             "10" = "Informal public sector",
             "99" = "Undetermined")
d[, position_grp := factor(POS_LAB[as.character(position)],
                           levels = unname(POS_LAB))]
d[, formal := as.integer(formal)]

# A residual group of employed workers matches none of the position rules and so
# has no formality. Coding them as informal would be a guess, and leaving them
# with a missing control would let fixest drop them silently -- making N differ
# across the columns of Table 2 for no stated reason. They are removed here
# instead, once, with the count on the record.
n_und <- d[position_grp == "Undetermined", .N]
if (n_und > 0.005 * nrow(d))
  stop("undetermined positions are ", round(100 * n_und / nrow(d), 2),
       "% of origins -- too many to drop; revisit add_position()")
d <- d[position_grp != "Undetermined"]

KEEP <- c("pid", "psu", "household", "strata", "panel_grp", "interview", "qtr",
          "matched_next", "dest_state",
          "exit", "exit_to_unemployment", "exit_to_nonpart", "exit_to_informal",
          "college", "female", "white", "nonwhite", "black_brown", "race5",
          "urban", "age", "hours", "income", "log_income",
          "formal", "temporary", "social_security", "signed_card", "tenure",
          "sector", "occupation", "position_grp", "state", "w")
d <- d[, ..KEEP]

# -----------------------------------------------------------------------------
# Diagnostics
# -----------------------------------------------------------------------------
tee("")
tee("Dropped, position undetermined: ", format(n_und, big.mark = ","),
    " (", formatC(100 * n_und / (n_und + nrow(d)), format = "f", digits = 3), "%)")
tee("Quarters        : ", qlab(min(d$qtr)), " to ", qlab(max(d$qtr)),
    "  (", uniqueN(d$qtr), " quarters)")
tee("Individuals     : ", format(uniqueN(d[matched_next == 1L]$pid), big.mark = ","))
tee("Households      : ", format(uniqueN(d$household), big.mark = ","))
tee("PSUs            : ", format(uniqueN(d$psu), big.mark = ","))
tee("Matched to t+1  : ", formatC(d[, mean(matched_next)], format = "f", digits = 4))
tee("")
tee("Interview number distribution:")
iv <- d[, .N, by = interview][order(interview)]
for (i in seq_len(nrow(iv))) tee("  ", iv$interview[i], ": ",
                                 format(iv$N[i], big.mark = ","))

est <- d[matched_next == 1L]
tee("")
tee("ESTIMATION SAMPLE: ", format(nrow(est), big.mark = ","), " matched origins")
tee("Weighted exit rate            : ",
    formatC(est[, weighted.mean(exit, w)], format = "f", digits = 4))
tee("  no college                  : ",
    formatC(est[college == 0L, weighted.mean(exit, w)], format = "f", digits = 4))
tee("  college                     : ",
    formatC(est[college == 1L, weighted.mean(exit, w)], format = "f", digits = 4))
tee("Weighted E->U rate            : ",
    formatC(est[, weighted.mean(exit_to_unemployment, w)], format = "f", digits = 4))
tee("Weighted E->N rate            : ",
    formatC(est[, weighted.mean(exit_to_nonpart, w)], format = "f", digits = 4))
tee("Weighted college share        : ",
    formatC(est[, weighted.mean(college, w)], format = "f", digits = 4))
tee("Weighted informal share       : ",
    formatC(est[, weighted.mean(1 - formal, w)], format = "f", digits = 4))

tee("")
tee("Missing values in the estimation sample:")
miss <- est[, lapply(.SD, function(x) sum(is.na(x)))]
any_miss <- FALSE
for (nm in names(miss)) if (miss[[nm]] > 0) {
  any_miss <- TRUE; tee("  ", nm, ": ", format(miss[[nm]], big.mark = ","))
}
if (!any_miss) tee("  none")

# -----------------------------------------------------------------------------
# Write
# -----------------------------------------------------------------------------
# See build/code/12_build_main_data.R: data.table auto-indices are stored as
# per-row attributes and arrow serialises them into the Parquet footer, which
# silently produces an unreadable file at this scale.
setindex(est, NULL); setindex(d, NULL)

write_parquet(est, ANALYSIS_PQ, compression = "zstd")
tee("")
tee("wrote ", ANALYSIS_PQ, " (", round(file.size(ANALYSIS_PQ) / 1024^2, 1), " MB)")

ORIGINS_PQ <- file.path(DIR_DATA, "analysis_origins.parquet")
write_parquet(d, ORIGINS_PQ, compression = "zstd")
tee("wrote ", ORIGINS_PQ, " (", round(file.size(ORIGINS_PQ) / 1024^2, 1), " MB)")

for (f in c(ANALYSIS_PQ, ORIGINS_PQ)) {
  ok <- tryCatch({ read_parquet(f, col_select = "qtr"); TRUE },
                 error = function(e) { tee("READ-BACK FAILED for ", f, ": ",
                                           conditionMessage(e)); FALSE })
  if (!ok) stop("wrote an unreadable parquet: ", f)
}
tee("both files read back successfully")

close(log_con)
msg("01_prepare_analysis_data.R done.")
