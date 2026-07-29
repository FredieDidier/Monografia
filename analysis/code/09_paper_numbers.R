# =============================================================================
# 09_paper_numbers.R
#
# Emits every number quoted in the body of the paper as a LaTeX macro, written
# to analysis/output/tables/numbers.tex and \input by latex/paper.tex.
#
# Nothing numeric is typed by hand into the manuscript: if an estimate changes,
# the text changes with it on the next compile.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow)
})

if (!exists("ROOT"))  source(file.path("analysis", "code", "_config.R"))
if (!exists("fmt_n")) source(file.path("analysis", "code", "_functions.R"))

d  <- as.data.table(read_parquet(ANALYSIS_PQ))
am <- fread(file.path(DIR_EST, "adjusted_margins_quarterly.csv"))
pm <- fread(file.path(DIR_EST, "adjusted_margins_periods.csv"))
sg <- fread(file.path(DIR_EST, "period_margins_by_segment.csv"))
dc <- fread(file.path(DIR_EST, "decomposition_periods_main.csv"))
rt <- fread(file.path(DIR_EST, "retention_by_period.csv"))
ip <- fread(file.path(DIR_EST, "attrition_ipw_comparison.csv"))

macros <- list()
put <- function(name, value) macros[[name]] <<- value

num  <- function(x, k = 3) fmt_num(x, k)
pct  <- function(x, k = 1) fmt_num(100 * x, k)
pp   <- function(x, k = 1) fmt_num(100 * x, k)   # in p.p.
cnt  <- function(x) formatC(x, format = "d", big.mark = ",")

# ---- Sample -----------------------------------------------------------------
put("Nobs",       cnt(nrow(d)))
put("Nind",       cnt(uniqueN(d$pid)))
put("Npsu",       cnt(uniqueN(d$psu)))
put("Nhh",        cnt(uniqueN(d$household)))
put("Nquarters",  as.character(uniqueN(d$qtr)))
put("QfirstLab",  qlab(min(d$qtr)))
put("QlastLab",   qlab(max(d$qtr)))

# ---- Unconditional exit rates ----------------------------------------------
put("RawExit",        pct(d[, weighted.mean(exit, w)]))
put("RawExitNoCol",   pct(d[college == 0L, weighted.mean(exit, w)]))
put("RawExitCol",     pct(d[college == 1L, weighted.mean(exit, w)]))
put("ShareCollege",   pct(d[, weighted.mean(college, w)], 0))
put("ShareInformal",  pct(d[, weighted.mean(1 - formal, w)], 0))
put("RawExitFormal",  pct(d[formal == 1L, weighted.mean(exit, w)]))
put("RawExitInformal", pct(d[formal == 0L, weighted.mean(exit, w)]))
put("RawExitEU",      pct(d[, weighted.mean(exit_to_unemployment, w)]))
put("RawExitEN",      pct(d[, weighted.mean(exit_to_nonpart, w)]))

# Every employed person-quarter, matched or not (the attrition denominator).
orig <- as.data.table(read_parquet(file.path(DIR_DATA, "analysis_origins.parquet"),
                                   col_select = c("interview", "matched_next", "w")))
put("NoriginsAll",   cnt(nrow(orig)))
# The match rate is computed on interviews 1-4, the origins for which a further
# interview is scheduled at all. Including the fifth interview would count a
# scheduled panel exit as a failure to match and understate the rate (69.9% on
# that definition), and it is the 1-4 rate that Table A.1 reports and that the
# breakdown calculation in Section 6 uses.
put("MatchRateAll",  pct(orig[interview < 5L, weighted.mean(matched_next, w)]))

# ---- Adjusted gaps by period ------------------------------------------------
PER <- c(Pre = "^Pre", Onset = "^Onset", Mid = "^Mid", Post = "^Post")
gp <- function(pat, col) pm[grepl(pat, period)][[col]]
for (nm in names(PER)) {
  k <- PER[[nm]]
  put(paste0(nm, "Gap"),     num(gp(k, "gap")))
  put(paste0(nm, "GapSE"),   num(gp(k, "gap_se")))
  put(paste0(nm, "GapPP"),   pp(gp(k, "gap")))
  put(paste0(nm, "GapLo"),   num(gp(k, "gap_lo")))
  put(paste0(nm, "GapHi"),   num(gp(k, "gap_hi")))
  put(paste0(nm, "LevNoCol"), num(gp(k, "m_nocol")))
  put(paste0(nm, "LevCol"),   num(gp(k, "m_col")))
}
put("PreGapAbsPP", pp(abs(pm[grepl("^Pre", period), gap])))
put("MidGapAbsPP", pp(abs(pm[grepl("^Mid", period), gap])))

# Relative change in adjusted levels vs the pre-pandemic benchmark
pre_n <- pm[grepl("^Pre", period), m_nocol]; pre_c <- pm[grepl("^Pre", period), m_col]
put("OnsetRelNoCol", pct(pm[grepl("^Onset", period), m_nocol] / pre_n - 1, 0))
put("OnsetRelCol",   pct(pm[grepl("^Onset", period), m_col]   / pre_c - 1, 0))
put("MidRelNoCol",   pct(pm[grepl("^Mid", period), m_nocol]   / pre_n - 1, 0))
put("MidRelCol",     pct(pm[grepl("^Mid", period), m_col]     / pre_c - 1, 0))
# The mid-pandemic changes are falls, so the text reads "falls by X%" and needs
# the magnitude: "falls by -43%" would say the opposite of what is meant.
put("MidRelNoColAbs", pct(abs(pm[grepl("^Mid", period), m_nocol] / pre_n - 1), 0))
put("MidRelColAbs",   pct(abs(pm[grepl("^Mid", period), m_col]   / pre_c - 1), 0))

# ---- Event-study detail -----------------------------------------------------
put("SuptCrit",  num(am$crit_sim[1], 2))
rev_q <- am[gap > 0, quarter]
put("NRevQuarters", as.character(length(rev_q)))
put("RevFirst", if (length(rev_q)) rev_q[1] else "--")
put("RevLast",  if (length(rev_q)) rev_q[length(rev_q)] else "--")
sim_pos <- am[gap_lo_sim > 0, quarter]
put("NRevSimQuarters", as.character(length(sim_pos)))
put("GapOnsetQ",  num(am[quarter == "2020Q1", gap]))
put("GapMaxRev",  num(max(am$gap)))
put("GapMaxRevQ", am[which.max(gap), quarter])

# ---- Heterogeneity ----------------------------------------------------------
sgp <- function(seg, per, col) sg[segment == seg & grepl(per, period)][[col]]
for (sname in c("Formal", "Informal")) {
  put(paste0(sname, "MidGap"),   num(sgp(sname, "^Mid", "gap")))
  put(paste0(sname, "PreGap"),   num(sgp(sname, "^Pre", "gap")))
  put(paste0(sname, "MidGapSE"), num(sgp(sname, "^Mid", "gap_se")))
}
for (sname in c("Men", "Women")) {
  put(paste0(sname, "MidGap"), num(sgp(sname, "^Mid", "gap")))
  put(paste0(sname, "PreGap"), num(sgp(sname, "^Pre", "gap")))
}
put("WhiteMidGap",      num(sgp("White", "^Mid", "gap")))
put("WhitePreGap",      num(sgp("White", "^Pre", "gap")))
put("NonWhiteMidGap", num(sgp("Non-White", "^Mid", "gap")))
put("NonWhitePreGap", num(sgp("Non-White", "^Pre", "gap")))
put("InfSelfMidGap",    num(sgp("Informal self-employed", "^Mid", "gap")))
put("InfPrivMidGap",    num(sgp("Informal private employee", "^Mid", "gap")))
put("InfPrivPreGap",    num(sgp("Informal private employee", "^Pre", "gap")))
put("FormPrivMidGap",   num(sgp("Formal private employee", "^Mid", "gap")))
put("FormSelfMidGap",   num(sgp("Formal self-employed", "^Mid", "gap")))
put("FormPubMidGap",    num(sgp("Formal public sector", "^Mid", "gap")))

# ---- Decomposition ----------------------------------------------------------
dcp <- function(per, col) dc[grepl(per, period)][[col]]
for (nm in names(PER)) {
  k <- PER[[nm]]
  put(paste0("Dec", nm, "Total"),  num(dcp(k, "gap_raw")))
  put(paste0("Dec", nm, "Comp"),   num(dcp(k, "composition")))
  put(paste0("Dec", nm, "Within"), num(dcp(k, "within")))
  put(paste0("Dec", nm, "CompShare"),
      formatC(100 * dcp(k, "composition") / dcp(k, "gap_raw"),
              format = "f", digits = 0))
  put(paste0("Dec", nm, "WithinShare"),
      formatC(100 * dcp(k, "within") / dcp(k, "gap_raw"),
              format = "f", digits = 0))
}
put("DecWithinShrink",
    formatC(100 * (1 - dcp("^Mid", "within") / dcp("^Pre", "within")),
            format = "f", digits = 0))
put("DecCompShrink",
    formatC(100 * (1 - dcp("^Mid", "composition") / dcp("^Pre", "composition")),
            format = "f", digits = 0))
put("DecRawShrink",
    formatC(100 * (1 - dcp("^Mid", "gap_raw") / dcp("^Pre", "gap_raw")),
            format = "f", digits = 0))

# ---- Attrition --------------------------------------------------------------
put("RetOverall",  num(rt[grepl("^Pre", period), retention_nocol]))
put("RetDiffPre",  num(rt[grepl("^Pre", period), diff_educ]))
put("RetDiffMid",  num(rt[grepl("^Mid", period), diff_educ]))
put("RetDiffOnset", num(rt[grepl("^Onset", period), diff_educ]))
put("IpwMidGap",   num(ip[grepl("^Mid", period), gap_ipw]))
put("IpwMidGapSE", num(ip[grepl("^Mid", period), se_ipw]))
put("BaseMidGap",  num(ip[grepl("^Mid", period), gap_base]))
# Both round to the same three decimals, so the sentence comparing them in
# Section 6 quotes four.
put("IpwMidGapFour",  num(ip[grepl("^Mid", period), gap_ipw],  4))
put("BaseMidGapFour", num(ip[grepl("^Mid", period), gap_base], 4))

bd <- readLines(file.path(DIR_LOGS, "07_attrition_breakdown.txt"))
put("BreakdownMid",
    sub(".*: ", "", grep("needed to zero out", bd, value = TRUE)[1]))

# ---- Alternative outcome: informal employment in t+1 ------------------------
rb <- fread(file.path(DIR_EST, "robustness_period_margins.csv"))
inf_row <- function(per) rb[grepl("informal employment", spec) & grepl(per, period), gap]
put("PreGapInf", num(inf_row("^Pre")))
put("MidGapInf", num(inf_row("^Mid")))

# ---- The two destination margins, mid-pandemic -------------------------------
for (y in c("exit_to_unemployment", "exit_to_nonpart")) {
  f <- file.path(DIR_EST, sprintf("adjusted_margins_periods_%s.csv", y))
  if (!file.exists(f)) next
  z <- fread(f)
  tag <- if (y == "exit_to_unemployment") "EU" else "EN"
  put(paste0("MidGap", tag),   num(z[grepl("^Mid", period), gap]))
  put(paste0("MidGap", tag, "SE"), num(z[grepl("^Mid", period), gap_se]))
  put(paste0("PreGap", tag),   num(z[grepl("^Pre", period), gap]))
}

# ---- Wild cluster bootstrap p-values (preferred specification) --------------
wcbp <- readRDS(file.path(DIR_EST, "wcb_pvalues.rds"))
fmt_p <- function(x, B = B_WILD) {
  if (is.na(x)) return("--")
  if (x < 1 / (B + 1)) sprintf("$<$%.4f", 1 / (B + 1))
  else formatC(x, format = "f", digits = 3)
}
put("WcbOnset", fmt_p(wcbp[["onset"]]))
put("WcbMid",   fmt_p(wcbp[["mid"]]))

# ---- Inference settings -----------------------------------------------------
put("BWild",   cnt(B_WILD))
put("BSupt",   cnt(B_SUPT))
put("BDecomp", cnt(B_DECOMP))
put("Seed",    as.character(SEED))

# ---- Write ------------------------------------------------------------------
lines <- c(
  "% Auto-generated by analysis/code/09_paper_numbers.R. Do not edit by hand.",
  sprintf("\\newcommand{\\%s}{%s}", names(macros), unlist(macros))
)
write_tex(lines, file.path(DIR_TABLES, "numbers.tex"))
msg("09_paper_numbers.R done (", length(macros), " macros).")
