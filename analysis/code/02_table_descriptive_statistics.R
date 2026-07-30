# =============================================================================
# 02_table_descriptive_statistics.R
#
# Table A.1  Descriptive statistics, overall and by education group.
# Table A.2  Sample composition by quarter (person-quarters, college share,
#            raw employment-exit rate).
#
# All statistics are survey weighted.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(arrow)
})

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))
if (!exists("fmt_n")) source(file.path("analysis", "code", "_functions.R"))

d <- as.data.table(read_parquet(ANALYSIS_PQ))

# -----------------------------------------------------------------------------
# Variables reported, in the order they appear in the table
# -----------------------------------------------------------------------------
d[, `:=`(
  informal              = 1L - formal,
  formal_private        = as.integer(position_grp == "Formal private employee"),
  informal_private      = as.integer(position_grp == "Informal private employee"),
  formal_selfemployed   = as.integer(position_grp == "Formal self-employed"),
  informal_selfemployed = as.integer(position_grp == "Informal self-employed"),
  formal_public         = as.integer(position_grp == "Formal public sector"),
  employer              = as.integer(position_grp %in% c("Formal employer",
                                                         "Informal employer"))
)]

VARS <- c(
  "exit"                  = "Employment exit in $t+1$",
  "exit_to_informal"      = "Informally employed in $t+1$",
  "college"               = "College degree",
  "female"                = "Woman",
  "nonwhite"              = "Non-White",
  "urban"                 = "Urban area",
  "age"                   = "Age",
  "formal"                = "Formal employment",
  "informal"              = "Informal employment",
  "formal_private"        = "Formal private employee",
  "informal_private"      = "Informal private employee",
  "formal_public"         = "Formal public sector",
  "formal_selfemployed"   = "Formal self-employed",
  "informal_selfemployed" = "Informal self-employed",
  "employer"              = "Employer",
  "signed_card"           = "Signed work card",
  "social_security"       = "Social security contributor",
  "temporary"             = "Temporary job",
  "hours"                 = "Usual weekly hours",
  "income"                = "Monthly labour income (R\\$)"
)

wsum <- function(x, w, digits = 2) {
  mu <- stats::weighted.mean(x, w)
  sd <- sqrt(stats::weighted.mean((x - mu)^2, w))
  c(mean = mu, sd = sd)
}

stat_block <- function(dd) {
  vapply(names(VARS), function(v) wsum(dd[[v]], dd$w), numeric(2))
}

all_s <- stat_block(d)
nc_s  <- stat_block(d[college == 0L])
c_s   <- stat_block(d[college == 1L])

fmtv <- function(x, v) {
  digits <- if (v %in% c("age", "hours")) 1 else if (v == "income") 0 else 3
  formatC(x, format = "f", digits = digits, big.mark = if (v == "income") "," else "")
}

body <- vapply(names(VARS), function(v) {
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
          VARS[[v]],
          fmtv(all_s["mean", v], v), fmtv(all_s["sd", v], v),
          fmtv(nc_s ["mean", v], v), fmtv(nc_s ["sd", v], v),
          fmtv(c_s  ["mean", v], v), fmtv(c_s  ["sd", v], v))
}, character(1))

# A blank rule separates the labour-market position block. The position rows are
# flush left like every other row: indenting them made the label column ragged
# without adding information the row labels do not already carry.
body <- append(body, "\\addlinespace",
               after = which(names(VARS) == "informal"))

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Descriptive statistics}",
  "\\label{tab:descriptives}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{All workers} & \\multicolumn{2}{c}{No college degree} & \\multicolumn{2}{c}{College degree} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  "& Mean & S.D. & Mean & S.D. & Mean & S.D. \\\\",
  "\\midrule",
  body,
  "\\addlinespace",
  sprintf("Person-quarters & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          fmt_n(nrow(d)), fmt_n(nrow(d[college == 0L])), fmt_n(nrow(d[college == 1L]))),
  sprintf("Individuals & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          fmt_n(uniqueN(d$pid)), fmt_n(uniqueN(d[college == 0L]$pid)),
          fmt_n(uniqueN(d[college == 1L]$pid))),
  sprintf("Primary sampling units & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
          fmt_n(uniqueN(d$psu)), fmt_n(uniqueN(d[college == 0L]$psu)),
          fmt_n(uniqueN(d[college == 1L]$psu))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: \\textit{PNAD Cont\\'inua} matched quarterly panel, ",
         qlab(min(d$qtr)), "--", qlab(max(d$qtr)), ". The unit of observation is a ",
         "person-quarter for a worker employed at the interview date in quarter $t$ ",
         "who is matched to quarter $t+1$. Employment exit equals one when the worker ",
         "is unemployed or out of the labour force in $t+1$. College degree indicates ",
         "completed tertiary education. All moments are survey ",
         "weighted using \\textit{PNAD Cont\\'inua} person weights."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

write_tex(tex, file.path(DIR_TABLES, "tab_descriptives.tex"))

# -----------------------------------------------------------------------------
# Sample composition by quarter (appendix)
# -----------------------------------------------------------------------------
byq <- d[, .(n       = .N,
             p_col   = stats::weighted.mean(college, w),
             exit    = stats::weighted.mean(exit, w),
             psus    = uniqueN(psu)), by = qtr][order(qtr)]
fwrite(byq, file.path(DIR_TABLES, "sample_by_quarter.csv"))

cell <- sprintf("%s & %s & %s & %s & %s",
                qlab(byq$qtr), fmt_n(byq$n), fmt_n(byq$psus),
                formatC(byq$p_col, format = "f", digits = 3),
                formatC(byq$exit,  format = "f", digits = 3))

# One row per quarter runs off the bottom of the page -- the folio ended up
# printed over the last data rows -- so the quarters are set in two blocks side
# by side, the second continuing the first.
half <- length(cell) %/% 2L
rows <- sprintf("%s & %s \\\\", cell[seq_len(half)], cell[half + seq_len(half)])
if (length(cell) > 2L * half)                       # odd number of quarters
  rows <- c(rows, sprintf("%s & & & & & \\\\", cell[length(cell)]))

qcol <- "Quarter $t$ & Person-quarters & PSUs & College & Exit rate"

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Sample composition by quarter}",
  "\\label{tab:sample_by_quarter}",
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrcc@{\\hspace{1.5em}}lrrcc}",
  "\\toprule",
  paste0(qcol, " & ", qcol, " \\\\"),
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Survey-weighted college share and employment-exit rate ",
         "among workers employed in quarter $t$ and matched to quarter $t+1$. ",
         "The two blocks continue one another: the left block runs from ",
         qlab(byq$qtr[1]), " to ", qlab(byq$qtr[half]), " and the right from ",
         qlab(byq$qtr[half + 1L]), " to ", qlab(byq$qtr[length(cell)]), "."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_sample_by_quarter.tex"))

# -----------------------------------------------------------------------------
# Matching algorithm performance (appendix)
#
# Computed by build/code/12_build_main_data.R over the full panel population and
# committed under analysis/input/matching, so this table builds without the
# micro-data.
# -----------------------------------------------------------------------------
DIR_MATCH <- file.path(ROOT, "analysis", "input", "matching")
perf <- fread(file.path(DIR_MATCH, "stage3_matching.csv"))
mbi  <- fread(file.path(DIR_MATCH, "stage3_match_by_interview.csv"))

f1 <- function(x) formatC(100 * x, format = "f", digits = 1)

perf_row <- function(u) {
  x <- perf[unit == u][order(n_int)]
  vals <- vapply(1:5, function(k) {
    v <- x[n_int == k, at_least]
    if (length(v)) f1(v) else "--"
  }, character(1))
  sprintf("%s & %s \\\\", u, paste(vals, collapse = " & "))
}

mbi_row <- sprintf("Employed workers & %s \\\\",
                   paste(c(vapply(1:4, function(k) {
                     v <- mbi[V1016 == k, match_rate]
                     if (length(v)) f1(v) else "--"
                   }, character(1)),
                   f1(stats::weighted.mean(mbi[V1016 < 5, match_rate],
                                           mbi[V1016 < 5, n]))),
                   collapse = " & "))

write_tex(c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Performance of the stage-3 panel identification}",
  "\\label{tab:matching}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccccc}",
  "\\multicolumn{6}{l}{\\textit{Panel A. Share linked across at least $k$ interviews (\\%)}} \\\\",
  "\\toprule",
  "& $k=1$ & $k=2$ & $k=3$ & $k=4$ & $k=5$ \\\\",
  "\\midrule",
  perf_row("Individuals"),
  perf_row("Households"),
  "\\bottomrule",
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel B. Share matched into the next quarter, by interview (\\%)}} \\\\",
  "\\toprule",
  "& From 1st & From 2nd & From 3rd & From 4th & Overall \\\\",
  "\\midrule",
  mbi_row,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Notes: Computed on all quarterly \\textit{PNAD Cont\\'inua} ",
         "rotation panels used in the paper. Panel A covers the whole panel ",
         "population and reports the share of linked units observed in at least ",
         "$k$ of the five scheduled interviews. Panel B is restricted to workers ",
         "employed at the interview date and reports the share the algorithm ",
         "links into the following quarter, by the interview they are observed ",
         "in; the fifth interview is the last scheduled one and is excluded. ",
         "Stage 3 links respondents on household, sex and date of birth, donates ",
         "birth dates across interviews, and resolves fragmented sequences with a ",
         "graph-theoretic fuzzy match."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
), file.path(DIR_TABLES, "tab_matching.tex"))

msg("02_table_descriptive_statistics.R done.")
