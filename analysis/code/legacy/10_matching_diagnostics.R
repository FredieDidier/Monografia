# =============================================================================
# 10_matching_diagnostics.R
#
# Retention and quarter-to-quarter transition rates of the Data Zoom matching
# algorithm, computed from the *source* rotation panels rather than from the
# analysis file (which keeps matched pairs only).
#
# OPTIONAL STEP. It reads every build/output/painel_YYYY_Q.dta in Dropbox
# (~15 GB) and is therefore not part of the default pipeline in
# 00_master_analysis.R. Its outputs are committed under analysis/input/matching
# so that the tables and the paper build without it. Re-run it only when the
# panel construction changes.
# =============================================================================

library(data.table)
library(haven)
library(stringr)
library(pbapply)

if (!exists("ROOT")) source(file.path("analysis", "code", "_config.R"))

#===============================================================================
# CONSOLIDATED ANALYSIS - MATCHING RATES BY QUARTER
# Calculating retention rates (% that stay for at least X quarters)
#===============================================================================

# Source rotation panels live in Dropbox, not in the repository.
file_path = file.path(DROPBOX, "build", "output")

# Get all panel .dta files
panel_files <- list.files(path = file_path, pattern = "^painel_.*\\.dta$", full.names = TRUE)

# Set progress bar
pboptions(type = "timer", char = "=", txt.width = 50, style = 3)

# Read and combine all panel files using data.table
combined_data <- rbindlist(pblapply(panel_files, function(file) {
  dt <- as.data.table(read_dta(file))
  return(dt[, .(Ano, Trimestre, UPA, Estrato, V1008, V1014, V1016, V1028, painel, idind)])
}), fill = TRUE)

# Compress and sort data
setorder(combined_data, idind, Ano, Trimestre)

# Collapse to unique combinations
combined_data <- combined_data[, .SD[1], by = .(idind, Ano, Trimestre)]

# Create household identifier
combined_data[, id_dom := paste(UPA, V1008, V1014, sep = "_")]

#===============================================================================
# INDIVIDUALS - Retention Analysis
#===============================================================================

# Count how many quarters each individual appears in
individual_quarters <- combined_data[, .(n_quarters = .N), by = idind]

# Count individuals by number of quarters they appear
individual_counts <- individual_quarters[, .(count = .N), by = n_quarters]

# Ensure we have all quarters 1-5, filling with 0 if missing
quarters_template <- data.table(n_quarters = 1:5)
individual_counts <- individual_counts[quarters_template, on = "n_quarters"]
individual_counts[is.na(count), count := 0]

# Calculate total individuals
total_individuals <- individual_counts[, sum(count)]

# Calculate percentages for individuals (exactly X quarters)
individual_counts[, percentage := (count / total_individuals) * 100]

# Calculate RETENTION rates (at least X quarters)
# Q1: 100% (everyone appears at least 1 quarter)
# Q2: 100% - % that appear exactly 1 quarter
# Q3: 100% - % that appear exactly 1 or 2 quarters
# Q4: 100% - % that appear exactly 1, 2, or 3 quarters
# Q5: 100% - % that appear exactly 1, 2, 3, or 4 quarters

individual_retention <- data.table(
  n_quarters = 1:5,
  retention_rate = c(
    100.0,  # Everyone appears at least 1 quarter
    100.0 - individual_counts[n_quarters == 1, percentage],
    100.0 - individual_counts[n_quarters <= 2, sum(percentage)],
    100.0 - individual_counts[n_quarters <= 3, sum(percentage)],
    100.0 - individual_counts[n_quarters <= 4, sum(percentage)]
  )
)

#===============================================================================
# HOUSEHOLDS - Retention Analysis  
#===============================================================================

# Create unique household-quarter combinations
household_data <- combined_data[, .SD[1], by = .(id_dom, Ano, Trimestre)]

# Count how many quarters each household appears in
household_quarters <- household_data[, .(n_quarters = .N), by = id_dom]

# Count households by number of quarters they appear
household_counts <- household_quarters[, .(count = .N), by = n_quarters]

# Ensure we have all quarters 1-5, filling with 0 if missing
household_counts <- household_counts[quarters_template, on = "n_quarters"]
household_counts[is.na(count), count := 0]

# Calculate total households
total_households <- household_counts[, sum(count)]

# Calculate percentages for households (exactly X quarters)
household_counts[, percentage := (count / total_households) * 100]

# Calculate RETENTION rates (at least X quarters)
household_retention <- data.table(
  n_quarters = 1:5,
  retention_rate = c(
    100.0,  # Everyone appears at least 1 quarter
    100.0 - household_counts[n_quarters == 1, percentage],
    100.0 - household_counts[n_quarters <= 2, sum(percentage)],
    100.0 - household_counts[n_quarters <= 3, sum(percentage)],
    100.0 - household_counts[n_quarters <= 4, sum(percentage)]
  )
)

#===============================================================================
# CREATE FINAL RETENTION TABLE
#===============================================================================

# Create retention percentage table (main output requested)
retention_table <- data.table(
  type = c("Individuals", "Households"),
  Q1 = c(
    individual_retention[n_quarters == 1, retention_rate],
    household_retention[n_quarters == 1, retention_rate]
  ),
  Q2 = c(
    individual_retention[n_quarters == 2, retention_rate],
    household_retention[n_quarters == 2, retention_rate]
  ),
  Q3 = c(
    individual_retention[n_quarters == 3, retention_rate],
    household_retention[n_quarters == 3, retention_rate]
  ),
  Q4 = c(
    individual_retention[n_quarters == 4, retention_rate],
    household_retention[n_quarters == 4, retention_rate]
  ),
  Q5 = c(
    individual_retention[n_quarters == 5, retention_rate],
    household_retention[n_quarters == 5, retention_rate]
  )
)

# Round to 2 decimal places
numeric_cols <- c("Q1", "Q2", "Q3", "Q4", "Q5")
retention_table[, (numeric_cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = numeric_cols]

#===============================================================================
# DISPLAY AND EXPORT RESULTS
#===============================================================================

# Display results
cat("=== RETENTION RATES ANALYSIS (% that stay for AT LEAST X quarters) ===\n")
print(retention_table)

output_dir <- file.path(ROOT, "analysis", "input", "matching")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Export retention table
fwrite(
  retention_table, 
  file.path(output_dir, "matching_retention.csv")
)


#===============================================================================
# ADDITIONAL ANALYSIS - PROBABILITY OF QUARTER-TO-QUARTER TRANSITION
# For each quarter T, what percentage of those observed appear in T+1?
#===============================================================================

cat("\n=== TRANSITION PROBABILITY ANALYSIS (Quarter-to-Quarter) ===\n")
#-------------------------------------------------------------------------------
# INDIVIDUALS - Transition Probability
#-------------------------------------------------------------------------------

# Sort data to ensure proper ordering
setorder(combined_data, idind, Ano, Trimestre)

# Create a lagged version to identify consecutive quarters
combined_data[, `:=`(
  next_ano = shift(Ano, type = "lead"),
  next_trim = shift(Trimestre, type = "lead")
), by = idind]

# Identify if the individual appears in the next quarter
# Next quarter is either: same year, next trimestre OR next year, trimestre 1
combined_data[, appears_next := ifelse(
  (Ano == next_ano & Trimestre + 1 == next_trim) |
    (Ano + 1 == next_ano & Trimestre == 4 & next_trim == 1),
  1, 0
)]

# For the last observation, mark it as 0 (not shown) instead of NA
combined_data[is.na(next_ano), appears_next := 0]

# Create a variable for quarter position (1st appearance, 2nd, etc.)
combined_data[, quarter_position := seq_len(.N), by = idind]

# Identify the last position of each individual
combined_data[, max_position := max(quarter_position), by = idind]

# Exclude only those in position 5 (last possible quarter in the panel)
# But include those who left in previous positions (1, 2, 3, 4)
individual_transition <- combined_data[quarter_position <= 4, .(
  n_observed = .N,
  n_appear_next = sum(appears_next),
  transition_prob = (sum(appears_next) / .N) * 100
), by = quarter_position]

# Rename for clarity
setnames(individual_transition, "quarter_position", "from_quarter")

# Overall transition probability (all quarters combined)
overall_ind_transition <- combined_data[quarter_position <= 4, .(
  from_quarter = "Overall",
  n_observed = .N,
  n_appear_next = sum(appears_next),
  transition_prob = (sum(appears_next) / .N) * 100
)]

# Combine results
individual_transition <- rbind(
  individual_transition,
  overall_ind_transition,
  fill = TRUE
)

# Round percentages
individual_transition[, transition_prob := round(transition_prob, 2)]

cat("\n--- INDIVIDUALS: Transition Probability by Quarter Position ---\n")
cat("(Given I observed someone in quarter N, what % appear in quarter N+1?)\n\n")
print(individual_transition)

#-------------------------------------------------------------------------------
# HOUSEHOLDS - Transition Probability
#-------------------------------------------------------------------------------

# Create household dataset with proper ordering
household_data_trans <- combined_data[, .SD[1], by = .(id_dom, Ano, Trimestre)]
setorder(household_data_trans, id_dom, Ano, Trimestre)

# Create lagged version for households
household_data_trans[, `:=`(
  next_ano = shift(Ano, type = "lead"),
  next_trim = shift(Trimestre, type = "lead")
), by = id_dom]

# Identify if household appears in next quarter
household_data_trans[, appears_next := ifelse(
  (Ano == next_ano & Trimestre + 1 == next_trim) |
    (Ano + 1 == next_ano & Trimestre == 4 & next_trim == 1),
  1, 0
)]

# For the last observation, mark it as 0 (not shown) instead of NA
household_data_trans[is.na(next_ano), appears_next := 0]

# Create quarter position for households
household_data_trans[, quarter_position := seq_len(.N), by = id_dom]

# Identify the last position of each household
household_data_trans[, max_position := max(quarter_position), by = id_dom]

# Exclude only position 5, include whoever left before
household_transition <- household_data_trans[quarter_position <= 4, .(
  n_observed = .N,
  n_appear_next = sum(appears_next),
  transition_prob = (sum(appears_next) / .N) * 100
), by = quarter_position]

setnames(household_transition, "quarter_position", "from_quarter")

# Overall transition probability
overall_hh_transition <- household_data_trans[quarter_position <= 4, .(
  from_quarter = "Overall",
  n_observed = .N,
  n_appear_next = sum(appears_next),
  transition_prob = (sum(appears_next) / .N) * 100
)]

# Combine results
household_transition <- rbind(
  household_transition,
  overall_hh_transition,
  fill = TRUE
)

# Round percentages
household_transition[, transition_prob := round(transition_prob, 2)]

cat("\n--- HOUSEHOLDS: Transition Probability by Quarter Position ---\n")
cat("(Given I observed a household in quarter N, what % appear in quarter N+1?)\n\n")
print(household_transition)

#-------------------------------------------------------------------------------
# CREATE SUMMARY TABLE (Main result for your boss)
#-------------------------------------------------------------------------------

# Simple summary table showing overall transition probability
transition_summary <- data.table(
  Type = c("Individuals", "Households"),
  `Observed in Quarter T` = c(
    overall_ind_transition$n_observed,
    overall_hh_transition$n_observed
  ),
  `Appear in Quarter T+1` = c(
    overall_ind_transition$n_appear_next,
    overall_hh_transition$n_appear_next
  ),
  `Transition Probability (%)` = c(
    overall_ind_transition$transition_prob,
    overall_hh_transition$transition_prob
  )
)

cat("\n=== SUMMARY: Quarter-to-Quarter Transition Probability ===\n")
cat("(Of every 100 people/households observed in a quarter, how many appear in the next quarter?)\n\n")
print(transition_summary)

#-------------------------------------------------------------------------------
# EXPORT RESULTS
#-------------------------------------------------------------------------------

# Export individual transitions
fwrite(
  individual_transition,
  file.path(output_dir, "individual_transition.csv")
)

# Export household transitions
fwrite(
  household_transition,
  file.path(output_dir, "household_transition.csv")
)

# Export summary table
fwrite(
  transition_summary,
  file.path(output_dir, "transition_summary.csv")
)

cat("\n✓ Transition probability analysis completed!\n")
cat("✓ Files exported to:", output_dir, "\n")