# Load required libraries
library(data.table)
library(haven)
library(stringr)
library(here)
library(pbapply)

# Clear environment
rm(list = ls())

#===============================================================================
# CONSOLIDATED ANALYSIS - MATCHING RATES BY QUARTER
# Calculating retention rates (% that stay for at least X quarters)
#===============================================================================

# Set working directory and load data files
file_path = here("build", "output")

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

output_dir <- here("analysis", "output", "descriptive_statistics")

# Export retention table
fwrite(
  retention_table, 
  file.path(output_dir, "_table_matching_retention_analysis.csv")
)