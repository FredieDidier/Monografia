# Load required libraries
library(data.table)
library(haven)
library(stringr)
library(here)

# Clear environment
rm(list = ls())

#===============================================================================
# CONSOLIDATED ANALYSIS - GENERAL AVERAGE BY QUARTER
# Calculating overlap between quarters for the entire sample
#===============================================================================

# Set working directory and load data files
file_path = here("build", "output")

# Get all panel .dta files
panel_files <- list.files(pattern = "^painel_.*\\.dta$", full.names = TRUE)

# Read and combine all panel files using data.table
combined_data <- rbindlist(lapply(panel_files, function(file) {
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
# INDIVIDUALS - Consolidated Analysis
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

# Calculate percentages for individuals
individual_counts[, percentage := (count / total_individuals) * 100]

#===============================================================================
# HOUSEHOLDS - Consolidated Analysis  
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

# Calculate percentages for households
household_counts[, percentage := (count / total_households) * 100]

#===============================================================================
# CREATE FINAL CONSOLIDATED TABLES
#===============================================================================

# Create percentage table (main output requested)
percentage_table <- data.table(
  type = c("Individuals", "Households"),
  Q1 = c(
    individual_counts[n_quarters == 1, percentage],
    household_counts[n_quarters == 1, percentage]
  ),
  Q2 = c(
    individual_counts[n_quarters == 2, percentage],
    household_counts[n_quarters == 2, percentage]
  ),
  Q3 = c(
    individual_counts[n_quarters == 3, percentage],
    household_counts[n_quarters == 3, percentage]
  ),
  Q4 = c(
    individual_counts[n_quarters == 4, percentage],
    household_counts[n_quarters == 4, percentage]
  ),
  Q5 = c(
    individual_counts[n_quarters == 5, percentage],
    household_counts[n_quarters == 5, percentage]
  )
)

# Round to 2 decimal places
numeric_cols <- c("Q1", "Q2", "Q3", "Q4", "Q5")
percentage_table[, (numeric_cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = numeric_cols]

#===============================================================================
# DISPLAY AND EXPORT RESULTS
#===============================================================================

# Display results
cat("=== CONSOLIDATED PERCENTAGE ANALYSIS ===\n")
print(percentage_table)

output_dir <- here("analysis", "output", "descriptive_statistics")

# Export percentage table
fwrite(
  percentage_table, 
  file.path(output_dir, "_table_matching_consolidated_analysis.csv")
)

