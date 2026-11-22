# ============================================================
# IMPC Group 13 — CSV Structural Cleaning and Validation
# ============================================================

# Purpose:
# Import and validate IMPC phenotyping CSV files, identify and
# repair structural inconsistencies, and combine all records
# into a single data frame for subsequent SOP-based cleaning
#
# Additional checks:
# Investigate consistency between filenames and analysis IDs,
# and identify identical and conflicting analysis records
# that may affect database integrity
#
# Data integrity:
# All structural repairs are performed in memory
# Original CSV files remain unchanged
# Duplicate and conflicting records are flagged, not removed
#
# Output:
# data_all — combined data frame containing 26,819 analysis
# records, 20 original fields and one source filename column


# 1. Data import and structural validation
# Import the SOP containing the data validation rules
sop <- read.csv("Data/IMPC_SOP_2026.csv")

# Identify all CSV files in the correct dataset
files <- list.files(
  path = "Data/group13_data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Confirm the number of CSV files
length(files)

## QC result: 26,819 CSV files identified

## Import warnings: 5 warnings reported, including malformed CSV rows
## and improper quotation marks. Investigate before combining the data

# Identify files with an unexpected number of rows or columns
structure_errors <- sapply(raw_list, function(data) {
  nrow(data) != 20 || ncol(data) != 2
})

# Record the paths of structurally problematic CSV files
structure_problem_files <- files[structure_errors]

# Display the affected filenames
structure_problem_files

# Count the affected files
length(structure_problem_files)

## QC result: 3 of 26,819 CSV files have unexpected dimensions
# following fread() import; investigate possible parsing errors

# 2. Investigation of structural errors

# Inspect the dimensions of each flagged file
lapply(raw_list[structure_errors], dim)

# Display the original text of each flagged file
for (file in structure_problem_files) {
  cat("\nFile:", file, "\n")
  print(readLines(file, warn = FALSE))
}
### QC result: 3 CSV files have structural import errors
# an1044293973.csv: lacz_colon has no associated value or separator
# an1071069523.csv: lacz_skeletal_muscle is missing a comma separator
# an5479464504.csv: gene_accession_id field name is missing
# Structural repairs are required before combining the CSV files

# 3. Field-name validation

# Identify files with missing, unexpected or duplicated field names
field_errors <- sapply(raw_list, function(data) {
  !setequal(data$V1, expected_fields) ||
    anyDuplicated(data$V1) > 0
})

# Record the filenames requiring field-name investigation
field_problem_files <- files[field_errors]

# Display the filenames and total number affected
field_problem_files
length(field_problem_files)

# QC result: 6 files have unexpected, missing or duplicated field names
# Three also have incorrect imported dimensions
# Inspect the remaining files before applying structural corrections

# Identify files with field-name errors but expected dimensions
additional_problem_files <- setdiff(
  field_problem_files,
  structure_problem_files
)

# Display the original contents of these files
for (file in additional_problem_files) {
  cat("\nFile:", file, "\n")
  print(readLines(file, warn = FALSE))
}
# QC result: 6 CSV files contain structural or field-name inconsistencies
# Three have malformed rows that affected the number of rows imported
# Two contain incorrectly formatted field names
# One contains two missing field names
# All six files require structural handling before data-frame construction

#### 4. Structural data cleaning

### Repair malformed CSV rows and field names in memory
### Preserve the original CSV files and biological values
### Reimport affected files to recover rows lost during fread()

for (file in field_problem_files) {
  
  # Read the original file as text
  lines <- readLines(file, warn = FALSE)
  
  # Confirm that the original file contains 20 lines
  stopifnot(length(lines) == 20)
  
  # Apply file-specific structural corrections
  if (basename(file) == "an1044293973.csv") {
    lines[11] <- '"lacz_colon",NA'
  }
  
  if (basename(file) == "an1071069523.csv") {
    lines[17] <- '"lacz_skeletal_muscle","no ^expression"'
  }
  
  if (basename(file) == "an5479464504.csv") {
    lines[1] <- '"gene_accession_id","MGI:1270129"'
  }
  
  if (basename(file) == "an1002211771.csv") {
    lines[16] <- '"lacz_lung","no expression"'
  }
  
  if (basename(file) == "an9995686988.csv") {
    lines[4] <- '"strain_name","C57BL/6N"'
  }
  
  if (basename(file) == "an9996797625.csv") {
    lines[11] <- '"lacz_colon","No data"'
    lines[13] <- '"lacz_heart","expression"'
  }
  
  # Reimport the corrected CSV text without changing the source file
  repaired_data <- read.csv(
    text = paste(lines, collapse = "\n"),
    header = FALSE,
    colClasses = "character",
    na.strings = c("", "NA")
  )
  
  # Replace the incomplete import with the repaired version
  raw_list[[match(file, files)]] <- repaired_data
}

# 5. Post-repair structural validation

# Confirm that every imported file contains 20 rows and 2 columns
remaining_structure_errors <- sum(sapply(raw_list, function(d) {
  nrow(d) != 20 || ncol(d) != 2
}))

# Confirm that each file contains all expected fields exactly once
remaining_field_errors <- sum(sapply(raw_list, function(d) {
  !setequal(d$V1, expected_fields) ||
    anyDuplicated(d$V1) > 0
}))

# Display the remaining error counts
remaining_structure_errors
remaining_field_errors

### Post-cleaning QC: All 26,819 files now have the expected
## 20 rows and 2 columns, with all 20 required field names
## present exactly once
## Structural validation passed: 0 remaining errors

# 6. Import integrity checks

# Check that each original CSV line contains exactly two fields
# when interpreted using standard CSV separators and quotation marks
parsing_errors <- sapply(files, function(file) {
  field_counts <- count.fields(file, sep = ",", quote = "\"")
  
  any(is.na(field_counts) | field_counts != 2)
})

# Identify original CSV files with potential parsing problems
parsing_problem_files <- files[parsing_errors]

# Display affected filenames and their total count
parsing_problem_files
length(parsing_problem_files)

### QC result: 4 original CSV files contain parsing inconsistencies
# All 4 were previously identified and structurally repaired in memory
# No additional files were flagged by the parsing check
# Original CSV files remain unchanged

# 7. Imported-value integrity validation

# Define the expected values following structural repairs
# These values were verified against the original CSV text
expected_repairs <- list(
  "an1044293973.csv" = c(lacz_colon = NA_character_),
  "an1071069523.csv" = c(lacz_skeletal_muscle = "no ^expression"),
  "an5479464504.csv" = c(gene_accession_id = "MGI:1270129"),
  "an1002211771.csv" = c(lacz_lung = "no expression"),
  "an9995686988.csv" = c(strain_name = "C57BL/6N"),
  "an9996797625.csv" = c(
    lacz_colon = "No data",
    lacz_heart = "expression"
  )
)

# Verify that the repaired fields contain their expected values
repair_checks <- sapply(names(expected_repairs), function(filename) {
  
  # Locate the corresponding imported CSV
  i <- match(filename, basename(files))
  data <- raw_list[[i]]
  
  # Retrieve field values by their names
  values <- setNames(as.character(data$V2), data$V1)
  expected <- expected_repairs[[filename]]
  
  # Compare actual and expected values, including missing values
  identical(unname(values[names(expected)]), unname(expected))
})

# Display the validation result for each repaired CSV
repair_checks

# Count repairs that failed validation
sum(!repair_checks)

### Validation result: All 6 structurally repaired CSV files passed
# the targeted imported-value integrity checks
# No discrepancies were identified in the repaired field-value pairs
# Original CSV files remain unchanged

# 8. Comprehensive import-integrity validation

# Identify files that did not require structural repairs
unrepaired_files <- setdiff(files, field_problem_files)

# Create a vector to record files with import discrepancies
import_problem_files <- character(0)

# Independently import each original CSV and compare its contents
# with the corresponding fread() import
for (file in unrepaired_files) {
  
  # Read the original CSV using base R
  reference_data <- tryCatch(
    read.csv(file, header = FALSE,
             colClasses = "character",
             na.strings = c("", "NA")),
    error = function(e) NULL
  )
  
  # Retrieve the corresponding data from the original import
  imported_data <- raw_list[[match(file, files)]]
  
  # Compare field names, values and missing-value positions
  if (is.null(reference_data) ||
      !identical(as.character(reference_data$V1),
                 as.character(imported_data$V1)) ||
      !identical(as.character(reference_data$V2),
                 as.character(imported_data$V2))) {
    
    import_problem_files <- c(import_problem_files, file)
  }
}

# Display files with discrepancies between the two import methods
import_problem_files

# Count files requiring further import investigation
length(import_problem_files)

# 9. Investigation of import discrepancies

# QC result: 2 CSV files were imported differently by
# fread() and read.csv(); investigate before combining records.

# Compare the original text and imported values for each file
for (file in import_problem_files) {
  
  cat("\nFile:", basename(file), "\n")
  
  # Display the original CSV contents
  cat("\nOriginal CSV:\n")
  print(readLines(file, warn = FALSE))
  
  # Import the file independently using base R
  reference_data <- read.csv(
    file,
    header = FALSE,
    colClasses = "character",
    na.strings = c("", "NA")
  )
  
  # Retrieve the existing fread() import
  imported_data <- raw_list[[match(file, files)]]
  
  # Display both imports for comparison
  cat("\nBase R import:\n")
  print(reference_data)
  
  cat("\nfread() import:\n")
  print(imported_data)
}

# 10. Resolve import-method discrepancies

# QC result: 2 CSV files were parsed differently by fread()
# and read.csv() due to malformed quotation marks.
# Use the base R interpretation while retaining anomalous
# values for subsequent SOP-based validation.

# Identify the files requiring consistent reimport
quote_problem_files <- import_problem_files

# Replace the affected imports in memory
for (file in quote_problem_files) {
  
  # Reimport the original CSV using base R
  corrected_import <- read.csv(
    file,
    header = FALSE,
    colClasses = "character",
    na.strings = c("", "NA")
  )
  
  # Update the corresponding entry in the imported list
  raw_list[[match(file, files)]] <- corrected_import
}

# Verify the two imported values
raw_list[[match("Data/group13_data/an5363428182.csv", files)]][6, 2]
raw_list[[match("Data/group13_data/an5433186127.csv", files)]][16, 2]

# 11. Final structural validation before data-frame construction

# Confirm that all imported files contain 20 rows and 2 columns
final_structure_errors <- sum(sapply(raw_list, function(d) {
  nrow(d) != 20 || ncol(d) != 2
}))

# Confirm that all expected fields occur exactly once in each file
final_field_errors <- sum(sapply(raw_list, function(d) {
  !setequal(d$V1, expected_fields) ||
    anyDuplicated(d$V1) > 0
}))

# Display the validation results
final_structure_errors
final_field_errors

### Import validation: 2 CSV files contained malformed quotation marks,
# resulting in different interpretations by fread() and read.csv()
# Both files were reimported using read.csv() for consistency
# The malformed p_value entry ("1".1) was parsed as 1.1;
# however, the intended statistical value remains uncertain
# The lacz_lung value was parsed as "no expression asdasdasd"
# Both values are retained for subsequent SOP-based investigation
# No assumptions were made about the correct biological values
# Original CSV files remain unchanged

# 11. Final structural validation

# Verify that all imported files contain 20 rows and 2 columns
final_structure_errors <- sum(sapply(raw_list, function(d) {
  nrow(d) != 20 || ncol(d) != 2
}))

# Verify that all 20 required field names occur exactly once
final_field_errors <- sum(sapply(raw_list, function(d) {
  !setequal(d$V1, expected_fields) ||
    anyDuplicated(d$V1) > 0
}))

# Display the remaining structural error counts
final_structure_errors
final_field_errors

### Final structural validation: All 26,819 imported CSV files
# contain 20 rows and 2 columns, with all required fields
# present exactly once
# No structural errors remain after in-memory repairs
# Original CSV files remain unchanged

# 12. Construct the combined analysis data frame

# Convert each field-value table into a named vector,
# ensuring values are aligned by their expected field names
data_list <- lapply(raw_list, function(data) {
  setNames(as.character(data$V2), data$V1)[expected_fields]
})

# Combine all analysis records into one data frame
data_all <- as.data.frame(do.call(rbind, data_list))

# Retain the original filename for traceability
data_all$file <- basename(files)

# Verify the dimensions of the combined data frame
dim(data_all)

## Import validation: The combined data frame contains 26,819 rows
# and 21 columns (20 analysis fields and the source filename)
# Each original CSV is represented by one row

### 13. Create a working copy for SOP-based cleaning


# Preserve the combined imported data while cleaning a separate copy
data_clean <- data_all


# Validation:

# 14. Validate analysis identifiers against source filenames

# Extract the expected analysis ID from each CSV filename
expected_analysis_ids <- tools::file_path_sans_ext(data_clean$file)

# Identify records where the internal analysis ID differs from the filename
filename_id_errors <- data_clean[
  is.na(data_clean$analysis_id) |
    data_clean$analysis_id != expected_analysis_ids,
  c("file", "analysis_id")
]

# Display affected records and count discrepancies
filename_id_errors
nrow(filename_id_errors)

# 15. Investigate filename and analysis ID discrepancies

# QC result: 60 filenames differ from their internal analysis IDs.
# All observed discrepancies involve an additional "_1" suffix.
# Investigate whether these files correspond to duplicate analyses.

# Identify filenames with a "_1.csv" suffix
suffix_files <- data_clean$file[
  grepl("_1\\.csv$", data_clean$file)
]

# Check whether each suffixed file has an unsuffixed counterpart
original_filenames <- sub("_1\\.csv$", ".csv", suffix_files)

counterpart_exists <- original_filenames %in% data_clean$file

# Summarise counterpart availability
table(counterpart_exists)

# Display any suffixed files without a corresponding original filename
suffix_files[!counterpart_exists]

#### QC result: All 60 files with an "_1.csv" suffix have
# a corresponding unsuffixed CSV file
# These files require comparison before determining
# whether they represent duplicate analysis records
# No files or analysis identifiers have been modified


# 16. Compare records sharing the same filename-derived analysis ID
# Locate the suffixed and unsuffixed records
suffix_indices <- match(suffix_files, data_clean$file)
original_indices <- match(original_filenames, data_clean$file)

# Select the 20 original analysis fields for comparison
analysis_fields <- setdiff(names(data_clean), "file")

# Check whether the values are identical within each pair
identical_pairs <- vapply(seq_along(suffix_indices), function(i) {
  identical(
    unname(as.character(data_clean[suffix_indices[i], analysis_fields])),
    unname(as.character(data_clean[original_indices[i], analysis_fields]))
  )
}, logical(1))

# Summarise identical and non-identical pairs
table(identical_pairs)

# Identify pairs with different field values
suffix_files[!identical_pairs]

### QC result: 60 pairs of files share the same analysis identifier
# Of these, 27 pairs contain identical values across all 20 fields
# The remaining 33 pairs differ in at least one field
# Non-identical pairs require investigation before database insertion
# No duplicate records have been removed or modified

# 17. Investigate conflicting analysis records

# Identify the fields that differ within each non-identical pair
pair_differences <- lapply(which(!identical_pairs), function(i) {
  
  original <- unlist(data_clean[original_indices[i], analysis_fields],
                     use.names = FALSE)
  
  duplicate <- unlist(data_clean[suffix_indices[i], analysis_fields],
                      use.names = FALSE)
  
  # Compare values, treating two missing values as equivalent
  different <- !(original == duplicate |
                   (is.na(original) & is.na(duplicate)))
  
  different[is.na(different)] <- TRUE
  
  # Return the filenames and names of differing fields
  data.frame(
    original_file = original_filenames[i],
    suffix_file = suffix_files[i],
    differing_fields = paste(analysis_fields[different],
                             collapse = ", ")
  )
})

# Combine the results into one table
pair_differences <- do.call(rbind, pair_differences)

# Display the fields that differ between the 33 pairs
pair_differences

### QC result: 60 pairs of CSV files share an analysis_id
# Of these, 27 pairs are identical across all 20 analysis fields
# The remaining 33 pairs contain conflicting values
# Differences affect gene annotations, allele symbols, phenotype
# parameters, life stages, p-values and LacZ expression fields
# Conflicting records require investigation before database insertion
# No records or analysis identifiers have been modified








