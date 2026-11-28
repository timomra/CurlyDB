# ============================================================
# IMPC Group 13 — SOP Data Cleaning and Validation 02
# ============================================================

# Clean and validate IMPC phenotyping data using the supplied SOP
# Preserve the original data and document cleaning decisions

# ============================================================
# 1. SETUP AND DATA IMPORT
# ============================================================

# Load structurally validated data from Script 01
data_all <- readRDS("Results/Cleaning_intermediates/2-1_structurally_validated.rds")

# Create a copy for cleaning
data_clean <- data_all

# import the SOP
sop <- read.csv("Data/IMPC_SOP_2026.csv")



# ============================================================
# 2. MISSING-VALUE ASSESSMENT
# ============================================================

# Select analysis fields, excluding filenames
analysis_fields <- setdiff(names(data_clean), "file")

# Count missing and blank values in each field
missing_count <- sapply(data_clean[analysis_fields], function(x) {
  sum(is.na(x) | trimws(x) == "")
})

# Summarise missing values and percentages
missing_summary <- data.frame(
  field = names(missing_count),
  missing_count = as.integer(missing_count),
  missing_percentage = round(missing_count / nrow(data_clean) * 100, 2)
)

print(missing_summary)

# Count empty and whitespace-only cells separately
blank_count <- sum(sapply(data_clean[analysis_fields], function(x) {
  sum(!is.na(x) & trimws(x) == "")
}))

print(blank_count)

# Check for non-standard missing-value indicators
missing_indicators <- c("n/a", "null", "missing",
                        "unknown", "not available", "na")

nonstandard_missing <- sum(sapply(data_clean[analysis_fields], function(x) {
  sum(tolower(trimws(as.character(x))) %in% missing_indicators,
      na.rm = TRUE)
}))

print(nonstandard_missing)

## QC Results:
# Missing values identified in 17 of the 20 analysis fields
# gene_accession_id, strain_name and analysis_id contained no missing values
# Missingness ranged from 0.92% to 1.09% across affected fields
# p_value had the highest missingness with 293 records (1.09%)
# No empty or whitespace-only cells were detected
# No additional textual missing-value indicators were detected
# Missing values were already represented as NA
# No imputation or removal of missing values was required

# ============================================================
# 3. P-VALUE VALIDATION
# ============================================================

# Convert p-values to numeric 
p_numeric <- as.numeric(data_clean$p_value)

# Identify non-numeric and out-of-range values
invalid_p_type <- !is.na(data_clean$p_value) & is.na(p_numeric)
invalid_p_range <- !is.na(p_numeric) & (p_numeric < 0 | p_numeric > 1)

# Count invalid values
sum(invalid_p_type)
sum(invalid_p_range)

# Inspect invalid records
invalid_p_records <- data_clean[
  invalid_p_type | invalid_p_range,
  c("file", "analysis_id", "p_value")
]

print(invalid_p_records)

## QC Results:
# All non-missing p-values were successfully converted to numeric
# No non-numeric p-values were detected
# Two p-values were outside the permitted SOP range of 0 to 1
# Value 1.89 was identified in an5163182202.csv
# Value 1.1 was identified in an5363428182.csv
# The value 1.1 originated from a malformed CSV entry
# P-values expressed in scientific notation were interpreted correctly
# Both invalid p-values were flagged for correction
# No values were modified during this validation stage

# ============================================================
# 4. P-VALUE CLEANING
# ============================================================

# Preserve invalid p-values for auditing
p_value_corrections <- invalid_p_records

# Replace invalid p-values with NA
data_clean$p_value[invalid_p_type | invalid_p_range] <- NA_character_

# Confirm both invalid values were corrected
sum(is.na(data_clean$p_value[invalid_p_type | invalid_p_range]))

## QC Results:
# Two p-values exceeded the permitted range of 0 to 1
# The correct statistical values could not be confirmed
# Both invalid values were replaced with NA
# Original values were preserved in data_all and p_value_corrections
# No analysis records were removed

# ============================================================
# 5. P-VALUE CLEANING VALIDATION
# ============================================================

# Convert cleaned p-values to numeric
p_clean_numeric <- as.numeric(data_clean$p_value)

# Check for remaining invalid p-values
remaining_invalid_type <- sum(!is.na(data_clean$p_value) & is.na(p_clean_numeric))
remaining_invalid_range <- sum(!is.na(p_clean_numeric) &
                                 (p_clean_numeric < 0 | p_clean_numeric > 1))

# Count missing p-values after cleaning
missing_p_after <- sum(is.na(data_clean$p_value))

# Display validation results
remaining_invalid_type
remaining_invalid_range
missing_p_after

## QC Results:
# No non-numeric p-values remain
# All non-missing p-values fall within the permitted range of 0 to 1
# Missing p-values increased from 293 to 295 after cleaning
# Two invalid values (1.89 and 1.1) were replaced with NA
# Original values remain preserved in data_all
# No analysis records were removed

# ============================================================
# 6. SOP CHARACTER-LENGTH VALIDATION
# ============================================================

# Select SOP rules for character fields
string_rules <- sop[sop$dataType == "String", ]

# Match SOP field names to dataset columns
string_rules$dataField[string_rules$dataField == "parameter_id"] <- "parameter_stable_id"

# Apply the LacZ rule to all 11 tissue fields
lacz_fields <- grep("^lacz_", names(data_clean), value = TRUE)

lacz_rules <- string_rules[
  rep(which(string_rules$dataField == "lacz_*"), length(lacz_fields)),
]
lacz_rules$dataField <- lacz_fields

string_rules <- rbind(
  string_rules[string_rules$dataField != "lacz_*", ],
  lacz_rules
)

# Count values outside the SOP character-length limits
length_errors <- data.frame(
  field = string_rules$dataField,
  too_short = 0,
  too_long = 0
)

for (i in seq_len(nrow(string_rules))) {
  values <- data_clean[[string_rules$dataField[i]]]
  lengths <- nchar(values)
  
  length_errors$too_short[i] <- sum(lengths < string_rules$minValue[i], na.rm = TRUE)
  length_errors$too_long[i] <- sum(lengths > string_rules$maxValue[i], na.rm = TRUE)
}

print(length_errors)

## QC Results:
# Character-length violations were detected across several fields
# All 26,819 analysis IDs were shorter than the SOP's 15-character minimum
# This conflicts with the SOP's 12-character example and description
# The SOP also contains length inconsistencies for LacZ and strain-name values
# Some explicitly permitted categories exceed the stated maximum lengths
# Other character-length violations require further investigation
# No values were modified during this validation

# ============================================================
# 7. CHARACTER-LENGTH INVESTIGATION
# ============================================================

# Identify gene symbols exceeding 13 characters
long_gene_symbols <- unique(data_clean$gene_symbol[
  nchar(data_clean$gene_symbol) > 13
])

# Identify strain names exceeding 25 characters
long_strain_names <- sort(table(data_clean$strain_name[
  nchar(data_clean$strain_name) > 25
]), decreasing = TRUE)

# Identify life stages exceeding 17 characters
long_life_stages <- unique(data_clean$mouse_life_stage[
  nchar(data_clean$mouse_life_stage) > 17
])

# Display flagged values
print(long_gene_symbols)
print(long_strain_names)
print(long_life_stages)

## QC Results:
# 15 distinct gene symbols exceeded the SOP's 13-character maximum
# Several contained unexpected punctuation or trailing whitespace
# Gene-symbol corrections require further identifier validation
# All 267 overlength strain records contained a permitted 26-character value
# These strain names were retained because they matched the SOP categories
# Three overlength life-stage values contained unexpected inserted characters
# These appeared to correspond to the permitted category Middle aged adult
# No values were modified during this investigation

# ============================================================
# 8. MOUSE LIFE-STAGE CLEANING
# ============================================================

# Define corrections for three malformed life-stage values
life_stage_corrections <- c(
  "Middle_ aged adult" = "Middle aged adult",
  "Middle ag_ed adult" = "Middle aged adult",
  "Middle age(d adult" = "Middle aged adult"
)

# Identify affected records
affected_rows <- which(data_clean$mouse_life_stage %in% names(life_stage_corrections))

# Preserve original values for auditing
life_stage_changes <- data_clean[
  affected_rows,
  c("file", "analysis_id", "mouse_life_stage")
]

# Apply corrections
data_clean$mouse_life_stage[affected_rows] <-
  unname(life_stage_corrections[data_clean$mouse_life_stage[affected_rows]])

# Check for remaining malformed values
sum(data_clean$mouse_life_stage %in% names(life_stage_corrections))

## QC Results:
# Three malformed mouse life-stage values were identified
# All three were corrected to the SOP-permitted category Middle aged adult
# No matching malformed values remain after cleaning
# Original values were preserved in life_stage_changes and data_all
# No analysis records were removed

# ============================================================
# 9. MOUSE LIFE-STAGE CLEANING VALIDATION
# ============================================================

# Confirm all three records were corrected
length(affected_rows)
all(data_clean$mouse_life_stage[affected_rows] == "Middle aged adult")

# Confirm original values were preserved
identical(
  data_all$mouse_life_stage[affected_rows],
  life_stage_changes$mouse_life_stage
)

# Check for remaining character-length violations
sum(nchar(data_clean$mouse_life_stage) > 17, na.rm = TRUE)

## QC Results:
# Three malformed mouse life-stage values were corrected
# All three now match the SOP-permitted category Middle aged adult
# No non-missing life-stage values exceed the 17-character limit
# Original values remain preserved in data_all and life_stage_changes
# No analysis records were removed

# ============================================================
# 10. LACZ CHARACTER-LENGTH INVESTIGATION
# ============================================================

# Identify LacZ values exceeding 19 characters
long_lacz_values <- unlist(lapply(data_clean[lacz_fields], function(x) {
  x[nchar(x) > 19 & !is.na(x)]
}))

# Summarise overlength values and their frequencies
long_lacz_summary <- sort(table(long_lacz_values), decreasing = TRUE)
print(long_lacz_summary)

# Count permitted and other overlength values
sum(long_lacz_values == "tissue not available")
sum(long_lacz_values != "tissue not available")

## QC Results:
# 25,890 LacZ values exceeded the SOP's 19-character maximum
# Of these, 20,828 matched the permitted category "tissue not available"
# This revealed an inconsistency between the SOP length limit and permitted values
# The remaining 5,062 overlength values required further investigation
# Capitalisation, whitespace and unexpected punctuation were observed
# No LacZ values were modified during this investigation

# ============================================================
# 11. LACZ FORMATTING CLEANING
# ============================================================

# Define the permitted LacZ category
valid_lacz_category <- "tissue not available"


# ============================================================
# 11.1 CAPITALISATION AND WHITESPACE
# ============================================================

# Combine values from all 11 LacZ fields
lacz_values <- unlist(data_clean[lacz_fields], use.names = FALSE)

# Standardise values for comparison
lacz_normalised <- tolower(trimws(lacz_values))

# Identify capitalisation and whitespace inconsistencies
simple_lacz_errors <- !is.na(lacz_values) &
  lacz_values != valid_lacz_category &
  lacz_normalised == valid_lacz_category

# Record original and corrected values
lacz_simple_changes <- data.frame(
  field = rep(lacz_fields, each = nrow(data_clean)),
  original_value = lacz_values,
  corrected_value = lacz_normalised
)

lacz_simple_changes <- lacz_simple_changes[simple_lacz_errors, ]

# Apply corrections across all LacZ fields
data_clean[lacz_fields] <- lapply(data_clean[lacz_fields], function(x) {
  normalised <- tolower(trimws(x))
  correct <- !is.na(x) & x != valid_lacz_category &
    normalised == valid_lacz_category
  
  x[correct] <- valid_lacz_category
  x
})

# Count corrections and remaining inconsistencies
nrow(lacz_simple_changes)

sum(unlist(lapply(data_clean[lacz_fields], function(x) {
  !is.na(x) & x != valid_lacz_category &
    tolower(trimws(x)) == valid_lacz_category
})))

## QC Results:
# 333 LacZ values were identified with capitalisation or whitespace inconsistencies
# The original investigation classified 226 as trailing-whitespace inconsistencies
# and 107 as capitalisation inconsistencies
# All 333 values were standardised to the permitted category "tissue not available"
# No matching capitalisation or whitespace inconsistencies remain
# Original values were preserved in data_all and lacz_simple_changes
# No analysis records were removed


# ============================================================
# 11.2 PUNCTUATION CORRECTIONS
# ============================================================

# Combine updated LacZ values
lacz_values_current <- unlist(data_clean[lacz_fields], use.names = FALSE)

# Remove punctuation for comparison only
lacz_without_punctuation <- gsub(
  "[^[:alnum:] ]", "",
  tolower(trimws(lacz_values_current))
)

# Identify punctuation inconsistencies
punctuation_matches <- !is.na(lacz_values_current) &
  lacz_values_current != valid_lacz_category &
  lacz_without_punctuation == valid_lacz_category

# Record original and corrected values
lacz_punctuation_changes <- data.frame(
  field = rep(lacz_fields, each = nrow(data_clean)),
  original_value = lacz_values_current,
  corrected_value = valid_lacz_category
)

lacz_punctuation_changes <- lacz_punctuation_changes[punctuation_matches, ]

# Apply corrections across all LacZ fields
data_clean[lacz_fields] <- lapply(data_clean[lacz_fields], function(x) {
  normalised <- gsub("[^[:alnum:] ]", "", tolower(trimws(x)))
  correct <- !is.na(x) & x != valid_lacz_category &
    normalised == valid_lacz_category
  
  x[correct] <- valid_lacz_category
  x
})

# Count corrections and remaining punctuation errors
nrow(lacz_punctuation_changes)

remaining_punctuation_errors <- sum(unlist(
  lapply(data_clean[lacz_fields], function(x) {
    normalised <- gsub("[^[:alnum:] ]", "", tolower(trimws(x)))
    
    !is.na(x) & x != valid_lacz_category &
      normalised == valid_lacz_category
  })
))

print(remaining_punctuation_errors)

## QC Results:
# 4,728 LacZ values were identified with punctuation-related inconsistencies
# All 4,728 matched "tissue not available" after punctuation removal for comparison
# The identified values were replaced with the permitted category
# No matching punctuation inconsistencies remain after cleaning
# Original values were preserved in data_all and lacz_punctuation_changes
# No analysis records were removed

# ============================================================
# 12. LACZ CATEGORY AND CHARACTER-LENGTH VALIDATION
# ============================================================

# Define the six permitted LacZ categories
valid_lacz_values <- c(
  "No data",
  "ambiguous",
  "expression",
  "imageOnly",
  "no expression",
  "tissue not available"
)

# Count invalid categories in each LacZ field
invalid_lacz <- lapply(data_clean[lacz_fields], function(x) {
  !is.na(x) & !(x %in% valid_lacz_values)
})

invalid_lacz_counts <- sapply(invalid_lacz, sum)
print(invalid_lacz_counts)

# Inspect invalid values and their frequencies
invalid_lacz_values <- unlist(lapply(lacz_fields, function(field) {
  data_clean[[field]][invalid_lacz[[field]]]
}), use.names = FALSE)

sum(invalid_lacz_counts)
sort(table(invalid_lacz_values), decreasing = TRUE)


# ============================================================
# 12.1 INVESTIGATE REMAINING LENGTH ERRORS
# ============================================================

# Identify values exceeding 19 characters, excluding the permitted exception
remaining_lacz_length_errors <- lapply(data_clean[lacz_fields], function(x) {
  !is.na(x) & nchar(x) > 19 & x != "tissue not available"
})

# Count remaining character-length violations
sum(unlist(remaining_lacz_length_errors))

# Inspect the affected lung record
lacz_length_anomaly <- data_clean[
  remaining_lacz_length_errors$lacz_lung,
  c("file", "analysis_id", "lacz_lung")
]

print(lacz_length_anomaly)

## QC Results:
# One LacZ value exceeded the 19-character maximum after previous corrections
# The permitted category "tissue not available" was excluded from this check
# The remaining anomaly was "no expression asdasdasd" in lacz_lung
# This value required source-level investigation before correction
# No values were modified during this investigation


# ============================================================
# 12.2 CORRECT AND VALIDATE THE LACZ ANOMALY
# ============================================================

# Inspect the original CSV record
raw_lacz_record <- readLines(
  "Data/group13_data/an5433186127.csv"
)

grep("lacz_lung", raw_lacz_record, value = TRUE, fixed = TRUE)

# Identify the affected record
lacz_lung_row <- which(data_clean$file == "an5433186127.csv")

# Confirm the original value before correction
stopifnot(
  length(lacz_lung_row) == 1,
  data_clean$lacz_lung[lacz_lung_row] == "no expression asdasdasd"
)

# Correct the source-verified value
data_clean$lacz_lung[lacz_lung_row] <- "no expression"

# Confirm the corrected value
data_clean$lacz_lung[lacz_lung_row]

# Check for remaining length violations
remaining_lacz_length_count <- sum(unlist(
  lapply(data_clean[lacz_fields], function(x) {
    !is.na(x) & nchar(x) > 19 & x != "tissue not available"
  })
))

print(remaining_lacz_length_count)

## QC Results:
# The malformed value was traced to an5433186127.csv
# Extraneous text was identified after the intended category
# The value was corrected from "no expression asdasdasd" to "no expression"
# No unexplained LacZ character-length violations remain
# The SOP-permitted category "tissue not available" was retained
# Original values remain preserved in data_all
# No analysis records were removed

# ============================================================
# 13. LACZ CATEGORY STANDARDISATION AND VALIDATION
# ============================================================

# ============================================================
# 13.1 IDENTIFY AND CORRECT INVALID CATEGORIES
# ============================================================

# Normalise values for comparison only
normalise_lacz <- function(x) {
  gsub("[^[:alnum:]]", "", tolower(trimws(x)))
}

# Create comparison keys for the permitted categories
valid_lacz_keys <- normalise_lacz(valid_lacz_values)

# Identify invalid LacZ values
invalid_lacz <- lapply(data_clean[lacz_fields], function(x) {
  !is.na(x) & !(x %in% valid_lacz_values)
})

invalid_lacz_values <- unlist(lapply(lacz_fields, function(field) {
  data_clean[[field]][invalid_lacz[[field]]]
}), use.names = FALSE)

# Check whether invalid values match permitted categories
lacz_category_matches <- match(
  normalise_lacz(invalid_lacz_values),
  valid_lacz_keys
)

# Count matched and unmatched values
sum(!is.na(lacz_category_matches))
sum(is.na(lacz_category_matches))

# Record corrections before modifying the dataset
lacz_category_changes <- do.call(rbind, lapply(lacz_fields, function(field) {
  values <- data_clean[[field]]
  category_index <- match(normalise_lacz(values), valid_lacz_keys)
  
  correct <- !is.na(values) &
    !(values %in% valid_lacz_values) &
    !is.na(category_index)
  
  data.frame(
    file = data_clean$file[correct],
    analysis_id = data_clean$analysis_id[correct],
    field = field,
    original_value = values[correct],
    corrected_value = valid_lacz_values[category_index[correct]]
  )
}))

# Confirm all invalid values have a correction
stopifnot(
  !anyDuplicated(valid_lacz_keys),
  !anyNA(lacz_category_matches),
  nrow(lacz_category_changes) == length(invalid_lacz_values)
)

# Apply corrections across all 11 LacZ fields
data_clean[lacz_fields] <- lapply(data_clean[lacz_fields], function(x) {
  category_index <- match(normalise_lacz(x), valid_lacz_keys)
  
  correct <- !is.na(x) &
    !(x %in% valid_lacz_values) &
    !is.na(category_index)
  
  x[correct] <- valid_lacz_values[category_index[correct]]
  x
})

# Count applied corrections
nrow(lacz_category_changes)

## QC Results:
# 31,476 LacZ values did not exactly match the six permitted categories
# All 31,476 values matched a permitted category after normalisation
# No unmatched or ambiguous category mappings were identified
# Corrections addressed capitalisation, whitespace and punctuation
# All identified values were standardised to their matching SOP categories
# Original values were preserved in data_all and lacz_category_changes


# ============================================================
# 13.2 VALIDATE FINAL LACZ CATEGORIES
# ============================================================

# Count remaining invalid values in each LacZ field
remaining_invalid_lacz <- sapply(data_clean[lacz_fields], function(x) {
  sum(!is.na(x) & !(x %in% valid_lacz_values))
})

print(remaining_invalid_lacz)

# Compare missing values before and after cleaning
lacz_missing_before <- sapply(data_all[lacz_fields], function(x) {
  sum(is.na(x))
})

lacz_missing_after <- sapply(data_clean[lacz_fields], function(x) {
  sum(is.na(x))
})

# Confirm valid categories and unchanged missingness
stopifnot(
  sum(remaining_invalid_lacz) == 0,
  identical(lacz_missing_before, lacz_missing_after)
)

## QC Results:
# No invalid non-missing LacZ categories remain across the 11 tissue fields
# Every non-missing value now matches one of the six SOP-permitted categories
# Missing-value counts were unchanged across all 11 LacZ fields
# Original LacZ values remain preserved in data_all
# No analysis records were removed

# ============================================================
# 14. STRAIN-NAME VALIDATION
# ============================================================

# Define the nine SOP-permitted strain names
valid_strains <- c(
  "B6Brd;B6Dnk;B6N-Tyr<c-Brd>",
  "C57BL/6Brd-Tyr<c-Brd>",
  "C57BL/6Dnk",
  "C57BL/6JTyr;C57BL/6N",
  "C57BL/6N",
  "C57BL/6NCrl",
  "C57BL/6NJ",
  "C57BL/6NTac",
  "C57BL/6NTacDen"
)

# Count strain names outside the permitted categories
invalid_strains <- sum(!is.na(data_clean$strain_name) &
                         !(data_clean$strain_name %in% valid_strains))

print(invalid_strains)

## QC Results:
# All 26,819 strain names matched one of the nine SOP-permitted categories
# The combined strain "C57BL/6JTyr;C57BL/6N" was retained as a permitted value
# The strain "B6Brd;B6Dnk;B6N-Tyr<c-Brd>" occurred in 267 records
# This permitted strain exceeds the SOP's stated 25-character maximum
# The inconsistency was documented without changing the strain names
# No invalid strain categories were identified and no corrections were required
# SOP compliance does not independently confirm experimental provenance
# Record-level verification would require original IMPC metadata
# No analysis records were removed

# ============================================================
# 15. MOUSE LIFE-STAGE CATEGORY VALIDATION
# ============================================================

# Define the seven permitted life-stage categories
valid_life_stages <- c(
  "E12.5",
  "E15.5",
  "E18.5",
  "E9.5",
  "Early adult",
  "Late adult",
  "Middle aged adult"
)

# Identify values outside the permitted categories
invalid_life_stages <- !is.na(data_clean$mouse_life_stage) &
  !(data_clean$mouse_life_stage %in% valid_life_stages)

# Count and inspect invalid values
sum(invalid_life_stages)

head(sort(table(data_clean$mouse_life_stage[invalid_life_stages]),
          decreasing = TRUE), 30)

## QC Results:
# 3,160 mouse life-stage values did not exactly match the seven permitted categories
# The main inconsistencies involved capitalisation, trailing whitespace and punctuation
# These values require further investigation before correction
# No additional life-stage values were modified during this validation
# No analysis records were removed

# ============================================================
# 16. MOUSE LIFE-STAGE STANDARDISATION
# ============================================================

# Normalise values for comparison only
normalise_category <- function(x) {
  gsub("[^[:alnum:]]", "", tolower(trimws(x)))
}

# Create comparison keys for permitted life stages
valid_life_stage_keys <- normalise_category(valid_life_stages)

# Match life-stage values to permitted categories
life_stage_category_index <- match(
  normalise_category(data_clean$mouse_life_stage),
  valid_life_stage_keys
)

# Identify invalid values with a permitted-category match
life_stage_to_correct <- invalid_life_stages &
  !is.na(life_stage_category_index)

# Record original and corrected values
life_stage_category_changes <- data.frame(
  file = data_clean$file[life_stage_to_correct],
  analysis_id = data_clean$analysis_id[life_stage_to_correct],
  original_value = data_clean$mouse_life_stage[life_stage_to_correct],
  corrected_value = valid_life_stages[
    life_stage_category_index[life_stage_to_correct]
  ]
)

# Confirm all invalid values have a valid match
stopifnot(
  !anyDuplicated(valid_life_stage_keys),
  sum(life_stage_to_correct) == sum(invalid_life_stages)
)

# Count identified corrections
nrow(life_stage_category_changes)

# Apply corrections
data_clean$mouse_life_stage[life_stage_to_correct] <-
  life_stage_category_changes$corrected_value

## QC Results:
# 3,160 mouse life-stage values required standardisation
# All 3,160 matched one of the seven permitted categories after normalisation
# No unmatched or ambiguous category mappings were identified
# Corrections addressed capitalisation, whitespace and punctuation
# Original values were preserved in data_all and life_stage_category_changes
# No analysis records were removed

# ============================================================
# 17. MOUSE LIFE-STAGE CLEANING VALIDATION
# ============================================================

# Confirm the number of corrections
nrow(life_stage_category_changes)

# Verify that recorded corrections were applied
all(data_clean$mouse_life_stage[life_stage_to_correct] ==
      life_stage_category_changes$corrected_value)

# Count remaining invalid life-stage categories
remaining_invalid_life_stages <- sum(
  !is.na(data_clean$mouse_life_stage) &
    !(data_clean$mouse_life_stage %in% valid_life_stages)
)

print(remaining_invalid_life_stages)

# Confirm missing-value counts were preserved
identical(
  sum(is.na(data_all$mouse_life_stage)),
  sum(is.na(data_clean$mouse_life_stage))
)

## QC Results:
# 3,160 mouse life-stage values were standardised to permitted categories
# All recorded corrections were successfully applied
# No invalid non-missing life-stage categories remain
# The three previously identified character-length errors were also corrected
# Missing-value counts were unchanged after cleaning
# Original values remain preserved in data_all and life_stage_category_changes
# No analysis records were removed

# ============================================================
# 18. GENE-SYMBOL FORMATTING INVESTIGATION
# ============================================================

# Extract gene symbols
gene_symbols <- data_clean$gene_symbol

# Identify character-length violations
invalid_gene_length <- !is.na(gene_symbols) &
  (nchar(gene_symbols) < 1 | nchar(gene_symbols) > 13)

# Identify non-alphanumeric characters
invalid_gene_characters <- !is.na(gene_symbols) &
  grepl("[^[:alnum:]]", gene_symbols)

# Identify leading or trailing whitespace
invalid_gene_whitespace <- !is.na(gene_symbols) &
  gene_symbols != trimws(gene_symbols)

# Identify hyphens and other unexpected characters
hyphenated_gene_symbols <- !is.na(gene_symbols) &
  grepl("-", gene_symbols, fixed = TRUE)

suspicious_gene_characters <- !is.na(gene_symbols) &
  grepl("[^[:alnum:]-]", gene_symbols)

# Summarise formatting inconsistencies
gene_symbol_qc <- data.frame(
  check = c(
    "Invalid length",
    "Non-alphanumeric characters",
    "Leading/trailing whitespace",
    "Symbols containing hyphens",
    "Other unexpected characters"
  ),
  affected_records = c(
    sum(invalid_gene_length),
    sum(invalid_gene_characters),
    sum(invalid_gene_whitespace),
    sum(hyphenated_gene_symbols),
    sum(suspicious_gene_characters)
  )
)

print(gene_symbol_qc)

# Inspect the most frequent suspicious gene symbols
head(sort(table(gene_symbols[suspicious_gene_characters]),
          decreasing = TRUE), 30)

## QC Results:
# Gene symbols were assessed against the SOP's 1–13 character requirement
# Non-alphanumeric characters and surrounding whitespace were investigated
# Hyphenated symbols were examined separately from other unexpected characters
# Formatting inconsistencies were identified for subsequent cleaning
# No gene-symbol values were modified during this investigation

# ============================================================
# 19. GENE-SYMBOL CLEANING AND VALIDATION
# ============================================================

# ============================================================
# 19.1 WHITESPACE CORRECTIONS
# ============================================================

# Identify gene symbols with surrounding whitespace
gene_whitespace_rows <- which(
  !is.na(data_clean$gene_symbol) &
    data_clean$gene_symbol != trimws(data_clean$gene_symbol)
)

# Record original and corrected values
gene_whitespace_changes <- data.frame(
  file = data_clean$file[gene_whitespace_rows],
  analysis_id = data_clean$analysis_id[gene_whitespace_rows],
  original_value = data_clean$gene_symbol[gene_whitespace_rows],
  corrected_value = trimws(data_clean$gene_symbol[gene_whitespace_rows])
)

# Apply corrections
data_clean$gene_symbol[gene_whitespace_rows] <-
  gene_whitespace_changes$corrected_value

# Count corrections and remaining whitespace errors
nrow(gene_whitespace_changes)

sum(!is.na(data_clean$gene_symbol) &
      data_clean$gene_symbol != trimws(data_clean$gene_symbol))

## QC Results:
# 277 gene symbols contained leading or trailing whitespace
# All 277 values were corrected using trimws()
# No surrounding whitespace inconsistencies remain
# Original values were preserved in data_all and gene_whitespace_changes
# No missing values or analysis records were removed


# ============================================================
# 19.2 GENE-SYMBOL FORMATTING
# ============================================================

# Preserve values before formatting
original_symbols <- data_clean$gene_symbol

# Remove characters other than letters and numbers
clean_symbols <- gsub("[^A-Za-z0-9]", "", original_symbols)

# Capitalise the first character
clean_symbols <- ifelse(
  is.na(clean_symbols),
  NA_character_,
  paste0(
    toupper(substr(clean_symbols, 1, 1)),
    substr(clean_symbols, 2, nchar(clean_symbols))
  )
)

# Record modified values
gene_symbol_changes <- data.frame(
  file = data_clean$file,
  analysis_id = data_clean$analysis_id,
  original_value = original_symbols,
  corrected_value = clean_symbols
)

gene_symbol_changes <- gene_symbol_changes[
  !is.na(original_symbols) & original_symbols != clean_symbols,
]

# Apply corrections
data_clean$gene_symbol <- clean_symbols

# Count corrections
nrow(gene_symbol_changes)

# Validate gene-symbol formatting
stopifnot(all(
  grepl("^[A-Z0-9][A-Za-z0-9]*$",
        data_clean$gene_symbol[!is.na(data_clean$gene_symbol)])
))

# Check remaining character-length violations
remaining_gene_length_errors <- sum(
  !is.na(data_clean$gene_symbol) &
    (nchar(data_clean$gene_symbol) < 1 |
       nchar(data_clean$gene_symbol) > 13)
)

print(remaining_gene_length_errors)

# Confirm missing values were preserved
identical(
  sum(is.na(data_all$gene_symbol)),
  sum(is.na(data_clean$gene_symbol))
)

## QC Results:
# 1,570 gene symbols were standardised by removing unexpected characters
# and capitalising the first character
# All non-missing gene symbols meet the required alphanumeric format
# No gene symbols violate the SOP's 1–13 character-length requirement
# Original values were preserved in data_all and gene_symbol_changes
# Missing-value counts were unchanged
# No analysis records were removed

# ============================================================
# 20. GENE ACCESSION ID VALIDATION
# ============================================================

# Identify gene accession IDs that do not match the expected MGI format
invalid_gene_accessions <- !is.na(data_clean$gene_accession_id) &
  !grepl("^MGI:[0-9]+$", data_clean$gene_accession_id)

# Count invalid accession IDs
sum(invalid_gene_accessions)

## QC Results:
# All non-missing gene accession IDs followed the expected MGI:number format
# No invalid accession ID formats were detected
# No corrections were required
# No analysis records were removed

# ============================================================
# 21. ALLELE-SYMBOL CLEANING AND VALIDATION
# ============================================================

# ============================================================
# 21.1 WHITESPACE CORRECTIONS
# ============================================================

# Identify allele symbols with surrounding whitespace
allele_whitespace_rows <- which(
  !is.na(data_clean$allele_symbol) &
    data_clean$allele_symbol != trimws(data_clean$allele_symbol)
)

# Record original and corrected values
allele_whitespace_changes <- data.frame(
  file = data_clean$file[allele_whitespace_rows],
  analysis_id = data_clean$analysis_id[allele_whitespace_rows],
  original_value = data_clean$allele_symbol[allele_whitespace_rows],
  corrected_value = trimws(data_clean$allele_symbol[allele_whitespace_rows])
)

# Apply whitespace corrections
data_clean$allele_symbol[allele_whitespace_rows] <-
  allele_whitespace_changes$corrected_value

# Count corrections and remaining whitespace errors
nrow(allele_whitespace_changes)

remaining_allele_whitespace <- sum(
  !is.na(data_clean$allele_symbol) &
    data_clean$allele_symbol != trimws(data_clean$allele_symbol)
)

print(remaining_allele_whitespace)

## QC Results:
# 254 allele symbols contained leading or trailing whitespace
# All identified values were corrected using trimws()
# No surrounding whitespace inconsistencies remain
# Original values were preserved in data_all and allele_whitespace_changes
# Missing-value counts were unchanged
# No analysis records were removed


# ============================================================
# 21.2 PUNCTUATION CORRECTIONS
# ============================================================

# Preserve values before formatting
original_alleles <- data_clean$allele_symbol

# Remove unexpected characters while preserving allele notation
clean_alleles <- gsub("[^A-Za-z0-9<>().-]", "", original_alleles)

# Identify modified records
allele_punctuation_rows <- which(
  !is.na(original_alleles) &
    original_alleles != clean_alleles
)

# Record original and corrected values
allele_punctuation_changes <- data.frame(
  file = data_clean$file[allele_punctuation_rows],
  analysis_id = data_clean$analysis_id[allele_punctuation_rows],
  original_value = original_alleles[allele_punctuation_rows],
  corrected_value = clean_alleles[allele_punctuation_rows]
)

# Apply corrections
data_clean$allele_symbol <- clean_alleles

# Count corrections and remaining invalid characters
nrow(allele_punctuation_changes)

remaining_invalid_allele_chars <- sum(
  !is.na(data_clean$allele_symbol) &
    grepl("[^A-Za-z0-9<>().-]", data_clean$allele_symbol)
)

print(remaining_invalid_allele_chars)

# Confirm missing-value counts were preserved
identical(
  sum(is.na(data_all$allele_symbol)),
  sum(is.na(data_clean$allele_symbol))
)

## QC Results:
# 3,899 allele symbols were standardised by removing unexpected characters
# Letters, numbers, angle brackets, parentheses, dots and hyphens were preserved
# No unexpected characters remain after cleaning
# Original values were preserved in data_all and allele_punctuation_changes
# Missing-value counts were unchanged
# No analysis records were removed

# ============================================================
# 22. ALLELE PARENTHESES VALIDATION
# ============================================================

# Count opening and closing parentheses
count_open <- nchar(gsub("[^(]", "", data_clean$allele_symbol))
count_close <- nchar(gsub("[^)]", "", data_clean$allele_symbol))

# Identify allele symbols with unequal parentheses counts
invalid_allele_brackets <- !is.na(data_clean$allele_symbol) &
  count_open != count_close

# Preserve affected records for review
allele_bracket_issues <- data_clean[
  invalid_allele_brackets,
  c("file", "analysis_id", "allele_symbol")
]

# Count affected records
nrow(allele_bracket_issues)

## QC Results:
# 828 allele symbols contained unequal numbers of opening and closing parentheses
# These records were flagged for further investigation
# No automatic corrections were applied because the intended allele notation could not be confirmed
# Original values were retained to avoid potentially altering biological identifiers
# No analysis records were removed

# ============================================================
# 23. PARAMETER ID CLEANING AND VALIDATION
# ============================================================

# Load the IMPC parameter reference file
parameters <- read.csv("Data/IMPC_parameter_description.txt",
                       strip.white = TRUE)


# ============================================================
# 23.1 REFERENCE-VERIFIED CORRECTIONS
# ============================================================

# Preserve original parameter IDs
original_ids <- data_clean$parameter_stable_id

# Remove unexpected punctuation for comparison
clean_ids <- gsub("[^A-Za-z0-9_]", "", original_ids)

# Identify corrections matching the IMPC reference
valid_corrections <- !is.na(original_ids) &
  original_ids != clean_ids &
  clean_ids %in% parameters$parameterId

# Record original and corrected values
parameter_id_changes <- data.frame(
  file = data_clean$file[valid_corrections],
  analysis_id = data_clean$analysis_id[valid_corrections],
  original_value = original_ids[valid_corrections],
  corrected_value = clean_ids[valid_corrections]
)

# Apply verified corrections
data_clean$parameter_stable_id[valid_corrections] <- clean_ids[valid_corrections]

# Count corrections and remaining inconsistencies
nrow(parameter_id_changes)

remaining_invalid_parameter_ids <- sum(
  !is.na(data_clean$parameter_stable_id) &
    !grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$",
           data_clean$parameter_stable_id)
)

remaining_unmatched_parameter_ids <- sum(
  !is.na(data_clean$parameter_stable_id) &
    !(data_clean$parameter_stable_id %in% parameters$parameterId)
)

print(remaining_invalid_parameter_ids)
print(remaining_unmatched_parameter_ids)

## QC Results:
# 3,040 parameter IDs were corrected using exact matches in the IMPC reference
# All corrected identifiers matched entries in the supplied reference file
# A further 2,202 records retained invalid parameter ID formatting
# 5,984 records contained IDs absent from the reference file
# Unresolved identifiers were retained for further investigation
# Original values were preserved in data_all and parameter_id_changes
# No analysis records were removed

# ============================================================
# 23.2 REMAINING FORMATTING CORRECTIONS
# ============================================================

# Identify remaining invalid parameter ID formats
invalid_rows <- !is.na(data_clean$parameter_stable_id) &
  !grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$",
         data_clean$parameter_stable_id)

# Remove unexpected punctuation while preserving underscores
candidates <- gsub("[^A-Za-z0-9_]", "", data_clean$parameter_stable_id)

# Select corrections that satisfy the required format
fix_rows <- invalid_rows &
  grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", candidates)

# Record original and corrected values
parameter_format_changes <- data.frame(
  file = data_clean$file[fix_rows],
  original_value = data_clean$parameter_stable_id[fix_rows],
  corrected_value = candidates[fix_rows]
)

# Apply formatting corrections
data_clean$parameter_stable_id[fix_rows] <- candidates[fix_rows]

# Count corrections and remaining invalid formats
nrow(parameter_format_changes)

remaining_invalid_parameter_ids <- sum(
  !is.na(data_clean$parameter_stable_id) &
    !grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$",
           data_clean$parameter_stable_id)
)

print(remaining_invalid_parameter_ids)

## QC Results:
# A further 2,065 parameter IDs were standardised by removing unexpected punctuation
# Corrections preserved letters, numbers and underscores
# All corrected values satisfied the required identifier format
# 137 records still failed the SOP formatting requirement
# 5,984 records contained IDs absent from the IMPC reference file
# Formatting corrections were not independently verified against reference identifiers
# Unresolved values were retained for further cleaning
# No analysis records were removed

# ============================================================
# 24. FINAL PARAMETER ID STANDARDISATION AND VALIDATION
# ============================================================

# ============================================================
# 24.1 STANDARDISE UNDERSCORES AND CAPITALISATION
# ============================================================

# Preserve parameter IDs before cleaning
original_remaining_ids <- data_clean$parameter_stable_id

# Standardise underscores and capitalisation
corrected_ids <- gsub("_+", "_", original_remaining_ids)
corrected_ids <- toupper(corrected_ids)
corrected_ids <- gsub("^_|_$", "", corrected_ids)

# Identify modified records
remaining_id_rows <- which(
  !is.na(original_remaining_ids) &
    original_remaining_ids != corrected_ids
)

# Record original and corrected values
remaining_parameter_changes <- data.frame(
  file = data_clean$file[remaining_id_rows],
  original_value = original_remaining_ids[remaining_id_rows],
  corrected_value = corrected_ids[remaining_id_rows]
)

# Apply corrections
data_clean$parameter_stable_id[remaining_id_rows] <-
  remaining_parameter_changes$corrected_value

# Count corrections
nrow(remaining_parameter_changes)

# ============================================================
# 24.2 REMOVE REMAINING PUNCTUATION
# ============================================================

# Identify IDs that still violate the required format
remaining_invalid <- !is.na(data_clean$parameter_stable_id) &
  !grepl("^[A-Z0-9]+(_[A-Z0-9]+)+$",
         data_clean$parameter_stable_id)

# Remove unexpected punctuation from invalid IDs
original_remaining <- data_clean$parameter_stable_id[remaining_invalid]
corrected_remaining <- gsub("[^A-Z0-9_]", "", original_remaining)

# Record original and corrected values
final_parameter_changes <- data.frame(
  file = data_clean$file[remaining_invalid],
  original_value = original_remaining,
  corrected_value = corrected_remaining
)

# Apply corrections
data_clean$parameter_stable_id[remaining_invalid] <- corrected_remaining

# Count corrections
nrow(final_parameter_changes)

#=============================================================
# 24.3 REBUILD PARAMETER IDS FROM THEIR FOUR PARTS
#=============================================================

# Every parameter ID has four parts: pipeline_procedure_number_version
# (e.g. IMPC_EYE_012_001). The last nine characters are always a
# 3-character procedure code and two 3-digit numbers, so the underscores
# can be removes and put back at fixed positions.

# Preserve parameter IDs before rebuilding
original_ids_rebuild <- data_clean$parameter_stable_id

#Remove all underscores, then cut into the four parts
joined <- gsub("_", "", original_ids_rebuild)
n <- nchar(joined)

rebuilt_ids <- paste(substr(joined, 1, n - 9),     # pipeline e.g.IMPC
                     substr(joined, n - 8, n - 6), # procedure e.g. EYE
                     substr(joined, n - 5, n -3),  # parameter e.g. 012
                     substr(joined, n - 2, n),     # version e.g. 001
                     sep = "_")
rebuilt_ids[is.na(original_ids_rebuild)] <- NA

# Record original and corrected values
rebuild_rows <- which(!is.na(original_ids_rebuild)& original_ids_rebuild != rebuilt_ids)

parameter_rebuild_changes <- data.frame(
  file = data_clean$file[rebuild_rows],
  original_value = original_ids_rebuild[rebuild_rows],
  corrected_value = rebuilt_ids[rebuild_rows]
)

# Apply corrections
data_clean$parameter_stable_id <-  rebuilt_ids

# Count corrections
nrow(parameter_rebuild_changes)

##QC Results:
# Underscores inserted inside ID components were not removed by earlier steps
# (e.g. IMPC_E_YE_012_001, IMP_C_DXA_004_001)
# All underscores were removed and re-inserted at fixed positions
# 175 paramter IDs were corrected
# Original values were preserved in data_all and parameter_rebuild_changes
# No analysis records were removed
# ============================================================
# 24.4 FINAL VALIDATION
# ============================================================

# Count remaining formatting errors
remaining_format_errors <- sum(
  !is.na(data_clean$parameter_stable_id) &
    !grepl("^[A-Z0-9]+_[A-Z0-9]{3}_[0-9]{3}_[0-9]{3}$",
           data_clean$parameter_stable_id)
)

print(remaining_format_errors)

# Count IDs absent from the IMPC reference
remaining_unmatched_parameter_ids <- sum(
  !is.na(data_clean$parameter_stable_id) &
    !(data_clean$parameter_stable_id %in% parameters$parameterId)
)

print(remaining_unmatched_parameter_ids)

## QC Results:
# Consecutive underscores were reduced to single underscores
# Parameter IDs were standardised to uppercase
# Leading and trailing underscores were removed
# Remaining unexpected punctuation was removed from invalid-format IDs
# IDs with underscores inside componenets were rebuilt in 24.3
# All non-missing parameter IDs now satisfy the required SOP formatting rule
# No parameter ID formatting violations remain
# 5,758 records still lack an exact match in the supplied IMPC reference file
# These unmatched identifiers remain unverified and were retained
# Correct formatting does not guarantee that an ID exists in the IMPC reference
# Original values were preserved in data_all and the correction audit tables
# No analysis records were removed

# ============================================================
# 25. PARAMETER NAME VALIDATION
# ============================================================

# Count missing parameter names
sum(is.na(data_clean$parameter_name))

# Check for leading or trailing whitespace
sum(!is.na(data_clean$parameter_name) &
      data_clean$parameter_name != trimws(data_clean$parameter_name))

# Count names absent from the IMPC reference
sum(!is.na(data_clean$parameter_name) &
      !(data_clean$parameter_name %in% parameters$name))

## QC Results:
# No missing parameter names were detected
# No leading or trailing whitespace inconsistencies were identified
# 3,585 parameter names did not exactly match the supplied IMPC reference
# Unmatched names were retained for further database consistency checks
# A missing reference match does not necessarily indicate an incorrect name
# No parameter names were modified or analysis records removed

# ============================================================
# 26. GENE–ALLELE PREFIX CLEANING
# ============================================================

# Extract the gene prefix before <
allele_prefix <- sub("<.*$", "", data_clean$allele_symbol)

# Normalise gene symbols for comparison
normalise_gene <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", x))
}

# Identify prefixes that match after normalisation
fix_rows <- !is.na(data_clean$gene_symbol) &
  !is.na(data_clean$allele_symbol) &
  grepl("<", data_clean$allele_symbol, fixed = TRUE) &
  data_clean$gene_symbol != allele_prefix &
  normalise_gene(data_clean$gene_symbol) == normalise_gene(allele_prefix)

fix_rows[is.na(fix_rows)] <- FALSE

# Record original and corrected allele symbols
gene_allele_changes <- data.frame(
  file = data_clean$file[fix_rows],
  original_value = data_clean$allele_symbol[fix_rows],
  corrected_value = paste0(
    data_clean$gene_symbol[fix_rows],
    sub("^[^<]*", "", data_clean$allele_symbol[fix_rows])
  )
)

# Apply corrections
data_clean$allele_symbol[fix_rows] <-
  gene_allele_changes$corrected_value

# Count corrections and remaining mismatches
nrow(gene_allele_changes)

sum(!is.na(data_clean$gene_symbol) &
      !is.na(data_clean$allele_symbol) &
      data_clean$gene_symbol !=
      sub("<.*$", "", data_clean$allele_symbol))

## QC Results:
# 673 gene–allele prefix inconsistencies were corrected
# These prefixes matched their corresponding gene symbols after normalisation
# Corrections addressed differences in capitalisation and punctuation
# Only the gene prefix before < was corrected
# The remaining allele designation was preserved
# Two gene–allele prefix mismatches remained for individual review
# Original values were retained in data_all and gene_allele_changes
# No analysis records were removed

# ============================================================
# 27. FINAL GENE–ALLELE CORRECTIONS AND VALIDATION
# ============================================================

# Identify the two remaining records
rows <- match(
  c("an5285097670.csv", "an8482090749.csv"),
  data_clean$file
)

# Verify the original allele symbols
stopifnot(
  !anyNA(rows),
  identical(
    data_clean$allele_symbol[rows],
    c("<tm1.1(KOMP)Vlcg>", "fxn5<tm1b(EUCOMM)Hmgu>")
  )
)

# Record original and corrected values
final_allele_changes <- data.frame(
  file = data_clean$file[rows],
  original_value = data_clean$allele_symbol[rows],
  corrected_value = c(
    "Ift70a2<tm1.1(KOMP)Vlcg>",
    "Sfxn5<tm1b(EUCOMM)Hmgu>"
  )
)

# Apply corrections
data_clean$allele_symbol[rows] <-
  final_allele_changes$corrected_value

# Count remaining gene–allele prefix mismatches
remaining_mismatches <- sum(
  !is.na(data_clean$gene_symbol) &
    !is.na(data_clean$allele_symbol) &
    data_clean$gene_symbol !=
    sub("<.*$", "", data_clean$allele_symbol)
)

print(remaining_mismatches)

stopifnot(remaining_mismatches == 0)

## QC Results:
# Two remaining gene–allele prefix inconsistencies were corrected individually
# The first record lacked the gene prefix Ift70a2
# The second contained fxn5 instead of Sfxn5
# Corrections were based on the corresponding gene symbols
# The allele-specific designations after < were preserved
# A total of 675 gene–allele prefix inconsistencies were corrected
# No gene–allele prefix mismatches remain
# Original values were preserved in data_all and the correction audit tables
# No analysis records were removed

# ============================================================
# 28. DUPLICATE REMOVAL AND FINAL QC
# ============================================================

# Record duplicate analysis IDs before removal
duplicate_records <- data_clean[
  duplicated(data_clean$analysis_id),
]

# Keep the first occurrence of each analysis ID
data_clean <- data_clean[
  !duplicated(data_clean$analysis_id),
]

# Count removed records
nrow(duplicate_records)

# Confirm all remaining analysis IDs are unique
stopifnot(
  nrow(data_clean) == 26759,
  anyDuplicated(data_clean$analysis_id) == 0
)

## QC Results:
# 60 records with repeated analysis IDs were removed
# The first occurrence of each analysis ID was retained
# Removed records were preserved in duplicate_records for auditing
# The cleaned dataset contains 26,759 records
# No duplicate analysis IDs remain


# Save the cleaned dataset
saveRDS(data_clean, "Results/Cleaning_intermediates/2-2_Cleaning.rds")

