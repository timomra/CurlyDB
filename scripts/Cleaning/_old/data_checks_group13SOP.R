###Group13: Data quality control checks (2026 dataset and SOP)

# Contents
# 0. Setup: load packages, import the SOP and identify the data files
# 1. Read every file once and check its structure (dimensions, field names)
# 2. SOP checks: missing values, data type and min/max value for each field
# 3. Remark checks: allowed values, ID and symbol formats, uniqueness, allele symbols
# 4. Structural and consistency checks: whitespace, duplicates, ID-to-name mappings
# 5. Reference files: parameter, procedure, disease and query gene files
# 6. Links between the analysis data and the reference files
# 7. Inspect flagged values to justify the cleaning decisions


### 0. Setup

# I used this package to make the files load faster
library(data.table)

# If you're working on the HPC, run this command :)
# setwd("/scratch/grp/msc_appbio/DCDM_Group13/")

# Import the SOP to examine the expected fields and validation rules
sop <- read.csv("_DATA/IMPC_SOP_2026.csv")

# Verify the SOP's dimensions and data types
dim(sop)
str(sop)

## QC result: SOP defines 10 fields (9 named fields + the lacz_* wildcard),
## including data type, min/max value, example value and remarks

# Identify all CSV files in the Group13 data set
files <- list.files(
  path = "Data/group13_data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Verify the number of files identified
length(files)

## QC result: 26,819 CSV files identified

# Import the first file to inspect its structure
test_data <- read.csv(files[1], header = FALSE)
str(test_data)

## QC result: each file is a 2-column key-value table (field name, value),
## with 20 rows: 9 core fields and 11 lacz tissue fields



### 1. Read every file once (raw) and check its structure
raw_list <- lapply(files, fread, header=FALSE)

## QC result: fread warnings show 2 files with a malformed line: "lacz_colon" with
## no value, and "lacz_skeletal_muscle""no ^expression" with no comma between
## field and value. fread stops reading these files at that line, so their
## remaining fields are missing (counted by the structural checks below).
## 3 files contain improperly quoted fields, which fread resolved

# SOP fields that are named differently in the data
setdiff(sop$dataField, test_data$V1)
 
## QC result: the data names two SOP fields differently: parameter_stable_id
## (SOP: parameter_id) and p_value (SOP: pvalue); lacz_* is a wildcard for the
## 11 lacz tissue fields

# Expected field names
sop_fields <- sop$dataField
sop_fields[sop_fields == "parameter_id"] <- "parameter_stable_id"
sop_fields[sop_fields == "pvalue"]       <- "p_value"
core_fields     <- sop_fields[sop_fields != "lacz_*"]
lacz_fields     <- grep("^lacz_", test_data$V1, value = TRUE)
expected_fields <- c(core_fields, lacz_fields)
length(expected_fields)

## QC result: 20 fields per file (9 SOP fields + 11 lacz tissue fields)

# Structural checks on the raw files
structure_errors <- sum(sapply(raw_list, function(d)
  nrow(d) != length(expected_fields) || ncol(d) != 2))

# Field names that differ from an expected name only in format (e.g. laczLung)
simplify_name <- function(x) tolower(gsub("_", "", x))
name_format_errors <- sum(sapply(raw_list, function(d)
  any(!d$V1 %in% expected_fields & simplify_name(d$V1) %in% simplify_name(expected_fields))))

# Files whose field names are not exactly the expected set
field_errors <- sum(sapply(raw_list, function(d)
  !setequal(d$V1, expected_fields) || anyDuplicated(d$V1) > 0))

# One row per file, one column per field (field names used exactly as written)
data_list <- lapply(raw_list, function(d) setNames(d$V2, d$V1)[expected_fields])  
data_all  <- as.data.frame(do.call(rbind, data_list))
colnames(data_all) <- expected_fields
data_all$file <- basename(files)

# Verify the combined data
dim(data_all)

## QC result: 26,819 rows (one per file) x 21 columns (20 fields + file name)



### 2. SOP checks: missing values, data type and min/max value
# One rule per field: the lacz_* rule applies to every lacz tissue field
rules <- sop[sop$dataField != "lacz_*", ]
rules <- rbind(rules, sop[rep(which(sop$dataField == "lacz_*"), length(lacz_fields)), ])
rules$dataField <- expected_fields

# SOP corrections (contradictions within the SOP itself)
# limits are corrected to match the SOP's own remarks and allowed values
# TIM edit:: Did you ask Michelle before editing the SOP requirements?

# analysis_id: min/max say 15, but the remark and example value are 12 characters
rules[rules$dataField == "analysis_id", c("minValue", "maxValue")] <- 12
# lacz_*: max is 19, but the allowed value "tissue not available" is 20 characters
rules$maxValue[rules$dataField %in% lacz_fields] <- 20
# strain_name: max is 25, but the allowed value "B6Brd;B6Dnk;B6N-Tyr<c-Brd>" is 26 characters
rules$maxValue[rules$dataField == "strain_name"] <- 26

# Missing values: NA (fread converts the text "NA") or blank
is_missing <- function(x) is.na(x) | trimws(x) == ""

# Table to record the number of errors per field
sop_errors <- data.frame(field = rules$dataField, missing = 0,
                         wrong_type = 0, out_of_range = 0)

#  Check each field against its SOP data type and min/max value 
for (i in seq_len(nrow(rules))) {
  values  <- data_all[[rules$dataField[i]]]
  missing <- is_missing(values)
  
  if (rules$dataType[i] == "Float") {
    # Numeric fields: must convert to a number within [minValue, maxValue]
    num          <- suppressWarnings(as.numeric(values))
    wrong_type   <- !missing & is.na(num)
    out_of_range <- !is.na(num) & (num < rules$minValue[i] | num > rules$maxValue[i])
  } else {
    # String fields: min/max refer to the number of characters
    wrong_type   <- rep(FALSE, length(values))
    len          <- nchar(values)
    out_of_range <- !missing & (len < rules$minValue[i] | len > rules$maxValue[i])
  }
  
  sop_errors$missing[i]      <- sum(missing)
  sop_errors$wrong_type[i]   <- sum(wrong_type)
  sop_errors$out_of_range[i] <- sum(out_of_range)
}

write.csv(
  x= sop_errors,
  "Results/QC/SOP_errors.csv"
)

## QC result:
## Missing values: every field has at least one; 248-294 in gene_symbol,
## mouse_life_stage, parameter_stable_id, p_value, allele_symbol and each lacz
## field; 1-3 in analysis_id, gene_accession_id, strain_name and parameter_name
## Wrong data type: 1 p_value is not numeric
## Out of range: gene_symbol 15, mouse_life_stage 3, p_value 1; lacz fields
## 7-1,150 per tissue. The high lacz counts (heart 1,150, vascular_system 1,150,
## skeletal_muscle 1,142, eye 681, blood 567) are "tissue not available"
## (20 characters) with an injected symbol or trailing space, e.g.
## "tissu&e not available", "tissue not available "
## No out-of-range values in analysis_id, gene_accession_id, strain_name,
## parameter_stable_id, parameter_name or allele_symbol



### 3. Remark checks: allowed values, formats, uniqueness and allele symbols

# Allowed values, read from the SOP remarks. Lists are split on commas; only a
# list with no commas (mouse_life_stage) is split on semicolons, because some
# strain names contain semicolons (e.g. "C57BL/6JTyr;C57BL/6N")

allowed_values <- function(field) {
  remark <- sop$remarks[sop$dataField == field]
  values <- sub("\\.$", "", trimws(sub(".*Values are:? ", "", remark)))
  sep    <- if (grepl(",", values)) "," else ";"
  trimws(strsplit(values, sep)[[1]])
}

# TRUE if every bracket ( ) and < > is properly opened and closed
brackets_closed <- function(x) {
  repeat {
    y <- gsub("\\([^()<>]*\\)|<[^()<>]*>", "", x)
    if (identical(y, x)) break
    x <- y
  }
  !grepl("[()<>]", x)
}

# Values to check (missing values are excluded; already counted in sop_errors)
strain <- data_all$strain_name
stage  <- data_all$mouse_life_stage
id     <- data_all$analysis_id
mgi    <- data_all$gene_accession_id
symbol <- data_all$gene_symbol
param  <- data_all$parameter_stable_id
allele <- data_all$allele_symbol
lacz   <- unlist(data_all[lacz_fields])

# Table of remark-based errors
remark_errors <- data.frame(
  field = c("strain_name", "mouse_life_stage", "lacz_* (all tissues)",
            "analysis_id", "analysis_id", "gene_accession_id",
            "gene_symbol", "parameter_stable_id", "allele_symbol", "allele_symbol"),
  check = c("Allowed values", "Allowed values", "Allowed values",
            "Alphanumeric", "Unique", "MGI format",
            "Title format, alphanumeric", "Alphanumeric (with underscores)",
            "Starts with gene_symbol", "Brackets properly closed"),
  errors = c(
    sum(!is_missing(strain) & !strain %in% allowed_values("strain_name")),
    sum(!is_missing(stage)  & !stage  %in% allowed_values("mouse_life_stage")),
    sum(!is_missing(lacz)   & !lacz   %in% allowed_values("lacz_*")),
    sum(!is_missing(id)     & !grepl("^[A-Za-z0-9]+$", id)),
    sum(duplicated(id[!is_missing(id)])),
    sum(!is_missing(mgi)    & !grepl("^MGI:[0-9]+$", mgi)), # "MGI:" followed by digits
    sum(!is_missing(symbol) & !grepl("^[A-Z0-9][A-Za-z0-9]*$", symbol)),
    sum(!is_missing(param)  & !grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", param)),
    sum(!is_missing(allele) & !is_missing(symbol) & !startsWith(allele, paste0(symbol, "<"))),
    sum(!is_missing(allele) & !brackets_closed(allele))
  )
)
write.csv(
  x= remark_errors,
  "Results/QC/SOP_remark_errors.csv"
)

## QC result:
## Allowed values: all strain_name values are valid; 3,163 mouse_life_stage and
## 36,535 lacz values (across all 11 tissues) are not in the SOP lists
## analysis_id: all alphanumeric, but 60 duplicates
## gene_accession_id: all in MGI format
## gene_symbol: 1,826 not title format / alphanumeric
## parameter_stable_id: 5,242 not alphanumeric with underscores
## allele_symbol: 3,168 do not start with the gene symbol; 832 have unclosed brackets
## (see section 7 for examples of each type of error)



### 4. Structural and consistency checks
consistency_errors <- data.frame(
  check = c("Wrong dimensions (not 20 x 2)", "Field-name format (e.g. laczLung)",
            "Missing/extra/duplicated fields", "Leading/trailing whitespace",
            "Duplicate records", "gene_accession_id -> >1 gene_symbol",
            "parameter_stable_id -> >1 parameter_name"),
  errors = c(
    structure_errors, name_format_errors, field_errors,
    sum(sapply(data_all[expected_fields], function(x) sum(x != trimws(x), na.rm = TRUE))),
    sum(duplicated(data_all[setdiff(expected_fields, "analysis_id")])),
    sum(tapply(symbol, mgi, function(x) length(unique(x))) > 1),
    sum(tapply(data_all$parameter_name, param, function(x) length(unique(x))) > 1)
  )
)
write.csv(
  x= consistency_errors,
  "Results/QC/SOP_structural_checks.csv"
)

## QC result:
## 3 files do not have 20 x 2 rows/columns, including the 2 files fread stopped
## reading early at a malformed line
## 6 files have missing, extra or duplicated fields: 2 are badly formatted names
## (e.g. laczLung), so 4 have genuinely missing, extra or broken fields
## 4,261 values have leading/trailing whitespace
## 30 duplicate records (identical in every field except analysis_id)
## 1,253 gene_accession_ids map to more than one gene_symbol
## Each parameter_stable_id maps to a single parameter_name



### 5. Reference files: parameters, procedures, diseases and query genes

# Parameter and procedure files are comma-separated with a space after each
# comma, so strip.white = TRUE removes the leading spaces from every value
parameters <- read.csv("_DATA/IMPC_parameter_description.txt", strip.white = TRUE)
procedures <- read.csv("_DATA/IMPC_procedure.txt", strip.white = TRUE)

# Disease file: an MGI report with comment lines starting with "#", then
# 8 tab-separated columns and no header row
dis_lines <- readLines("_DATA/Disease_information.txt")
sum(startsWith(dis_lines, "#"))
table(count.fields("_DATA/Disease_information.txt", sep = "\t", quote = "",
                   comment.char = "#", blank.lines.skip = TRUE))
diseases <- read.delim("_DATA/Disease_information.txt", header = FALSE, quote = "",
                       comment.char = "#", colClasses = "character")
names(diseases) <- c("human_symbol", "human_name", "hgnc_id", "do_disease_name",
                     "do_disease_id", "mouse_genotype_ids", "mouse_symbol", "mgi_id")

# Query genes: one gene symbol per line
query_genes <- readLines("_DATA/group13_query_genes.csv", warn = FALSE)

# Verify the structure of each file
str(parameters)
str(procedures)
str(diseases)
query_genes

# Missing values per column
colSums(sapply(parameters, is_missing))
colSums(sapply(procedures, is_missing))
colSums(sapply(diseases, is_missing))

# Table of reference-file errors
reference_errors <- data.frame(
  file = c(rep("parameter_description", 5), rep("procedure", 3),
           rep("disease_information", 7), rep("query_genes", 2)),
  check = c(
    # parameter_description
    "Duplicate rows",
    "Duplicate impcParameterOrigId",
    "parameterId wrong format",
    "parameterId -> >1 name",           # same parameterId appears with different names
    "name longer than 200 characters (SOP)",
    # procedure
    "Duplicate impcParameterOrigId",
    "impcParameterOrigId not in parameter file",
    "Procedure -> >1 description",
    # disease_information
    "Duplicate rows",
    "HGNC ID wrong format",
    "DOID wrong format",
    "Mouse MGI ID wrong format",
    "Mouse genotype IDs wrong format",
    "DOID -> >1 disease name",
    "Mouse MGI ID -> >1 mouse symbol",
    # query_genes
    "Not found in analysis data",
    "Gene IDs not found in disease data"
  ),
  errors = c(
    sum(duplicated(parameters)),
    sum(duplicated(parameters$impcParameterOrigId)),
    sum(!grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", parameters$parameterId)),
    sum(tapply(parameters$name, parameters$parameterId,
               function(x) length(unique(x))) > 1),
    sum(nchar(parameters$name) > 200),
    
    sum(duplicated(procedures$impcParameterOrigId)),
    sum(!procedures$impcParameterOrigId %in% parameters$impcParameterOrigId),
    sum(tapply(procedures$description, procedures$name,
               function(x) length(unique(x[!is_missing(x)]))) > 1),
    
    sum(duplicated(diseases)),
    sum(!is_missing(diseases$hgnc_id) & !grepl("^HGNC:[0-9]+$", diseases$hgnc_id)),
    sum(!is_missing(diseases$do_disease_id) & !grepl("^DOID:[0-9]+$", diseases$do_disease_id)),
    sum(!is_missing(diseases$mgi_id) & !grepl("^MGI:[0-9]+$", diseases$mgi_id)),
    sum(!is_missing(diseases$mouse_genotype_ids) &
          !grepl("^MGI:[0-9]+(\\|MGI:[0-9]+)*$", diseases$mouse_genotype_ids)),
    sum(tapply(diseases$do_disease_name, diseases$do_disease_id,
               function(x) length(unique(x))) > 1),
    sum(tapply(diseases$mouse_symbol, diseases$mgi_id,
               function(x) length(unique(x))) > 1),
    
    sum(!query_genes %in% symbol),
    sum(!unique(mgi[symbol %in% query_genes]) %in% diseases$mgi_id)
  )
)
write.csv(
  x= reference_errors,
  "Results/QC/Reference_file_checks.csv"
)

## QC result:
## Parameter file: 2,691 missing descriptions; 14 duplicate rows and 14 duplicate
## impcParameterOrigIds (the counts match, so each repeated ID is an exact copy
## of a row); 25 parameterIds appear with more than one name
## Procedure file: 3,481 missing descriptions; 1 procedure has more than one description
## Disease file: 18 comment lines; all 13,912 data lines have 8 columns;
## 6,830 rows have no mouse genotype IDs (no mouse model recorded for that disease)
## Query genes: all 5 found in the analysis data; 1 (Mmp15, MGI:109320) has no disease info



### 6. Links between the analysis data and the reference files

# Distinct parameter IDs in the analysis data, split by whether they are in the parameter file
param_ids   <- unique(param[!is_missing(param)])
missing_ids <- param_ids[!param_ids %in% parameters$parameterId]
found       <- !is_missing(param) & param %in% parameters$parameterId

link_errors <- data.frame(
  check = c("parameter_stable_id not in parameter file",
            "  of which contain injected symbols (fail format check)",
            "  of which are well-formed (genuinely absent from the file)",
            "parameter_stable_id in file but with a different parameter_name",
            "Genes with disease information (not an error)"),
  count = c(
    length(missing_ids),
    sum(!grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", missing_ids)),
    sum(grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", missing_ids)),
    sum(!unique(paste(param[found], data_all$parameter_name[found])) %in%
          paste(parameters$parameterId, parameters$name)),
    sum(unique(mgi[!is_missing(mgi)]) %in% diseases$mgi_id)
  )
)
write.csv(
  x= link_errors,
  "Results/QC/link_problems.csv"
)

## QC result:
## 5,195 distinct parameter_stable_ids are not in the parameter file:
## 4,143 contain injected symbols and 1,052 are well-formed IDs absent from the
## file (mostly non-IMPC pipelines, e.g. MGP_, JAX_, UCD_)
## Every parameter_stable_id that is in the file has the matching parameter_name
## 919 distinct genes in the analysis data have disease information



### 7. Inspect flagged values (justifies the cleaning decisions)

# analysis_id lengths (confirms the SOP's 15 should be 12)
table(nchar(id))

## QC result: all 26,816 non-missing analysis_ids are 12 characters

# Raw field names that are not expected field names
raw_names <- table(unlist(lapply(raw_list, `[[`, "V1")))
raw_names[!names(raw_names) %in% expected_fields]

## QC result: 1 file has every field name wrapped in quote marks (e.g. "allele_symbol");
## other unexpected names are a blank name, laczLung and strainname (missing underscore)

# Values not in the allowed lists
table(strain[!is_missing(strain) & !strain %in% allowed_values("strain_name")])
table(stage[!is_missing(stage) & !stage %in% allowed_values("mouse_life_stage")])
head(sort(table(lacz[!is_missing(lacz) & !lacz %in% allowed_values("lacz_*")]),
          decreasing = TRUE), 30)

## QC result: all strain names are valid
## mouse_life_stage errors: injected symbols (e.g. "E#arly adult", "E12!.5"),
## wrong case (e.g. "EARLY ADULT", "early adult", "e12.5") and trailing spaces
## (e.g. "Early adult ", "E12.5 ")
## lacz errors: trailing spaces (e.g. "no expression ", 1,098), wrong case
## (e.g. "NO EXPRESSION", "no data") and injected symbols (e.g. "no exp&ression")

# lacz values outside the 7-20 character limit (mostly "tissue not available"
# with an injected symbol or trailing space)
lacz_bad <- lacz[!is_missing(lacz) & (nchar(lacz) < 7 | nchar(lacz) > 20)]
head(sort(table(lacz_bad), decreasing = TRUE), 10)

## QC result: almost all are "tissue not available" with a trailing space (226)
## or an injected symbol (e.g. "ti#ssue not available")

# Badly formatted gene symbols and parameter IDs
head(unique(symbol[!is_missing(symbol) & !grepl("^[A-Z0-9][A-Za-z0-9]*$", symbol)]), 20)
head(unique(param[!is_missing(param) & !grepl("^[A-Za-z0-9]+(_[A-Za-z0-9]+)+$", param)]), 20)

## QC result: gene symbols contain injected symbols (e.g. "Ap4$e1", "@Selenow")
## or a trailing space (e.g. "Ml%x ", "Paox ")
## parameter IDs contain injected symbols (e.g. "IMP%C_&XRY_001_001",
## "-IMPC_PAT_003_002"); some non-IMPC IDs use hyphens (e.g. "M-G-P_008_001_022")

# Allele symbols that do not start with the gene symbol or have unclosed brackets
head(unique(data_all[!is_missing(allele) & !is_missing(symbol) &
                       !startsWith(allele, paste0(symbol, "<")),
                     c("gene_symbol", "allele_symbol")]), 20)
head(unique(allele[!is_missing(allele) & !brackets_closed(allele)]), 20)

## QC result: gene/allele mismatches are caused by an injected symbol in either
## the gene symbol (e.g. "Ap4$e1" / "Ap4e1<tm1b(KOMP)Wtsi>") or the allele
## (e.g. "Dcdc2b" / "Dcdc*2b<tm1b(KOMP)Wtsi>")
## unclosed brackets come from an injected bracket (e.g. "Mgat4d<tm1b(KOMP()Wtsi>")

# Rows sharing an analysis_id
dup_ids <- id[duplicated(id) & !is_missing(id)]
head(data_all[id %in% dup_ids, ][order(id[id %in% dup_ids]), 1:6], 10)

## QC result: duplicate analysis_ids are copies of the same analysis, some
## corrupted differently (e.g. "Early adult" / "Early adu&lt")

# Out-of-range p-values
p <- suppressWarnings(as.numeric(data_all$p_value))
summary(p[!is.na(p) & (p < 0 | p > 1)])

## QC result: 1 p-value is out of range (1.89)

# Query genes and their accession IDs, and which have no disease information
query_ids <- unique(data_all[symbol %in% query_genes, c("gene_symbol", "gene_accession_id")])
query_ids
query_ids[!query_ids$gene_accession_id %in% diseases$mgi_id, ]

## QC result: all 5 query genes are in the analysis data, each with a single
## accession ID; Mmp15 (MGI:109320) has no disease information
