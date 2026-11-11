getwd()
# Group13: Data quality control checks

# Import the SOP to examine the expected fields and validation rules
sop <- read.csv("Data/Group13/IMPC_SOP.csv")

# Verify the SOP's dimensions and data types
dim(sop)
str(sop)

# Identify all CSV files in the Group13 data set
files <- list.files(
  path = "Data/Group13/data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Verify the number of files identified
length(files)

## QC result: 30,277 CSV files

# Import the first file to inspect its structure
test_data <- read.csv(files[1], header = FALSE)
str(test_data)


### 1. Read every file once (raw)

raw_list <- lapply(files, read.csv, header = FALSE)

# Structural checks on the raw files
structure_errors <- sum(sapply(raw_list, function(d) nrow(d) != 8 || ncol(d) != 2))
case_errors <- sum(sapply(raw_list, function(d)
  !all(sop$dataField %in% d$V1) && all(sop$dataField %in% tolower(trimws(d$V1)))))
field_errors <- sum(sapply(raw_list, function(d) {
  f <- tolower(trimws(d$V1))
  !setequal(f, sop$dataField) || anyDuplicated(f) > 0
}))

# One row per file, one column per SOP field
data_list <- lapply(raw_list, function(data) {
  data$V1 <- tolower(trimws(data$V1))          # standardise field-name capitalisation
  setNames(data$V2, data$V1)[sop$dataField]    # order values by SOP field
})
data_all <- as.data.frame(do.call(rbind, data_list))
data_all$file <- basename(files)

# Verify the combined data
dim(data_all)

## QC result: 30,277 rows 1 per file) and 9 columns (8 SOP fields and file name)


### 2. SOP checks: missing values, data type and min/max value

# Table to record the number of errors per field
sop_errors <- data.frame(field = sop$dataField, missing = 0,
                         wrong_type = 0, out_of_range = 0)

# Check each field against its SOP data type and min/max value
for (i in seq_len(nrow(sop))) {
  values  <- data_all[[sop$dataField[i]]]
  missing <- is.na(values) | trimws(values) == ""
  
  if (sop$dataType[i] == "Float") {
    # Numeric fields: must convert to a number within [minValue, maxValue]
    num          <- suppressWarnings(as.numeric(values))
    wrong_type   <- !missing & is.na(num)
    out_of_range <- !is.na(num) & (num < sop$minValue[i] | num > sop$maxValue[i])
  } else {
    # String fields: min/max refer to the number of characters
    wrong_type   <- rep(FALSE, length(values))
    len          <- nchar(values)
    out_of_range <- !missing & (len < sop$minValue[i] | len > sop$maxValue[i])
  }
  
  sop_errors$missing[i]      <- sum(missing)
  sop_errors$wrong_type[i]   <- sum(wrong_type)
  sop_errors$out_of_range[i] <- sum(out_of_range)
}
sop_errors

## QC result: mouse_life_stage has 206 missing values and 3 out of range values (4-17 characters)
## QC result: pvalue has 312 values out of range (0-1)


### 3. Remark checks: allowed values, formats, uniqueness and title case

# Allowed values, read straight from the SOP remarks ("Values are A; B; C")
allowed_values <- function(field) {
  remark <- sop$remarks[sop$dataField == field]
  trimws(strsplit(sub(".*Values are ", "", remark), "; ")[[1]])
}

# Values to check (NA = missing, already counted in sop_errors)
strain <- data_all$mouse_strain
stage  <- data_all$mouse_life_stage
id     <- data_all$analysis_id
mgi    <- data_all$gene_accession_id
symbol <- data_all$gene_symbol

# Table of remark-based errors
remark_errors <- data.frame(
  field = c("mouse_strain", "mouse_life_stage", "analysis_id",
            "analysis_id", "gene_accession_id", "gene_symbol"),
  check = c("Allowed values", "Allowed values", "Alphanumeric",
            "Unique", "MGI format", "Title format"),
  errors = c(
    sum(!is.na(strain) & !strain %in% allowed_values("mouse_strain")),
    sum(!is.na(stage)  & !stage  %in% allowed_values("mouse_life_stage")),
    sum(!is.na(id)     & !grepl("^[A-Za-z0-9]+$", id)),
    sum(duplicated(id[!is.na(id)])),
    sum(!is.na(mgi)    & !grepl("^MGI:[0-9]+$", mgi)),
    sum(!is.na(symbol) & substr(symbol, 1, 1) != toupper(substr(symbol, 1, 1)))
  )
)
remark_errors

## QC result: mouse_strain has 256 values that aren't in the specified list
## QC result: mouse_life_stage has 291 values that aren't in the specified list
## QC result: gene_accession_id has 280 values in lower case


### 4. Structural and consistency checks

consistency_errors <- data.frame(
  check = c("Wrong dimensions (not 8 x 2)", "Field-name capitalisation",
            "Missing/extra/duplicated fields", "Leading/trailing whitespace",
            "Duplicate records", "gene_accession_id -> >1 gene_symbol",
            "parameter_id -> >1 parameter_name"),
  errors = c(
    structure_errors, case_errors, field_errors,
    sum(sapply(data_all[sop$dataField], function(x) sum(x != trimws(x), na.rm = TRUE))),
    sum(duplicated(data_all[setdiff(sop$dataField, "analysis_id")])),
    sum(tapply(data_all$gene_symbol, data_all$gene_accession_id,
               function(x) length(unique(x))) > 1),
    sum(tapply(data_all$parameter_name, data_all$parameter_id,
               function(x) length(unique(x))) > 1)
  )
)
consistency_errors

## QC result: 296 files have field name capitalisation errors
## QC result: gene_accession_id has 92 values that map to more than one gene symbol


### 5. Reference files: parameters, procedures, diseases and query genes

# Parameter and procedure files are comma-separated with a space after each
# comma, so strip.white = TRUE removes the leading spaces from every value
parameters <- read.csv("Data/Group13/IMPC_parameter_description.txt",
                       strip.white = TRUE, na.strings = c("", "NA"))
procedures <- read.csv("Data/Group13/IMPC_procedure.txt",
                       strip.white = TRUE, na.strings = c("", "NA"))

# Disease file is tab-separated; quote = "" keeps quote marks inside names
diseases <- read.delim("Data/Group13/Disease_information.txt", quote = "",
                       na.strings = c("", "NA"), check.names = FALSE)
names(diseases) <- c("do_disease_id", "do_disease_name", "omim_ids", "mgi_id")

# Query genes: one gene symbol per line, no header
query_genes <- trimws(readLines("Data/Group13/query_genes.csv"))
query_genes <- query_genes[query_genes != ""]

# Verify the structure of each file
str(parameters)
str(procedures)
str(diseases)
query_genes

# Missing values per column
colSums(is.na(parameters))
colSums(is.na(procedures))
colSums(is.na(diseases))

## QC result: 2691 missing parameter descriptions and 3481 missing procedures

# Table of reference-file errors
reference_errors <- data.frame(
  file = c(rep("parameter_description", 5), rep("procedure", 3),
           rep("disease_information", 4), rep("query_genes", 2)),
  check = c(
    # parameter_description
    "Duplicate rows",
    "Duplicate impcParameterOrigId",
    "parameterId wrong format",
    "parameterId -> >1 name",
    "name longer than 74 characters (SOP)",
    # procedure
    "Duplicate impcParameterOrigId",
    "impcParameterOrigId not in parameter file",
    "Procedure -> >1 description",
    # disease_information
    "Duplicate rows",
    "DOID / MGI / OMIM wrong format",
    "Disease names wrapped in quotes",
    "DOID -> >1 disease name",
    # query_genes
    "Not found in analysis data",
    "Not found in disease data"
  ),
  errors = c(
    sum(duplicated(parameters)),
    sum(duplicated(parameters$impcParameterOrigId)),
    sum(!grepl("^[A-Za-z0-9-]+_[A-Za-z]+_[0-9]+_[0-9]+$", parameters$parameterId)),
    sum(tapply(parameters$name, parameters$parameterId,
               function(x) length(unique(x))) > 1),
    sum(nchar(parameters$name) > 74),
    
    sum(duplicated(procedures$impcParameterOrigId)),
    sum(!procedures$impcParameterOrigId %in% parameters$impcParameterOrigId),
    sum(tapply(procedures$description, procedures$name,
               function(x) length(unique(na.omit(x)))) > 1),
    
    sum(duplicated(diseases)),
    sum(!grepl("^DOID:[0-9]+$", diseases$do_disease_id) |
          !grepl("^MGI:[0-9]+$", diseases$mgi_id) |
          !grepl("^OMIM:[0-9]+(\\|OMIM:[0-9]+)*$", diseases$omim_ids)),
    sum(grepl("\"", diseases$do_disease_name)),
    sum(tapply(diseases$do_disease_name, diseases$do_disease_id,
               function(x) length(unique(x))) > 1),
    
    sum(!query_genes %in% data_all$gene_symbol),
    sum(!unique(data_all$gene_accession_id[data_all$gene_symbol %in% query_genes])
        %in% diseases$mgi_id)
  )
)
reference_errors

## QC result: parameter_description has 14 duplicate rows, 25 parameter IDs with more than 1 name
## QC result: parameter_description also has 4 names over 74 characters and parameter ID isn't unique
## QC result: one procedure has more han one description
## QC result: 20 disease names are in quotations
## QC result: 8 cases of query genes not being found in disease data because each gene appears under 2 ID spellings (MGI and mgi)


### 6. Links between the analysis data and the reference files

link_errors <- data.frame(
  check = c("Analysis parameter_id not in parameter file",
            "Analysis parameter_name differs from parameter file",
            "Analysis genes with disease information (not an error)"),
  count = c(
    sum(!unique(data_all$parameter_id) %in% parameters$parameterId),
    sum(!unique(paste(data_all$parameter_id, tolower(data_all$parameter_name))) %in%
          paste(parameters$parameterId, tolower(parameters$name))),
    sum(unique(data_all$gene_accession_id) %in% diseases$mgi_id)
  )
)
link_errors

## QC result: 252 parameter_id in analysis data not found in parameter file
## QC result: only 10 genes are linked to a disease


### 7. Inspect flagged values for cleaning
# Strain and life-stage values that aren't allowed
table(strain[!is.na(strain) & !strain %in% allowed_values("mouse_strain")])
table(stage[!is.na(stage) & !stage %in% allowed_values("mouse_life_stage")])

# Badly formatted MGI IDs
head(unique(mgi[!grepl("^MGI:[0-9]+$", mgi)]), 20)

# Parameter IDs missing from the parameter file
head(unique(data_all$parameter_id[!data_all$parameter_id %in% parameters$parameterId]), 20)

# Out-of-range p-values
p <- as.numeric(data_all$pvalue)
summary(p[p < 0 | p > 1])

# Query genes and their accession IDs
unique(data_all[data_all$gene_symbol %in% query_genes, c("gene_symbol", "gene_accession_id")])

# Example of a gene ID with more than one symbol
n_sym <- tapply(symbol, mgi, function(x) length(unique(x)))
head(unique(data_all[mgi %in% names(n_sym)[n_sym > 1], c("gene_accession_id", "gene_symbol")]), 10)