# Group13: Structural and field-name capitalisation quality control
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

# Import the first file to inspect its structure
test_data <- read.csv(files[1], header = FALSE)

# Verify the structure and data types of the imported file
str(test_data)


# Create vector to record files with structural errors
failed_files <- character(0)

# Check the dimensions of each CSV file
for (file in files) {
  data <- read.csv(file, header = FALSE)
  
  if (nrow(data) != 8 || ncol(data) != 2) {
    failed_files <- c(failed_files, file)
  }
}

# Count the files that failed structural validation
length(failed_files)
### none of the 30,277 CSV files failed the structural dimensions check thus all contain 8 rows and 2 columns


# Import the SOP and identify all CSV files
sop <- read.csv("Data/Group13/IMPC_SOP.csv")
files <- list.files("Data/Group13/data", pattern = "\\.csv$",
                    full.names = TRUE)

# Identify files with inconsistent field-name capitalisation
case_errors <- character(0)

for (file in files) {
  data <- read.csv(file, header = FALSE)
  
  if (!all(sop$dataField %in% data$V1) &&
      all(sop$dataField %in% tolower(data$V1))) {
    case_errors <- c(case_errors, file)
  }
}

# Count affected files
length(case_errors)

### QC result: 296 of 30,277 CSV files contain field-name capitalisation inconsistencies
