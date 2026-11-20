# Set working dir for HPC.
# setwd("/users/k26067916/DCDM_project/")

# First we call the QC script to re-use some of their useful variables and avoid duplicating code
source("Scripts/Cleaning/Initial_SOP_QualityChecks.R")
# Then I call the SOP cleaning script, primarily to get it's data_clean variable
source("Scripts/Cleaning/SOP_Cleaning.R")

source("Scripts/Cleaning/Norvig.R") # <- sourcing a spell check function from another script. 
library(dplyr)
library(stringr)


# I first made a copy of the SOP_cleaning dataframe
cleaned_spellings <- data_clean


# ================= # 
#   Clean Tissues
# ================= #

# Cleaning steps: 
# 1. remove trailing whitespace
# 2. put all names in lower case. given the SOP wants "No data" to have the first 
# letter upper case, this is technically changing the data, but I'd argue this is 
# defensible as it isn't changing the meaning, but to be safe I changed it back.
# 3. Remove all of the special characters, since none of the allowed names 
# contain any numbers or special characters. 

# A regular expression containing all the special characters I could think of and numbers 
special_pat <- "[\\!@#$%^&*()_|\\-|\\+=|\\[|\\]|\\{|\\}|;:'|\"|,.<>/?~0-9]"

# Now perform basic cleaning (see comments on each line for the three main steps)
# (We're selecting using lacz_fields variable which was defined in the previous function)
cleaned_spellings <- cleaned_spellings %>%     
  mutate(across(all_of(lacz_fields),trimws),) %>%                                          # <- Remove trailing white space
  mutate(across(all_of(lacz_fields),tolower),)%>%                                          # <- Convert all to lowercase 
  mutate(across(all_of(lacz_fields),function(x){str_replace_all(x,special_pat,"")}),)%>%   # <- Remove all special characters
  mutate(across(all_of(lacz_fields),function(x){str_replace_all(x,"no data","No data")}),)%>%# <- change the "no data" back into "No data"
  mutate(across(all_of(lacz_fields),function(x){str_replace_all(x,"imageonly","imageOnly")}),) # <- change the "no data" back into "No data"
  
# Evaluate cleaning
# The QC script defines a function which gives us the defined "allowed values"
allowed_lacz <- allowed_values("lacz_*")
lacz_cleaned   <- unlist(cleaned_spellings[lacz_fields])


# There is now only one value we couldnt fix. It's not clear what this one was meant to be...
table(lacz_cleaned[!(lacz_cleaned %in% allowed_lacz)],useNA = "ifany")


cleaned_spellings$lacz_colon[!(cleaned_spellings$lacz_colon %in% allowed_lacz)] <- NA



# ================= # 
# Clean Life stages
# ================= #
allowed_lifestage <- allowed_values("mouse_life_stage")
allowed_lifestage
# Make a slightly different regular expression without a . and 0-9since this is
# used in some of the allowed names
special_pat_lifestage <- "[\\!@#$%^&*()_|\\-|\\+=|\\[|\\]|\\{|\\}|;:'|\"|,<>/?~]"

cleaned_spellings <- cleaned_spellings %>%
  mutate(across(mouse_life_stage,tolower)) %>%
  mutate(across(mouse_life_stage,trimws))  %>%
  mutate(across(mouse_life_stage,function(x){str_replace_all(x,special_pat_lifestage,"")}),) %>%
  mutate(across(mouse_life_stage,function(x){paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))})) # <- This time all of them need to start with a capital letter so we do this instead

# We cant blanket remove characters like . or numerics this time. so I used Norvig's
# Algorithm for this as some of them will be ~ 1-3 edits away from being allowable names
# WARNING: this part is slow, but since we're only doing the cleaning once it's fine.
uni <- unique(cleaned_spellings$mouse_life_stage)
fixes <-   lapply(uni, norvig ,d=allowed_lifestage)
cleaned_spellings$mouse_life_stage <- fixes[match(cleaned_spellings$mouse_life_stage,uni)]

# Fix the capitalisation at start of first letter again.
cleaned_spellings<-cleaned_spellings %>%
  mutate(across(mouse_life_stage,function(x){paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))}))

table(cleaned_spellings$mouse_life_stage[!(cleaned_spellings$mouse_life_stage%in% allowed_lifestage)])



# ================= # 
# Clean gene symbols 
# ================= #
# I downloaded the official dataset containing all mouse genes to justify my cleaning (found at: https://www.informatics.jax.org/downloads/reports/index.html)
# To save memory I only kept the marker symbols
mgi_genes <- read.delim("Scripts/Cleaning/MRK_List2.rpt")
mgi_genes <- mgi_genes$Marker.Symbol

# The official Guidelines for Nomenclature of Genes, Genetic Markers, Alleles, 
# and Mutations in Mouse and Rat state that: 
# "a gene symbol should include punctuation only in specific special cases"
non_alphanumeric <- "([^A-Za-z0-9])"
gene_specials <-  unique(na.omit(as.vector(str_match(mgi_genes,non_alphanumeric),)))
print(gene_specials)
# "." "-" "(" "/" "_" "<"   <- these types of punctuation are apparently acceptable in gene names.
# Adjust the regular expression accordingly.
special_pat_genename <-  "[\\!@#$%^&*|\\)|\\+=|\\[|\\]|\\{|\\}|;:'|\"|,?~>]"


cleaned_spellings <- cleaned_spellings %>%
  mutate(across(gene_symbol,trimws))  %>%
  # Strip out any special characters that should not be in gene names
  mutate(across(gene_symbol,function(x){str_replace_all(x,special_pat_genename,"")}),) %>%
  
  # The official guidelines also state that all genes names should begin with an 
  # uppercase letter (not a number)
  # the remaining letters should be lowercase EXCEPT in  some special cases 
  # e.g., when the gene is unidentified, however I will assume 
  # this does not apply here since the IMPC explicitly says they are testing 
  # known protein-coding genes.
  # For this reason, every gene is forced to have an uppercase first character.
  mutate(across(gene_symbol,tolower)) %>%
  mutate(across(gene_symbol,function(x){paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))})) %>%
  # While it isn't possible to blanket remove characters like _, because of the 
  # we know they should never be at the start of a gene name, so we can remove them 
  # from there.
  mutate(across(gene_symbol,function(x){str_replace_all(x,"^[_\\-()</.]+","")}),)
  
  
# Technically, we could use the accession IDs from the downloaded dataframe to 
# fix the remainder, but that feels like cheating.
unique(cleaned_spellings$gene_symbol[!(cleaned_spellings$gene_symbol %in% mgi_genes)])





# ================= # 
# Clean Parameter IDs
# ================= #
# It never explicitly says these should not have special characters, and it's
# unclear which can be excluded.
# So I only made everything upper-case since that seems to be the convention.
cleaned_spellings$parameter_stable_id

cleaned_spellings <- cleaned_spellings %>%
  mutate(across(parameter_stable_id,trimws)) %>%
  mutate(across(parameter_stable_id,toupper)) 



# ================= # 
# Clean Allele symbols
# ================= #

# Will need to check the SOP dataframe for what these are meant to look like, but
# it's probably fine again




# ================= # 
# Clean Row duplicates
# ================= #

# do a quick check to make sure the rows are identical and then just drop them.
# if not, drop both as we cant know which is the correct one.







# Finally, save the cleaned dataframe

write.csv(cleaned_spellings, file="Results/cleaned_spellings/Clean_names.csv")

