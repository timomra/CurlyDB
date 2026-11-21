# Set working dir for HPC.
# setwd("/users/k26067916/DCDM_project/")

# First we call the QC script to re-use some of their useful variables and avoid duplicating code
source("Scripts/Cleaning/Initial_SOP_QualityChecks.R")
# Then I call the SOP cleaning script, primarily to get it's data_clean variable
source("Scripts/Cleaning/SOP_Cleaning.R")

source("Scripts/Cleaning/SpellCheck_Function.R") # <- sourcing a spell check function from another script. 

# Save memory by removing some of the un-needed variables
remove(data_all)
remove(raw_list)
remove(diseases)

# Library imports
library(dplyr)


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


cleaned_spellings <- cleaned_spellings %>%
  mutate(across(all_of(lacz_fields),function(x){
                                            x[!(x %in% allowed_lacz)] <- NA
                                            x}))


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

# We cant blanket remove characters like . or numerics this time, so I used my custom 
# spell correct function to fix this.

uni <- unique(cleaned_spellings$mouse_life_stage)
fixes <-   lapply(uni, fix_insertions,allowed_strings=allowed_lifestage)
cleaned_spellings$mouse_life_stage <- fixes[match(cleaned_spellings$mouse_life_stage,uni)]

# Fix the capitalisation at start of first letter again.
cleaned_spellings<-cleaned_spellings %>%
  mutate(across(mouse_life_stage,function(x){paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))}))

table(cleaned_spellings$mouse_life_stage[!(cleaned_spellings$mouse_life_stage%in% allowed_lifestage)])

# Overall looking good.
lifestage_checks <- cbind(data_clean$mouse_life_stage,cleaned_spellings$mouse_life_stage)
lifestage_checks<- lifestage_checks[lifestage_checks[,1] != lifestage_checks[,2],]
# View(lifestage_checks)


# Finally we replace any values not in the list of allowables with NA
cleaned_spellings$mouse_life_stage[!(cleaned_spellings$mouse_life_stage %in% allowed_lifestage)] <- NA


# ================= # 
# Clean gene symbols 
# ================= #
# I downloaded the official dataset containing all mouse genes to justify my cleaning (found at: https://www.informatics.jax.org/downloads/reports/index.html)

# I originally had a much longer, more complicated method of cleaning the gene names 
# Which resembled the previous spell-checks. 
# However since we can use official MGI data from the internet, I simply used this
# dataset to sub-in the correct gene names.

# To save memory I only kept the marker symbols
mgi_genes <- read.delim("Data/MRK_List2.rpt")


# There are only two genes in our dataset which do not appear in the official dataset, 
# and these contain missing values for most fields including gene_name and gene_accession. 
#View(cleaned_spellings[!(cleaned_spellings$gene_accession_id %in% mgi_genes$MGI.Accession.ID),])

# Additionally, when I merged the Marker.Symbol column onto my main dataframe, 96% of the symbols matched perfectly. 
cleaned_spellings <- merge(x=cleaned_spellings, y=mgi_genes[,c("MGI.Accession.ID","Marker.Symbol")], 
                           by.x = "gene_accession_id", by.y = "MGI.Accession.ID",
                           all.x =TRUE # <= specifying left merge
)
sum(cleaned_spellings$Marker.Symbol == cleaned_spellings$gene_symbol,na.rm = TRUE) / nrow(cleaned_spellings) *100

# Most of the small differences come from minor injections of characters and symbols.
diff_mask<- cleaned_spellings$gene_symbol != cleaned_spellings$Marker.Symbol
#View(unique(cleaned_spellings[,c("gene_symbol","Marker.Symbol")][diff_mask,]))


# Indeed the average Levenschtein distance between these differences is 1.5 - they are all very similar.
mean(stringdist(a=cleaned_spellings$gene_symbol[diff_mask], b=cleaned_spellings$Marker.Symbol[diff_mask], method = "lv"),na.rm = TRUE)

# Because of this, I simply used this column as the gene names instead
cleaned_spellings <- cleaned_spellings[,!(names(cleaned_spellings) %in% c("gene_symbol"))]
# cleaned_spellings <- rename(cleaned_spellings, gene_symbol= Marker.Symbol)


# save memory by removing MGI genes
remove(mgi_genes)


# ================= # 
# Clean Allele symbols
# ================= #

# I used a similar approach, using an external dataset to clean these names, 
# (the dataset: https://www.informatics.jax.org/downloads/reports/MGI_PhenotypicAllele.rpt)
# however this time since genes can have many alleles it wasnt as simple.

allele_data <- read.delim("Data/MGI_PhenotypicAllele.tsv",sep = "\t", header = FALSE)
colnames(allele_data) <- c("MGI Allele Accession ID", "Allele Symbol","Allele Name","Allele Type","Allele Attribute","PubMed ID for original reference","MGI Marker Accession ID","Marker Symbol","Marker RefSeq ID","Marker Ensembl ID","High-level Mammalian Phenotype ID (comma-delimited)","Synonyms (|-delimited)","Marker Name")

# This time only 77% of the alleles in my data were present in the dataset. 
# For this reason I was much more stringent with my method of automatically cleaning.
sum(cleaned_spellings$allele_symbol %in% allele_data$`Allele Symbol`)/nrow(cleaned_spellings)*100


# As MGI specifically states that only roman-letters or arabic-numerals are allowed 
# in allele and gene names and that other punctiuations are only allowed in 
# explicit and specific circumstances, I removed any punctuation characters 
# that are not mentioned in their guidelines

allele_special_pat <- "[!@$%^&?]"

cleaned_spellings <- cleaned_spellings %>%
  mutate(across(allele_symbol,trimws)) %>%
  mutate(across(allele_symbol,function(x){str_replace_all(x,allele_special_pat,"")}),)

# The number of alleles present in the external data now jumps up to 85%, but I still want to be careful
sum(cleaned_spellings$allele_symbol %in% allele_data$`Allele Symbol`)/nrow(cleaned_spellings)*100


# I mapped all the missing alleles to their genes, and used this to create a 
# much larger dataframe which maps each allele to all of the possible alleles they could be.
missing_alleles <- cleaned_spellings[!(cleaned_spellings$allele_symbol  %in% allele_data$`Allele Symbol`),c("allele_symbol","gene_accession_id")]
allele_to_genes <- merge(missing_alleles,allele_data[,c("Allele Symbol","MGI Marker Accession ID")],by.x = "gene_accession_id",by.y = "MGI Marker Accession ID")
allele_to_genes <- allele_to_genes[!is.na(allele_to_genes$allele_symbol),]


# Grouping by allele symbol (such as to create a list of candidate allele symbols)
# then summarise these groups using the fix_insertions, however this time the function
# has much more strict arguments put into it.
corrections <- allele_to_genes %>% 
  group_by(allele_symbol) %>%
  summarise(
    corrected_allele_symbol = fix_insertions(
                    incorrect_string = first(allele_symbol),
                    allowed_strings = unique(`Allele Symbol`),
                    max_acceptable_dist=1 # <- setting this parameter as such means that we will accept up to four deletions OR one insertion, transposition or substitution.
                                          # As mentioned, I am being more stringent with this one as I am less certain all correct spellings lies in our external dataset.
          ),
    .groups = "drop",
  )

# Create vec of alleles in the main data, and their matching row of corrections.
correction_rows <- match(cleaned_spellings$allele_symbol,corrections$allele_symbol)
# Only identifying the rows that have a match, we then replace these.
cleaned_spellings$allele_symbol[!is.na(correction_rows)] <- corrections$corrected_allele_symbol[correction_rows[!is.na(correction_rows)]]

# By eye, the corrections look pretty solid, albeit a little conservative. However, 
# as allele symbol isnt essential for the integrity of the database, this is acceptable as long as the symbols are unique.
#View(corrections)

# Now around 96% match to my dataset.
sum(cleaned_spellings$allele_symbol %in% allele_data$`Allele Symbol`)/nrow(cleaned_spellings)*100

# save memory
remove(allele_data)
remove(allele_to_genes)
remove(missing_alleles)


# ================= # 
# Clean Parameter IDs
# ================= #
# Like before, we have a dataset of correct parameter IDs that we will try
# to match to our dataset.

# The complete list of parameter stable IDs used by the IMPC
# Source: https://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-25.0/results/ 
stat_resuls <- read.csv("Data/statistical-results-brief.csv")


# View a table of characters that are used in the IMPC parameter IDs 
allowed_param_chars <- table(unlist(strsplit(stat_resuls$parameter_stable_id,"")))
allowed_param_chars

# This time as I have all the allowed characters in the IMPC identifiers, I can
# simply remove characters that are not in this list instead of removing specific special characters,
names(allowed_param_chars)
allowed_param_chars_pat <- "[^ \\-_0-9A-Za-z]"  # [^ ] specifies a negated class of characters

sum(!(cleaned_spellings$parameter_stable_id %in% parameters$parameterId))/nrow(cleaned_spellings)*100


cleaned_spellings <- cleaned_spellings %>%
  mutate(across(parameter_stable_id,trimws)) %>%
  mutate(across(parameter_stable_id,toupper))%>% # <- this time all IDs are upper-case
  mutate(across(parameter_stable_id,function(x){str_replace_all(x,allowed_param_chars_pat,"")}),)

# Having removed these characters, only around 8% of the parameter IDs did not 
# match an official parameter stable id used by IMPC
# many of these were either NA values, or were simply not found in the dataset.

sum(!(cleaned_spellings$parameter_stable_id %in% stat_resuls$parameter_stable_id))/nrow(cleaned_spellings)*100


# HOWEVER Around 23% of these corrected IDs do not occur in our given dataset.
parameters <- parameters %>%  # cleaned whitespace out of parameters 
  mutate(across(parameterId,trimws))

sum(!(cleaned_spellings$parameter_stable_id %in% parameters$parameterId))/nrow(cleaned_spellings)*100


# This will be addressed in the subsequent script.
# ================= # 
# Spell_Checks_Complete!
# ================= #



