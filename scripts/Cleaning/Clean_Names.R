# First we call the QC script (written by Ella) to use some of it's useful variables
source("Scripts/Cleaning/data_checks_group13SOP.R")
library("dplyr")
library("spellcheckr")  #  <- More information at https://www.rdocumentation.org/packages/spellcheckr/versions/0.1.2
library(stringr)
# Plan:
# Use an implementation of Peter Norvig's spell check algorithm with edited parameters (more info at: https://www.norvig.com/spell-correct.html)




# I'll use these "bad names" as a test dataset
lacz_bad


# The QC script defines a function which gives us the defined "allowed values"
# As Norvig's algorithm takes as input a dictionary of allowable words,
# we can take advantage of this
allowed_lacz <- allowed_values(field = "lacz_*")


set.seed(67)
lacz_bad_samp <- sample(lacz_bad,size=200)



# A pattern which matches most of the special characters I could think of
special_pat <- "[\\!@#$$%^&*()_|\\-|\\+=|\\[|\\]|\\{|\\}|;:'|\"|,.<>/?~]"

lacz_good1_edit <- lacz_bad_samp %>%
  lapply(tolower) %>%
  lapply(str_replace_all,special_pat,"") %>%     # <- replace all special characters to speed up Norvig's algorithm
  lapply(correct,maxedit = 1,dictionary = allowed_lacz) 

lacz_good2edit <- lacz_bad_samp %>%
  lapply(tolower) %>%
  lapply(str_replace_all,special_pat,"") %>%
  lapply(correct,maxedit = 2,dictionary = allowed_lacz) 

lacz_good_nonorvig <- lacz_bad %>% 
  lapply(trimws)  %>%
  lapply(tolower) %>%
  lapply(str_replace_all,special_pat,"")


result2 <- cbind(lacz_bad_samp,lacz_good1_edit,lacz_good2edit,lacz_good_nonorvig)
result3 <- cbind(lacz_bad_samp,lacz_good_nonorvig)



lacz_good_nonorvig[!lacz_good_nonorvig %in% allowed_lacz]



correct("no expression asdasdasd" ,maxedit = 2,dictionary = allowed_lacz)
correct("no expressiokln" ,maxedit = 2, dictionary=allowed_lacz)



data_all[,]



# Select the tissue columns
tissue_cols <- colnames(data_all)[grepl(pattern="lacz_",colnames(data_all))]

# Index the data with this variable.
upper_tissues <- data_all[,tissue_cols] %>%
  sapply(toupper)


upper_tissues 





  
























