# Import the Functional Diversity Package in R.
# https://cran.r-project.org/web/packages/FD/refman/FD.html
# Specifically we're using the Gower dissimilarity (distance) function which 
# implements the method proposed by Podani (1999) for dealing with categorical ordinal variables
# https://search.r-project.org/CRAN/refmans/FD/html/gowdis.html
library(dplyr)
library(tibble)
library(tidyr)

# Distance Packages
library(FD)
library(prabclus)



sig_level = 0.05 
distance = "gower"


# Define some external functions to avoid repeating code.
get_mode <- function(x) {names(which.max(table(x)))}


pheno_distmatrix <- function(in_pheno_data,in_sig_level,distance){
  # First identify which
  in_pheno_data$sig_phenotype <- in_pheno_data$p_value < in_sig_level
  
  # If pooling the data from multiple life stages, or in rare cases for the same life stage (sex and zygosity are not given)
  # there are genes with multiple values for the same parameter.
  # In these situations, we take the most common non-NA result for that gene and that parameter.
  in_pheno_data_long <- in_pheno_data[!is.na(in_pheno_data$sig_phenotype),] %>% 
    group_by(gene_accession_id,parameter_stable_id) %>%
    summarise(
      most_freq_result = get_mode(sig_phenotype)  
    )
  
  # Convert our logical variables into numeric (i.e., TRUE=1, FALSE=0)
  in_pheno_data_long$most_freq_result <- as.numeric(as.logical(in_pheno_data_long$most_freq_result))
  
  
  # Pivot the data wide to put it in a format that the distance calculators can accept.
  # Naturally this is very sparse, which may 
  in_pheno_data_wide <- pivot_wider(
    in_pheno_data_long,
    id_cols = gene_accession_id,
    names_from = parameter_stable_id,
    values_from = most_freq_result)
  
  # Move Gene accession IDs to the row indices
  in_pheno_data_wide <- in_pheno_data_wide %>%
    column_to_rownames("gene_accession_id")
  
  
  
  if (distance=="gower"){
    dist_matrix <- gowdis(in_pheno_data_wide)
    
    
    
  }
    
  # Then pivot the table wide - this will be the standard input for the various distance functions.
  
  return(dist_matrix)
}






















