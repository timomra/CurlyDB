# This script calculates a distance matrix depending on the input data.

library(dplyr)
library(tibble)
library(tidyr)
# Distance Packages
# Import the Functional Diversity Package in R. (https://cran.r-project.org/web/packages/FD/refman/FD.html)
# Specifically we're using the Gower dissimilarity (distance) function which 
# implements the method proposed by Podani (1999) for dealing with categorical ordinal variables (https://search.r-project.org/CRAN/refmans/FD/html/gowdis.html)
library(FD)
# A library that gives a fast function for calculating Jaccard Distance
# https://search.r-project.org/CRAN/refmans/prabclus/html/jaccard.html
library(prabclus)


# Define some external functions to avoid repeating code.
get_mode <- function(x) {names(which.max(table(x)))}


pheno_distmatrix <- function(in_pheno_data,in_sig_level,distance){
  # First identify which values are significant
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
    out_dist_matrix <- gowdis(in_pheno_data_wide)
  }
  else if (distance=="jaccard"){
    # The function for jaccard distance expects samples to be columns, so we feed in a transposed matrix.
    transposed_in_pheno_data_wide <- t(in_pheno_data_wide)
    
    # Additionally, as Jaccard only considers elements that are present in both sets (1s not 0s), and since this function 
    # does not allow NA values, we replace all missing values with 0 - hence implicitly assuming that traits which
    # were not measured for these genotypes were not present, which is a flaw, and why we do not recomend using jaccard distance.
    transposed_in_pheno_data_wide[is.na(transposed_in_pheno_data_wide)] <- 0
    
    out_dist_matrix <- jaccard(transposed_in_pheno_data_wide)
    # the output of this is already a matrix type, so no need to convert
  }
  else if (distance=="euclidean"){
    # Uses R's built-in distance function to calculate euclidean distance.
    out_dist_matrix <- dist(in_pheno_data_wide,method = "euclidean")
    
    # Scale the matrix to a 1-0 scale so that it is compatible with my other functions
    out_dist_matrix <- out_dist_matrix/max(out_dist_matrix,na.rm = TRUE) 
  }
  
  # some of the functions outpt a dist object instead of a matrix, so we 
  # standardise this by converting them all into matrices
  return(as.matrix(out_dist_matrix))
  }


# dist_matrix <- pheno_distmatrix(pheno_data,0.05,"gower")


