# Given a distance matrix (as a matrix object) this function creates 
# a heatmap with hierarchical clustering to give an overview of the dataset.
library(viridis)
library(pheatmap)


cluster_heatmap <- function(in_dist_matrix, distance_metric){
  # Replace NA values with 1 to allow clustering algorithm.
  in_dist_matrix[is.na(in_dist_matrix)] <- 1

  
    heat <- pheatmap(
      as.dist(in_dist_matrix),
      
      # Due to the sparsity of shared parameters in the dataset, hierarchical clustering
      # can be less informative, as the algorithm needs to sequentially pick the
      # "next closest" cluster However if genes have only a few shared parameters, and 
      # all are negative (i.e., between clusters of genes which all produce 
      # no phenotype and thus have a distance of 0 from eachother), the algorithm 
      # has to arbitrarily decide which to add to the growing tree thus hierarchical 
      # clustering has been turned off.
      cluster_rows=FALSE, cluster_cols=FALSE,
      
      show_rownames  = FALSE,show_colnames  = FALSE,
      color = viridis(5, direction = -1),
      
      # prevents it from being drawn in Rstudio.
      silent = TRUE
    )
  
  return(heat)
}






