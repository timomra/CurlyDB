# Given a distance matrix (as a matrix object) this function creates 
# a heatmap with hierarchical clustering to give an overview of the dataset.
library(viridis)
library(pheatmap)


cluster_heatmap <- function(in_dist_matrix){
  # Replace NA values with 1 to allow clustering algorithm.
  in_dist_matrix[is.na(in_dist_matrix)] <- 1
  
  heat <- pheatmap(
    in_dist_matrix,
    
    # force the function to use our pre-computed distances by giving it a dist object
    clustering_distance_rows = as.dist(in_dist_matrix), 
    clustering_distance_cols = as.dist(in_dist_matrix),
    
    # sets how hierarchical clustering calculates distances between linked rows/cols
    # and other groups/single rows/clusters
    clustering_method = "average",
    
    # our matrix is pre-calculated, we dont need to scale it.
    scale="none",
    
    show_rownames  = FALSE,show_colnames  = FALSE,
    color = viridis(5, direction = -1),
    
    # prevents it from being drawn in Rstudio.
    silent = TRUE
  )
  
  return(heat)
}
