# Functions for k-medoids clustering

# We're using the kmed library:
# Budiaji W, Wijaya FB. kmed: An R package for reproducible distance-based 
# k-medoids clustering. Software Impacts. 2026 Mar 24:100833.
library(kmed)



# Calculate the minimum distance of all points to their medoid
calc_mindist <- function(in_dist_matrix, km_clust,k){
  cum_sum = 0
  for (i in 1:k){
    cum_sum <- cum_sum + sum(in_dist_matrix[km_clust$medoid[i],][names(km_clust$cluster[km_clust$cluster==i])])
  }
  return(cum_sum)
}


optimise_k <- function(in_d_matrix,n=round(0.15*nrow(in_d_matrix)),randomseed){
  set.seed(randomseed)
  # k Medoids requires there not be missing values, therefore we impute our
  # missing distances with maximum distances of 1.
  in_d_matrix[is.na(in_d_matrix)] <- 1
  
  
  min_dists <- numeric(n)
  for (k in 1:length(min_dists)){
    kminit<- sample(1:nrow(in_d_matrix),k)
    
    # note: the random starts add noise to the data... but the regression line
    # makes it easier to pick a value for k. 
    kmed_clusters <- fastkmed(as.dist(in_d_matrix),ncluster = k, iterate = 100, init=kminit)
    
    # Minimum_distance is the distance of all objects to their cluster medoid.
    # Plotting these gives a similar indication to sum of squared distances in k-means
    min_dists[k] <- calc_mindist(in_d_matrix, kmed_clusters,k)
  }
  
  p <- ggplot()+
    geom_smooth(aes(x=1:n,y=min_dists),se = FALSE)+
    geom_point(aes(x=1:n,y=min_dists)) +
    labs(
      title = "K-Medoid clustering: Elbow plot",
      x = "k",
      y = "distance of all objects to their cluster medoid"
    )
  return(p)
}




clustering <- function(randomseed,in_d_matrix,cluster_method,k){
  # Note: k is is taken as an argument even if it may not necessarily be used in 
  # some clustering algorithms.
  
  set.seed(randomseed)
  
  if (cluster_method == "kmedoid"){
    # k Medoids requires there not be missing values, therefore we impute our
    # missing distances with maximum distances of 1.
    kminit<- sample(1:nrow(in_d_matrix),k)
    
    in_d_matrix[is.na(in_d_matrix)] <- 1
    kmed_clusters <- fastkmed(as.dist(in_d_matrix),ncluster = k, iterate = 100,init=kminit)
  }
  return(cluster_df)
}



































