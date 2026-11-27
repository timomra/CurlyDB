# Calculates dimensionality reduction embeddings from our dataset to visualise
library(tsne)
library(umap)
library(ggplot2)



get_embeddings <- function(in_dist_matrix,random_seed,dimred_method,hyper_param = 30){
  # Set the random seed
  set.seed(random_seed)
  
  # Impute NA values with the maximum distance so that the dimension reduction 
  # algorithms can be run.
  in_dist_matrix[is.na(in_dist_matrix)] <- 1
  
  # Perform UMAP
  if (dimred_method =="umap"){
    # Fortunately, the UMAP function gives us a df with 
    embeddings_df <- umap(as.matrix(in_dist_matrix), input="dist", n_neighbors=hyper_param)$layout
    
    colnames(embeddings_df) <- c("UMAP 1","UMAP 2")
  }
  
  # Perform t-SNE
  if (dimred_method =="tsne"){
    # Fortunately, the UMAP function gives us a df with 
    t <- tsne(
      as.dist(in_dist_matrix),perplexity = hyper_param,
      k=2 # <- not to be confused with k in k-medoids, this simply tells tSNE how many dimensional embeddings we want
    )
    rownames(t) <- rownames(in_dist_matrix)
    embeddings_df <- t
    
    colnames(embeddings_df) <- c("t-SNE 1","t-SNE 2")
  }
  return(embeddings_df)
}





plot_dimred <- function(embeddings_df,include_clusters,cluster_df){
  # Assumes an embeddings df of the form
  
  if (include_clusters){
    embedding_clusters_labelled <- merge(embeddings_df,cluster_df, by.x="row.names",by.y="row.names")
    
    cols <- names(embedding_clusters_labelled)
    p <- ggplot(data=embedding_clusters_labelled, aes(x=.data[[cols[2]]], y=.data[[cols[3]]]))+
      geom_point(aes(fill = .data[[cols[4]]]),
                 shape=21, stroke=0.8, size=3, alpha=0.8,
                 colour = "black")
  } else {
    p <- ggplot(data=embedding_clusters_labelled, aes(x=.data[[cols[2]]], y=.data[[cols[3]]]))+
      geom_point(shape=21, stroke=0.8, size=3, alpha=0.8,
                 colour = "black",fill="black")
    
  }
  
  
  return(p)
}




#e <- get_embeddings(
#  in_dist_matrix= ph_dist_matrix,
#  random_seed = 67,
#  dimred_method = "umap",
#  hyper_param = 5)





























