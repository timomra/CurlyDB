library(pheatmap)


set.seed(67)


sample(0:1,100, replace = T)


random_matrix <- matrix(runif(n=10, min=1, max=20), nrow=5)

random_matrix





m <- matrix(
  sample(0:1,100, replace = T),
  ncol = 10)



euclidean_dist_rows <- dist(m, method = "binary",na.exclude=TRUE)
bin_dist_rows       <- dist(m, method = "euclidean")


complete_clusters_bin_dist_rows <- hclust(bin_dist_rows, method = "complete")
complete_clusters_euclidean_rows <- hclust(euclidean_dist_rows, method = "complete")



pheatmap(
  m,
  cluster_rows = complete_clusters_euclidean_rows
)


pheatmap(
  m,
  cluster_rows = complete_clusters_bin_dist_rows
)

