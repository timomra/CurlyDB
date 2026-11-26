# Import the Functional Diversity Package in R.
# https://cran.r-project.org/web/packages/FD/refman/FD.html
# Specifically we're using the Gower dissimilarity (distance) function.
# https://search.r-project.org/CRAN/refmans/FD/html/gowdis.html
library(FD)
library(dplyr)
library(tsne)
library(umap)
library(tibble)
library(ggplot2)
library(pheatmap)
library(igraph)


# Import the Data
# ** Replace this with the SQL query
analysis_Data <- read.csv("Results/Cleaned_Data/Cleaned_Data.csv")



# Define a function to avoid repeating code.
rank_order_encodings <- function(col, orderings){
  order <- factor(col,orderings, ordered = TRUE)
  ranks  <- ordered(as.integer(order))   # <- explicitly casts these as ordered, so that the Gower distance function can know how to handle them.
  return(ranks)
}

lacz_fields <- grep("^lacz_", colnames(analysis_Data), value = TRUE)


# We'll start by keeping it simple and getting tissue expressions independent of lifestage.
tissue_expression_nolifestage <- analysis_Data[,c("gene_symbol",lacz_fields)] %>%     
  mutate(across(all_of(lacz_fields),function(x){rank_order_encodings(x,c("no expression","expression"))}),)  %>%   
  mutate(across(all_of(lacz_fields),as.character),)


table(analysis_Data$mouse_life_stage)

# DELETE_LATER this sets the value for each gene to be the the most common in
# each column, totally ignoring life stage, which isn't very useful.
tissue_expression_nolifestage <- tissue_expression_nolifestage %>%
  group_by(gene_symbol) %>%
  summarise(across(all_of(lacz_fields),
                   ~ unique(.x)[which.max(tabulate(match(.x, unique(.x))))]),
            .groups = "drop")




# Remove rows with missing gene symbols
tissue_expression_nolifestage <- tissue_expression_nolifestage[!is.na(tissue_expression_nolifestage$gene_symbol),]


tissue_expression_nolifestage <- tissue_expression_nolifestage %>%
  column_to_rownames("gene_symbol")


tissue_expression_nolifestage_gowd <- gowdis(tissue_expression_nolifestage,ord = "podani")




# Currently the challenge is the amount of missing data For this
gow_matrix <- as.matrix(tissue_expression_nolifestage_gowd)

gow_matrix[is.na(gow_matrix)] <- 1

set.seed(67)
selections <- sample(1:nrow(gow_matrix),size=200)

gow_matrix <- gow_matrix[selections,selections]

# Trying out a heatmap - advantage of this is that we can see all the data, including the points
# that had missing values

pheatmap(gow_matrix)


set.seed(67)

gene_expression_tsne <- tsne(gow_matrix,  
                             k=2 ,  # <- we only want the fist two dimensions of the embedding.
                             perplexity = 2
)



gow_d <- as.dist(gow_matrix)



# Hierarchical clustering
hclust_tissue_expression <- hclust(gow_d, method = "average")
plot(hclust_tissue_expression,
     main="Hierarchical clustering of genes"
)

gene_expression_umap <- umap(gow_matrix, input = "dist")
gene_expression_tsne <- tsne(gow_matrix, k = 2)





# Leiden clustering
gow_graph <- graph_from_adjacency_matrix(gow_matrix, mode = "max",weighted = TRUE)
cl <- cluster_leiden(gow_graph,objective_function = "modularity")

# k-means clustering
kmeans <- kmeans(gow_matrix, centers = 4)

 


df <- data.frame(gene_expression_umap$layout)
df$cluster <- NA 

for (i in 1:length(cl)){
  df$cluster[rownames(df) %in% unlist(cl[i])] <- paste0("Leiden_",i)
}


df$kmeans_cluster <- factor(kmeans$cluster[rownames(df)])


ggplot(data.frame(gene_expression_tsne), aes(x=X1,y=X2))+
  geom_point()+
  labs(title = "tSNE")


ggplot(df, aes(x=X1,y=X2,color=kmeans_cluster))+
  geom_point()+
  labs(title = "UMAP")

ggplot(df, aes(x=X1,y=X2,color=cluster))+
  geom_point()+
  labs(title = "UMAP")







