




dim(dist_matrix)

library(ggtree)


den <- as.dendrogram(cl)


dat_wide_t <- t(dat_wide)

dat_wide_t*1

typeof(dat_wide_t)

# Jaccard Distance
dat_wide_t[is.na(dat_wide_t)] <- 0

j_matrix <- jaccard(dat_wide_t)

j_matrix2 <- j_matrix
j_matrix2[is.na(j_matrix2)] <- 1


pheno_umap <- umap(j_matrix2, input = "dist")
pheno_tsne <- tsne(j_matrix2, k = 2)






df <- data.frame(pheno_umap$layout)


pheno_data$gene_symbol

df <- merge(df,unique(pheno_data[,c("gene_accession_id","gene_symbol")]),by.x="row.names",by.y="gene_accession_id",all.x=TRUE)



kmeans <- kmeans(j_matrix2, centers = 10)


df$kmeans_cluster <- factor(kmeans$cluster[rownames(df)])

df <- merge(df,data.frame(kmeans$cluster), by.x="Row.names",by.y = "row.names")


rownames(data.frame(kmeans$cluster))

df$kmeans.cluster <- as.factor(df$kmeans.cluster)

ggplot(data=df, aes(x=X1,y=X2,color=kmeans.cluster)) +
  geom_point()


ggplot(data.frame(pheno_tsne), aes(x=X1,y=X2))+
  geom_point()+
  labs(title = "tSNE")

# tissue_expression_nolifestage_gowd <- gowdis(tissue_expression_nolifestage,ord = "podani")







