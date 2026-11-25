






pheno_data$sig_phenotype <- pheno_data$p_value < sig_level

dat_long <- pheno_data[!is.na(pheno_data$sig_phenotype),] %>% 
  group_by(gene_accession_id,parameter_stable_id) %>%
  summarise(
    most_freq_result = get_mode(sig_phenotype)  
  )

dat_long$most_freq_result <- as.numeric(as.logical(dat_long$most_freq_result))


dat_wide <- pivot_wider(dat_long,
                        id_cols = gene_accession_id,
                        names_from = parameter_stable_id,
                        values_from = most_freq_result
)



dat_wide <- dat_wide %>%
  column_to_rownames("gene_accession_id")



# Euclidean distance matrix
dist_matrix <- dist(dat_wide)

cl <- hclust(dist_matrix)

class(dist_matrix)

is.na(as.matrix(dist_matrix)[1:10,1:10]) 

heatmap(as.matrix(dist_matrix),Rowv = NA, Colv = NA,scale="none")

dim(dist_matrix)









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







