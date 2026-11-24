

eval_phenogroups <- function(in_pheno_data,param_grouping){
  # Evaluate the phenotype groupings
  
  # Generate some summaries about the data that has been retrieved
  total_records <- nrow(in_pheno_data)
  total_genes <- n_distinct(in_pheno_data$gene_accession_id)
  total_params <- n_distinct(in_pheno_data[[param_grouping]])
  
  # Generate a visualisation to let the user evaluate the distribution of parameters per gene
  params_per_gene <- in_pheno_data %>%
    group_by(gene_accession_id) %>% 
    summarise(      
      params = n_distinct(.data[[param_grouping]])
    )
  
  param_gene_distribution <- ggplot(params_per_gene, aes(x=params)) + 
    geom_histogram(colour = "black",binwidth = 1) + 
    xlim(-1,quantile(params_per_gene$params,.995))+
    
    labs(
      y="counts",
      x="Grouped parameters associated with a gene",
      title="Grouped parameters associated with a gene"
    )
  
  return(c(total_records,total_genes,total_params,param_gene_distribution))
}