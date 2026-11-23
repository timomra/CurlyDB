# Import the Functional Diversity Package in R.
# https://cran.r-project.org/web/packages/FD/refman/FD.html
# Specifically we're using the Gower dissimilarity (distance) function.
# https://search.r-project.org/CRAN/refmans/FD/html/gowdis.html
library(FD)
library(dplyr)
library(tibble)
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







