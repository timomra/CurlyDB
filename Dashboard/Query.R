# A script containing functions to query our SQL database

list.files("./")


query_tissues <- function(life_stages){
  
  # Import the Data
  # ** Replace this with the SQL query
  analysis_Data <- read.csv("Results/Cleaned_Data/Cleaned_Data.csv")
  lifestage_subsetData<- analysis_Data[analysis_Data$mouse_life_stage%in%life_stages,]
  
  
  return (lifestage_subsetData)
  }


query_phenotype <- function(life_stages,param_grouping){
  # Import the Data
  # ======================== # 
  # ** Replace this with the SQL query
  # The SQL query must retrieve the gene id and the ENSEMBL ID.
  analysis_Data <- read.csv("../Results/Cleaned_Data/Cleaned_Data.csv")
  life_subset <- analysis_Data[analysis_Data$mouse_life_stage %in% life_stages,]
  
  # load in parameter data and make a fake parameter grouping
  parameter_descriptions <- read.csv("../Results/Cleaned_Data/Parameters_withGroups.csv")
  parameter_descriptions$nogroups <- parameter_descriptions$parameterId
  
  # Merge - remember to check that the merge isnt adding duplicates
  phenotype_data <- merge(life_subset,unique(parameter_descriptions[,c(param_grouping,"parameterId")]), by.x = "parameter_stable_id", by.y = "parameterId",all.x = TRUE)
  # ======================== #
  
  
  return (phenotype_data)
  }
