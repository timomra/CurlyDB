# Call the previous scripts
source("Scripts/Cleaning/SpellCheck_Cleaning.R")
library(ggplot2)



cleaned_analysis_data <- cleaned_spellings



# ================= # 
# Clean Row duplicates
# ================= #
# Do a quick check to make sure the rows are identical and then just drop them.
# if not, drop both as we cant know which is the correct one.




# ================= # 
# Create a dataset of flagged records.
# ================= #
# Now after the spell check cleaning, we locate those files which had the same name to see if they still do.

suffix_indices <- match(suffix_files, cleaned_analysis_data$file)
original_indices <- match(original_filenames, cleaned_analysis_data$file)

identical_pairs <- vapply(seq_along(suffix_indices), function(i) {
  identical(
    unname(as.character(cleaned_analysis_data[suffix_indices[i], analysis_fields])),
    unname(as.character(cleaned_analysis_data[original_indices[i], analysis_fields]))
  )
}, logical(1))


# After spell-corrections, only 9 of the records are non-matching duplicates which mostly differ by one field.
table(identical_pairs)

suffix_files[!identical_pairs]

pair_differences <- lapply(which(!identical_pairs), function(i) {
  
  original <- unlist(cleaned_analysis_data[original_indices[i], analysis_fields],
                     use.names = FALSE)
  
  duplicate <- unlist(cleaned_analysis_data[suffix_indices[i], analysis_fields],
                      use.names = FALSE)
  
  # Compare values, treating two missing values as equivalent
  different <- !(original == duplicate |
                   (is.na(original) & is.na(duplicate)))
  
  different[is.na(different)] <- TRUE
  
  # Return the filenames and names of differing fields
  data.frame(
    original_file = original_filenames[i],
    suffix_file = suffix_files[i],
    differing_fields = paste(analysis_fields[different],
                             collapse = ", ")
  )
})

pair_differences <- do.call(rbind, pair_differences)


# Proposed solution (Tim)
# Drop the 27 duplicates with the same analysis ID and same fields.
# data_clean <- data_clean[-suffix_indices[identical_pairs],]

pair_differences
data.frame(pair_differences[1])[,]

c(data.frame(pair_differences[1])[,3])

for (i in 1:length(pair_differences)){
  print(cleaned_analysis_data[cleaned_analysis_data$file %in% data.frame(pair_differences[i])[,1:2], c(data.frame(pair_differences[i])[,3])])
}



# For the remainder, I will re-evaluate them after running through my 
# spell-correcting script.


# ================= # 
# Flag Significant results 
# ================= #

# As there are many statistical tests being performed, we need to correct for multiple testing.
# From the IMPC website; Generally, the IMPC has adopted a significance level of 10-4 (P < 0.0001), which is the one displayed on the website, unless otherwise indicated. This is the choice made when screens of thousands of genes are conducted, to maintain a false discovery rate (FDR) below 5%. Please note that the significance level for pain sensitivity data, based on 110 genes, is 10-3 (P < 0.001).
# For our analyses, we will consider a gene-knockout to cause the phenotype if it has this significance level too.
cleaned_analysis_data$p_value <- as.numeric(cleaned_analysis_data$p_value)

# We can see that there are quite a lot of non-significant values, but also some
# significant ones
ggplot(cleaned_analysis_data, aes(x=p_value)) +
  geom_density(stat = "count",fill="black") +
  scale_x_log10()+ 
  scale_y_log10()+
  geom_vline(xintercept =  0.0001, color="red",)


# At this level, there are only around 814 significant hits
# As this is the more stringent method we will recommend this to the user, but 
# significance level will be selectable within the interface as is the case with
# other softwares like perseus for proteomics.
table(cleaned_analysis_data$p_value < 0.0001)

# At the normal 5% level, there are around 3590 significant hits
table(cleaned_analysis_data$p_value < 0.05) 



# ================= # 
# Export Cleaned Data from our analysis df
# ================= #
# Finally, save the cleaned dataframe
write.csv(cleaned_analysis_data, file="Results/Cleaned_Data/Cleaned_Data.csv")






# ================= # 
# Introduce Parameter groupings into the parameter dataset (Ella)
# ================= #
# Drop duplicate rows in the parameters df and create a new object to store our 
# groupings
parameters_grouped <- unique(parameters)

head(parameters_grouped)

parameters_grouped$Grouping 


write.csv(parameters_grouped,"Results/Cleaned_Data/Parameters_withGroups.csv")


# ================= # 
# Group procedures by procedure ID
# ================= #
# The procedures and params DFs were defined in the initial quality checks script
# First add parameter_ids onto procedures by joining params on "impcParameterOrigId".
procedures_extrainfo <- merge(procedures,parameters[,c("impcParameterOrigId","parameterId")], by="impcParameterOrigId")

# As we can see, some of these parameter IDs can be found in our stats results DF,
# therefore giving us the original procedure groupings, but not all of them.
table(procedures_extrainfo$parameterId %in% stat_resuls$parameter_stable_id)

# Now we join these procedure_ids onto our procedures dataset using the parameterId.
procedures_extrainfo <- merge(procedures_extrainfo,unique(stat_resuls[,c("parameter_stable_id","procedure_stable_id")]),by.x = "parameterId", by.y = "parameter_stable_id", all.x = TRUE)



# we can group up to 80 procedures together using these IDs which will reduce
# the redundancy of our database quite a lot (this image shows 1 procedure ID can collect up to 80 parameters)
examine_groupings <- procedures_extrainfo %>% 
  group_by(procedure_stable_id)  %>% 
  summarise(
    nrow = n(),
    no_param_ids = n_distinct(parameterId))

# However, some of the parameters are collected using multiple procedures, so 
# this technically is a many-to-many relationship in some cases (though its 
# usually the procedures collecting many parameters and not the other way around).
examine_groupings <- procedures_extrainfo %>% 
  group_by(parameterId)  %>% 
  summarise(
    nrow = n(),
    no_procedures = n_distinct(procedure_stable_id))

# View(examine_groupings)


# As we can see, the dataset has grown (by +18%) because of this join, but using 
# this info will reduce redundancy in our relational database and make the information more 
# accurate as we can now see which parameters belong to exactly which procedures 
nrow(procedures_extrainfo) / nrow(procedures)


# Remove parameter IDs now we dont need them anymore.
procedures_extrainfo <- procedures_extrainfo %>% select(-parameterId)   


head(procedures_extrainfo)


# Adding new key values...
# First replace the NA values with placeholders
miss_mask <- is.na(procedures_extrainfo$procedure_stable_id)
procedures_extrainfo$procedure_stable_id <- replace(procedures_extrainfo$procedure_stable_id, miss_mask,paste0("MISSING_ID_", which(miss_mask)))

# Then use these placeholders to assign unique keys
procedures_extrainfo <- procedures_extrainfo %>%
  group_by(procedure_stable_id) %>%
  mutate(procedure_key = cur_group_id()) %>%    # <- assign a unique numeric identifier to our unique groups
  ungroup()


write.csv(procedures_extrainfo,"Results/Cleaned_Data/Procedures_extra_info.csv")



