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
# Groups parameters by naming similarity (weight, images, brain) and by test
# similarity (procedure code in the parameter ID) for the collaborator

# Drop duplicate rows in the parameters df and create a new object to store our 
# groupings
parameters_grouped <- unique(parameters)

# One row per parameter: the reference file can list a parameterId several
# times (one row per impcParameterOrigId), so keep each parameterId once
parameter_table <- unique(parameters_grouped[, c("parameterId", "name")])
parameter_table <- parameter_table[!duplicated(parameter_table$parameterId), ]
names(parameter_table) <- c("parameter_stable_id", "parameter_name")

# Add the parameters that are only in the analysis data (not in the reference file)
analysis_params <- unique(cleaned_analysis_data[, c("parameter_stable_id", "parameter_name")])
analysis_params <- analysis_params[!duplicated(analysis_params$parameter_stable_id) &
                                     !analysis_params$parameter_stable_id %in% parameter_table$parameter_stable_id, ]
parameter_table <- rbind(parameter_table, analysis_params)

# Groups: matched on the parameter name (naming similarity) or on the
# procedure code inside the parameter ID (test similarity)
groups <- data.frame(
  group_id   = 1:10,
  group_name = c("weight", "images", "brain", "bone", "heart", "behaviour",
                 "metabolism", "blood", "eye", "hearing"),
  match_on   = c("name", "name", "name", rep("id", 7)),
  pattern    = c("weight", "image", "brain", "_(DXA|XRY)_", "_ECG_", "_(OFD|GRS)_",
                 "_(IPG|CAL)_", "_(HEM|CBC)_", "_EYE_", "_ABR_"),
  group_description = c(
    "Parameters whose name contains 'weight'",
    "Parameters whose name contains 'image'",
    "Parameters whose name contains 'brain'",
    "Body composition (DXA) and X-ray (XRY) procedures",
    "Electrocardiogram (ECG) procedure",
    "Open field (OFD) and grip strength (GRS) procedures",
    "Glucose tolerance (IPG) and indirect calorimetry (CAL) procedures",
    "Haematology (HEM) and clinical blood chemistry (CBC) procedures",
    "Eye morphology (EYE) procedure",
    "Auditory brainstem response (ABR) procedure")
)

# Parameters that describe how a test was run, not a phenotype (excluded from all groups)
metadata <- paste(
  "equipment|experimenter|date|comment|calibrat|manufacturer|model|restrained|strip|pixel",
  "anaesthe|anesthe|dosage|administration route|time of injection|method|tube|anticoagulant",
  "storage|kept on ice|sample|sop$|sop |dilution|bleed|fasting|chip card|fight wounds",
  "mouse status|^alive$|settings|voltage|free ontology|disinfectant|start time|software",
  "arena size|arena id|arena material|color of arena|height of the wall|light intensity",
  "light level|noise level|number of animals|type of analysis|zone|definition threshold",
  "light beam|lightbeam|lead configuration|sampling rate|range of|step size|tone pip",
  "repetition rate|number averages|recording environment|extension of stimulus",
  "room temperature|chamber temperature|dimension|height from platform|available space",
  "infrared|scanning rate|refractory|bedding|igloo|acclimation|dark cycle|duration of test",
  "upper limit|scheimpflug|sheimpflug|^haw$",
  sep = "|")

is_metadata <- grepl(metadata, parameter_table$parameter_name, ignore.case = TRUE) |
  grepl("_EXD_", parameter_table$parameter_stable_id)

# Body weight belongs in the weight group, not the procedure-based groups
is_body_weight <- grepl("^body weight", parameter_table$parameter_name, ignore.case = TRUE)

# Assign parameters to groups (a parameter can be in more than one group)
parameter_group_member <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  if (groups$match_on[i] == "name") {
    hit <- grepl(groups$pattern[i], parameter_table$parameter_name, ignore.case = TRUE)
  } else {
    hit <- grepl(groups$pattern[i], parameter_table$parameter_stable_id) & !is_body_weight
  }
  hit <- hit & !is_metadata
  data.frame(group_id = groups$group_id[i],
             parameter_stable_id = parameter_table$parameter_stable_id[hit])
}))

# Check: parameters per group, and parameters in more than one group
table(groups$group_name[parameter_group_member$group_id])
sum(duplicated(parameter_group_member$parameter_stable_id))

# Group lookup table (each group's name and description stored once)
parameter_group <- groups[, c("group_id", "group_name", "group_description")]

# Add a group_id column to the parameters table. A column holds one group per
# parameter, so a parameter in two groups keeps the first group listed above
first_group <- parameter_group_member[!duplicated(parameter_group_member$parameter_stable_id), ]
parameters_grouped <- merge(parameters_grouped, first_group,
                            by.x = "parameterId", by.y = "parameter_stable_id", all.x = TRUE)

# Save the parameters table with its group_id column, and the two group tables
write.csv(parameters_grouped, "Results/Cleaned_Data/Parameters_withGroups.csv", row.names = FALSE)
write.csv(parameter_group, "Results/Cleaned_Data/Parameter_group.csv", row.names = FALSE)
write.csv(parameter_group_member, "Results/Cleaned_Data/Parameter_group_member.csv", row.names = FALSE)

# Readable list of each group's parameters, to check by eye
group_check <- merge(merge(parameter_group_member, parameter_group[, 1:2]), parameter_table)
write.csv(group_check[order(group_check$group_name), c("group_name", "parameter_stable_id", "parameter_name")],
          "Results/Cleaned_Data/Parameter_groups_check.csv", row.names = FALSE)




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



