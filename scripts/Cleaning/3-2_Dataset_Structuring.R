# Call the previous scripts
library(ggplot2)



cleaned_analysis_data <- readRDS("Results/Cleaning_intermediates/3-1_Spellcheck_Cleaning.rds")
parameters <- read.csv("Data/IMPC_parameter_description.txt", strip.white = TRUE)
procedures <- read.csv("Data/IMPC_procedure.txt", strip.white = TRUE)
stat_resuls <- read.csv("Data/External_Data/statistical-results-brief.csv")


# ================= # 
# Clean Row duplicates
# ================= #
# Do a quick check to make sure the rows are identical and then just drop them.
# if not, drop both as we cant know which is the correct one.


# TO be completed...





# For the remainder, I will re-evaluate them after running through my 
# spell-correcting script.


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
# Introduce Parameter groupings into the parameter dataset 
# ================= #

# Copy of the parameter reference table, which will get a group_id column
parameters_grouped <- unique(parameters)

# Parameters to group: only those with statistical results in the analysis data
# Test-setup fields (e.g. equipment, arena settings, reagents) are never analysed, so they are left out
parameter_table <- unique(cleaned_analysis_data[, c("parameter_stable_id", "parameter_name")])

# Image parameters are never statistically analysed, but the brief requires an images group,
# so they are added from the parameter reference table (excluding image comments, pixel size and capture date)
image_params <- parameters[grepl("image", parameters$name, ignore.case = TRUE) &
                             !grepl("comment|pixel|date", parameters$name, ignore.case = TRUE),
                           c("parameterId", "name")]
names(image_params) <- c("parameter_stable_id", "parameter_name")
parameter_table <- rbind(parameter_table, image_params)

# Keep one row per parameter ID
parameter_table <- parameter_table[!duplicated(parameter_table$parameter_stable_id), ]

# 19 groups: weight, images and brain (required by the brief) are matched on parameter name;
# the other 16 are matched on the procedure code inside the parameter ID (e.g. IMPC_ECG_001_001)
groups <- data.frame(
  group_id   = 1:19,
  group_name = c("weight", "images", "brain", "bone", "heart", "behaviour", "muscle",
                 "metabolism", "blood", "eye", "hearing", "immunology", "pathology",
                 "embryo", "placenta", "viability and fertility", "gene expression",
                 "respiratory", "general observation"),
  match_on   = c("name", "name", "name", rep("id", 16)),
  pattern    = c("weight", "image", "brain",
                 "_(DXA|XRY)_", "_(ECG|ECH|HWT)_", "_(OFD|LDT|YMZ|FEA|ACS|SLW)_", "_(GRS|ROT)_",
                 "_(IPG|CAL|INS|URI)_", "_(HEM|CBC|PLC)_", "_EYE_", "_ABR_", "_(IMM|MLN|PBI|BMI|EEI)_",
                 "_(HIS|PAT|BLK)_", "_(GEL|GEM|GEO|GEP|EMA|EML|EMO|EOL|MAA)_",
                 "_(GPL|GPM|GPO|GPP)_", "_(VIA|EVL|EVM|EVO|EVP|FER)_", "_(ALZ|ELZ)_",
                 "_CHL_", "_(CSD|WEL)_"),
  group_description = c(
    "Parameters whose name contains 'weight'",
    "Parameters whose name contains 'image'",
    "Parameters whose name contains 'brain'",
    "Body composition (DXA) and X-ray (XRY) procedures",
    "Electrocardiogram (ECG), echocardiography (ECH) and heart weight (HWT) procedures",
    "Open field (OFD), light-dark (LDT), Y-maze (YMZ), fear conditioning (FEA), acoustic startle (ACS) and sleep-wake (SLW) procedures",
    "Grip strength (GRS) and rotarod (ROT) procedures",
    "Glucose tolerance (IPG), indirect calorimetry (CAL), insulin (INS) and urinalysis (URI) procedures",
    "Haematology (HEM), clinical blood chemistry (CBC) and plasma chemistry (PLC) procedures",
    "Eye morphology (EYE) procedure",
    "Auditory brainstem response (ABR) procedure",
    "Immunophenotyping of spleen (IMM), mesenteric lymph node (MLN), peripheral blood (PBI), bone marrow (BMI) and ear epidermis (EEI) procedures",
    "Histopathology (HIS), gross pathology (PAT) and tissue banking (BLK) procedures",
    "Embryo gross morphology (GEL, GEM, GEO, GEP), microCT (EMA, EML, EMO, MAA) and OPT (EOL) procedures",
    "Placenta gross morphology (GPL, GPM, GPO, GPP) procedures",
    "Viability (VIA, EVL, EVM, EVO, EVP) and fertility (FER) procedures",
    "Adult (ALZ) and embryo (ELZ) lacZ expression procedures",
    "Whole body plethysmography (CHL) procedure",
    "SHIRPA and dysmorphology (CSD) and welfare observation (WEL) procedures"))

# Body weight is recorded in most procedures, so it is kept out of the procedure groups
# and only goes in the weight group
is_body_weight <- grepl("^body weight", parameter_table$parameter_name, ignore.case = TRUE)

# Match every parameter against each group's pattern (by name or by ID)
# A parameter can match more than one group (e.g. grip strength normalised against body weight)
parameter_group_member <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
  if (groups$match_on[i] == "name") {
    hit <- grepl(groups$pattern[i], parameter_table$parameter_name, ignore.case = TRUE)
  } else {
    hit <- grepl(groups$pattern[i], parameter_table$parameter_stable_id) & !is_body_weight
  }
  data.frame(group_id = rep(groups$group_id[i], sum(hit)),
             parameter_stable_id = parameter_table$parameter_stable_id[hit])
}))

# Checks: number of parameters per group, and how many parameters are in more than one group
table(groups$group_name[parameter_group_member$group_id])
sum(duplicated(parameter_group_member$parameter_stable_id))

# Group lookup table (parameter_group in the database)
parameter_group <- groups[, c("group_id", "group_name", "group_description")]

# Add a group_id column to the parameters table, keeping the first group for each parameter
# (parameter_group_member keeps all of them)
first_group <- parameter_group_member[!duplicated(parameter_group_member$parameter_stable_id), ]
parameters_grouped <- merge(parameters_grouped, first_group,
                            by.x = "parameterId", by.y = "parameter_stable_id", all.x = TRUE)

# Save the grouped parameters table, the group lookup and the group membership table
write.csv(parameters_grouped, "Results/Cleaned_Data/Parameters_withGroups.csv", row.names = FALSE)
write.csv(parameter_group, "Results/Cleaned_Data/Parameter_group.csv", row.names = FALSE)
write.csv(parameter_group_member, "Results/Cleaned_Data/Parameter_group_member.csv", row.names = FALSE)

# Check file: every grouped parameter with its group and name, for manual review
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



