# Call the previous scripts
source("Scripts/Cleaning/SpellCheck_Cleaning.R")


# 

cleaned_analysis_data <- cleaned_spellings


# ================= # 
# Clean Row duplicates
# ================= #
# do a quick check to make sure the rows are identical and then just drop them.
# if not, drop both as we cant know which is the correct one.



# ================= # 
# Flag potential duplicates
# ================= #


# ================= # 
# Flag Significant results
# ================= #
# 

clean



# ================= # 
# Export Cleaned Data 
# ================= #




# ================= # 
# Group procedure descriptions
# ================= #


stat_resuls[(stat_resuls$p_value <1e-4) & (stat_resuls$significant == "false"),]

# Finally, save the cleaned dataframe
write.csv(cleaned_spellings, file="Results/Cleaned_Data/Cleaned_Data.csv")




















write.csv(cleaned_spellings, file="Results/Cleaned_Data/Cleaned_Data.csv")
