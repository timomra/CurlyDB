# Data Cleaning Scripts

A final cleaned Dataframe can be found under `Results/Cleaned_Data`

---

1. ``Initial_SOP_QualityChecks.R``  Runs the initial quality checks. (By Ella)
2. `SOP_cleaning.R` Imports and validates Group 13 CSV files, identifies and repairs structural errors in memory, and combines records into a single data frame. Additionally, flags duplicate and conflicting analyses for further investigation. (by Melissa)
3. `SpellCheck_Cleaning.R`  calls the previous scripts, and uses them to clean the data and create the final cleaned dataframe. (By Tim)
4. `Dataset_Structuring.R` Performs the final cleaning modifications (removing duplicate rows etc...) and adds additional information (parameter groupings, procedure IDs) to the dataset before it is added to the database (By Tim, Ella, Melissa)

---

**Function Scripts**

`SpellCheck_Function.R` defines a function which is uses Levenshtein distance for spell checks. (see ``SpellCheck_cleaning_function.svg`` for a visualisation of how it works)

