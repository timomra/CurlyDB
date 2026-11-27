#!/bin/bash -l

#SBATCH -p msc_appbio        # <- to run on the msc_appbio node (change this if necessary)
#SBATCH --job-name=Run_DCDM13_cleaning
#SBATCH --nodes=1

#SBATCH --time=48:00:00


# >>> If on HPC
# module load r/4.5.1


# >>> if on local
# export PATH="/path/to/R/bin:$PATH"

export PATH="/c/Program Files/R/R-4.5.1/bin:$PATH"


cd ../..


#====== 1. run the initial quality checks script
## this is actually uneccessary as 2-1_data_cleaning_structural_validation.R already sources this script
## and thus generates the report automatically

#Rscript Scripts/Cleaning/1-1_Initial_SOP_QualityChecks.R


#====== 2. run the SOP cleaning

Rscript Scripts/Cleaning/2-1_data_cleaning_structural_validation.R

Rscript Scripts/Cleaning/2-2_data_cleaning_sop.R


## 3. Run the spelling corrections and Dataset Restructuring
Rscript Scripts/Cleaning/3-1_SpellCheck_Cleaning.R

Rscript Scripts/Cleaning/3-2_Dataset_Structuring.R
