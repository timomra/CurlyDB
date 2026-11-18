# QC check results:



##### 1. `SOP_errors.csv` - checks for missing values, data type and min/max value

QC result:every field has at least one missing value apart from parameter_name 
QC result:every field has at least two out of range values apart from analysis_id, gene_accession_id, parameter_stable_id and parameter_name





##### 2. `SOP_remark_errors.csv` Allowed values, formats, uniqueness and allele symbols

QC result:

Not in SOP allowed values: 

- strain_name 1,540, 
- mouse_life_stage 3,163, 
- lacz fields 36,536 (across all 11 tissues)

analysis_id: 60 duplicates

gene_symbol: 1,913 not title format / alphanumeric

parameter_stable_id: 5,242 not alphanumeric with underscores

allele_symbol: 3,168 do not start with the gene symbol and 832 have unclosed brackets



##### 3. `SOP_structural_checks.csv` Structural and consistency checks



QC result:
1 file does not have 20 x 2 rows/columns (the file with the unmatched quote mark)
5 files have missing, extra or duplicated fields: 2 are badly formatted names
(e.g. laczLung), so 3 have genuinely missing, extra or broken fields
4,261 values have leading/trailing whitespace
30 duplicate records (identical in every field except analysis_id)
1,253 gene_accession_ids map to more than one gene_symbol
Each parameter_stable_id maps to a single parameter_name


##### 4. `Reference_file_checks.csv` Reference files: parameters, procedures, diseases and query genes

QC result:
Parameter file: 2,691 missing descriptions, 14 duplicate rows and 14 duplicate
impcParameterOrigIds (the counts match, so each repeated ID is an exact copy of a row); 25 parameterIds map to more than one name
Procedure file: 3,481 missing descriptions; 1 procedure has more than one description
Disease file: 6,830 rows have no mouse genotype IDs (no mouse model recorded for that disease)
Query genes: all found in the analysis data; 1 query gene's accession ID is not in the disease file


##### 5. `link_problems.csv` Links between the analysis data and the reference files

(These will probably become issues when we try to build the DB)

QC result:
5,195 distinct parameter_stable_ids are not in the parameter file
919 distinct genes in the analysis data have disease information





