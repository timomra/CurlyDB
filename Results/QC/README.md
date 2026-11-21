# QC check results:



##### 1. `SOP_errors.csv` - checks for missing values, data type and min/max value

QC result:

Missing values: every field has at least one

- 248-294 in gene_symbol, mouse_life_stage, parameter_stable_id, p_value, allele_symbol and each lacz field
- 1-3 in analysis_id, gene_accession_id, strain_name and parameter_name

Wrong data type: 1 p_value is not numeric

Out of range:

- gene_symbol 15, mouse_life_stage 3, p_value 1 (value 1.89)
- lacz fields 7-1,150 per tissue. The high counts (heart 1,150, vascular_system 1,150, skeletal_muscle 1,142, eye 681, blood 567) are "tissue not available" (20 characters) with an injected symbol or trailing space, e.g. "tissu&e not available"
- none in analysis_id, gene_accession_id, strain_name, parameter_stable_id, parameter_name or allele_symbol

Note: the SOP contradicts itself for three fields, so these limits were corrected (to be confirmed with the module lead): analysis_id 15 -> 12 characters, lacz_* maximum 19 -> 20, strain_name maximum 25 -> 26





##### 2. `SOP_remark_errors.csv` Allowed values, formats, uniqueness and allele symbols

QC result:

Not in SOP allowed values: 

- strain_name 0 (all valid), 
- mouse_life_stage 3,163, 
- lacz fields 36,535 (across all 11 tissues)

The invalid values have injected symbols (e.g. "E#arly adult"), wrong case (e.g. "EARLY ADULT") or a trailing space (e.g. "Early adult ")

analysis_id: all alphanumeric, but 60 duplicates

gene_accession_id: all in MGI format

gene_symbol: 1,826 not title format / alphanumeric

parameter_stable_id: 5,242 not alphanumeric with underscores

allele_symbol: 3,168 do not start with the gene symbol and 832 have unclosed brackets



##### 3. `SOP_structural_checks.csv` Structural and consistency checks



QC result:
3 files do not have 20 x 2 rows/columns, including the 2 files fread stopped reading early at a malformed line
6 files have missing, extra or duplicated fields: 2 are badly formatted names
(e.g. laczLung), so 4 have genuinely missing, extra or broken fields
4,261 values have leading/trailing whitespace
30 duplicate records (identical in every field except analysis_id)
1,253 gene_accession_ids map to more than one gene_symbol
Each parameter_stable_id maps to a single parameter_name


##### 4. `Reference_file_checks.csv` Reference files: parameters, procedures, diseases and query genes

QC result:
Parameter file: 2,691 missing descriptions, 14 duplicate rows and 14 duplicate
impcParameterOrigIds (the counts match, so each repeated ID is an exact copy of a row); 25 parameterIds appear with more than one name
Procedure file: 3,481 missing descriptions; 1 procedure has more than one description
Disease file: 18 comment lines; all 13,912 data lines have 8 columns; 6,830 rows have no mouse genotype IDs (no mouse model recorded for that disease)
Query genes: all 5 found in the analysis data; Mmp15 (MGI:109320) has no disease information


##### 5. `link_problems.csv` Links between the analysis data and the reference files

(These will probably become issues when we try to build the DB)

QC result:
5,195 distinct parameter_stable_ids are not in the parameter file: 4,143 contain injected symbols and 1,052 are well-formed IDs absent from the file (mostly non-IMPC pipelines, e.g. MGP_, JAX_, UCD_)
Every parameter_stable_id that is in the file has the matching parameter_name
919 distinct genes in the analysis data have disease information



