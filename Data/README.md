# External Datasets used

As discussed with the module co-ordinator, we were permitted to use external datasets to enhance our data cleaning and database.

As such we used:

1. `MRK_List2.rpt"` Complete list of MGI genes (from: https://www.informatics.jax.org/downloads/reports/index.html)

2. `MGI_PhenotypicAllele.rpt` A large list of MGI allele names. (from: https://www.informatics.jax.org/downloads/reports/MGI_PhenotypicAllele.rpt)

3. ``statistical-results-ALL.csv`` All statistical results from the IMPC (found at: https://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-25.0/results/)



**As the  `statistical-results-ALL.csv` is very large, I trimmed it down with the following command before loading it into Rstudio**

``python -m csvkit.utilities.csvcut -c parameter_stable_id,p_value,significant statistical-results-ALL.csv > statistical-results-brief.csv``







phenotyping_center,

intercept_estimate,

weight_effect_p_value,

male_mutant_count,

pipeline_stable_key,

female_ko_effect_p_value,

pipeline_stable_id,

parameter_stable_key,

data_type,

parameter_stable_id,

interaction_significant,

strain_accession_id,

parameter_name,

allele_name,

weight_effect_stderr_estimate,

weight_effect_parameter_estimate,

procedure_stable_id,

status,

sex_effect_parameter_estimate,

female_ko_effect_stderr_estimate,

female_percentage_change,

group_2_residuals_normality_test,

marker_accession_id,

mp_term_name,

group_1_residuals_normality_test,

genotype_effect_p_value,

resource_name,

procedure_name,

doc_id,

top_level_mp_term_id,

allele_accession_id,

p_value,

marker_symbol,

pipeline_name,

sex,

interaction_effect_p_value,

colony_id,

project_name,

female_ko_parameter_estimate,

female_mutant_count,

female_control_count,

intermediate_mp_term_id,

male_ko_effect_p_value,

top_level_mp_term_name,

metadata_group,

sex_effect_stderr_estimate,

zygosity,

male_percentage_change,

sex_effect_p_value,

mp_term_id,

male_ko_effect_stderr_estimate,

additional_information,

statistical_method,

intercept_estimate_stderr_estimate,

male_control_count,

intermediate_mp_term_name,

strain_name,

classification_tag,

effect_size,

procedure_stable_key,

allele_symbol,

group_2_genotype,

variance_significant,

group_1_genotype,

male_ko_parameter_estimate,

genotype_effect_parameter_estimate,

batch_significant,

genotype_effect_stderr_estimate,

resource_fullname,

significant
