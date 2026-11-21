# External Datasets used

As discussed with the module co-ordinator, we were permitted to use external datasets to enhance our data cleaning and database.

As such we used:

1. `MRK_List2.rpt"` Complete list of MGI genes (from: https://www.informatics.jax.org/downloads/reports/index.html)

2. `MGI_PhenotypicAllele.rpt` A large list of MGI allele names. (from: https://www.informatics.jax.org/downloads/reports/MGI_PhenotypicAllele.rpt)

3. ``statistical-results-ALL.csv`` All statistical results from the IMPC (found at: https://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-25.0/results/)



**As the  `statistical-results-ALL.csv` is very large, I trimmed it down with the following command before loading it into Rstudio**

``python -m csvkit.utilities.csvcut -c <selected column headers> statistical-results-ALL.csv > statistical-results-brief.csv``










