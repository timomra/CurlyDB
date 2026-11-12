# Database Schema Design

## 1. Proposed database tables

### Table 1: Gene

**Purpose:** Store information about individual mouse genes.

| Attribute | Data type | Key | Description |
|---|---|---|---|
| gene_accession_id | VARCHAR | PK | Unique MGI gene identifier |
| gene_symbol | VARCHAR | — | Gene symbol |

### Table 2: Allele

**Purpose:** Store information about genetically modified alleles.

| Attribute | Data type | Key | Description |
|---|---|---|---|
| allele_id | INT | PK | Unique allele identifier |
| allele_symbol | VARCHAR | — | Engineered allele symbol |
| gene_accession_id | VARCHAR | FK | References Gene(gene_accession_id) |

### Table 3: Strain

**Purpose:** Store the genetic backgrounds of the mice.

| Attribute | Data type | Key | Description |
|---|---|---|---|
| strain_id | INT | PK | Unique strain identifier |
| strain_name | VARCHAR | UNIQUE | Name of the mouse strain |

### Table 4: Phenotype Analysis

**Purpose:** Store statistical results from mouse phenotyping analyses.

| Attribute | Data type | Key | Description |
|---|---|---|---|
| analysis_id | VARCHAR | PK | Unique statistical analysis identifier |
| allele_id | INT | FK | References Allele(allele_id) |
| strain_id | INT | FK | References Strain(strain_id) |
| parameter_id | VARCHAR | FK | References Parameter(parameter_id) |
| p_value | DOUBLE | — | P-value for phenotype association |
| mouse_life_stage | VARCHAR | — | Developmental stage at phenotyping |

### Table 5: Parameter

**Purpose:** Store information about IMPC phenotyping parameters.

| Attribute | Data type | Key | Description |
|---|---|---|---|
| parameter_id | VARCHAR | PK | Stable IMPC parameter identifier |
| impc_parameter_orig_id | INT | Candidate UNIQUE | Original numerical parameter identifier |
| parameter_name | VARCHAR | — | Name of the parameter |
| description | TEXT | — | Additional parameter description |

## 2. Table relationships

| Parent table | Child table | Relationship | Linking attribute |
|---|---|---|---|
| Gene | Allele | One-to-many (1:N) | gene_accession_id |
| Allele | Phenotype Analysis | One-to-many (1:N) | allele_id |
| Strain | Phenotype Analysis | One-to-many (1:N) | strain_id |
| Parameter | Phenotype Analysis | One-to-many (1:N) | parameter_id |
