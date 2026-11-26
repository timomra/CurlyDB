# Parameter groupings plan

The brief asks for groupings for: **weight, images and brain** **+ minimum 3 others**,
based on test similarity and/or naming similarity, stored in the database.

We use **20 groups**: the 3 required groups (by name) and 17 extra groups (by procedure).

## Groups
| ID | Group | How parameters are chosen | Type |
|---|---|---|---|
| 1 | weight | name contains "weight" | naming similarity (required) |
| 2 | images | name contains "image" | naming similarity (required) |
| 3 | brain | name contains "brain" | naming similarity (required) |
| 4 | bone | Body composition (DXA) and X-ray (XRY) | test similarity |
| 5 | heart | Electrocardiogram (ECG), echocardiography (ECH) and heart weight (HWT) | test similarity |
| 6 | behaviour | Open field (OFD), light-dark (LDT), Y-maze (YMZ), fear conditioning (FEA), acoustic startle (ACS) and sleep-wake (SLW) | test similarity |
| 7 | muscle | Grip strength (GRS) and rotarod (ROT) | test similarity |
| 8 | metabolism | Glucose tolerance (IPG), indirect calorimetry (CAL), insulin (INS) and urinalysis (URI) | test similarity |
| 9 | blood | Haematology (HEM), clinical blood chemistry (CBC) and plasma chemistry (PLC) | test similarity |
| 10 | eye | Eye morphology (EYE) | test similarity |
| 11 | hearing | Auditory brainstem response (ABR) | test similarity |
| 12 | immunology | Immunophenotyping of spleen (IMM), mesenteric lymph node (MLN), peripheral blood (PBI), bone marrow (BMI) and ear epidermis (EEI) | test similarity |
| 13 | pathology | Histopathology (HIS), gross pathology (PAT) and tissue banking (BLK) | test similarity |
| 14 | embryo | Embryo gross morphology (GEL, GEM, GEO, GEP), microCT (EMA, EML, EMO, MAA) and OPT (EOL) | test similarity |
| 15 | placenta | Placenta gross morphology (GPL, GPM, GPO, GPP) | test similarity |
| 16 | viability and fertility | Viability (VIA, EVL, EVM, EVO, EVP) and fertility (FER) | test similarity |
| 17 | gene expression | Adult (ALZ) and embryo (ELZ) lacZ expression | test similarity |
| 18 | respiratory | Whole body plethysmography (CHL) | test similarity |
| 19 | pain | Formalin pain test (FOR) | test similarity |
| 20 | general observation | SHIRPA and dysmorphology (CSD) and welfare observation (WEL) | test similarity |

Test-similarity groups use the procedure code inside the parameter ID
(e.g. IMPC_**ECG**_001_001). Every phenotyping procedure belongs to a group except LFO and OWT
(see Known limitations). Only parameters with statistical results in the analysis data are grouped.

The 20 groups cover **91.6% of analyses**. Procedure codes from other centres' pipelines
(SLW, ROT, URI, PLC, MLN, PBI, BMI, EEI) were assigned to groups after checking their parameter names.

## Exclusion rules

- **Test-setup parameters are excluded from all groups:** parameters that record how a test
  was run rather than a phenotype (e.g. equipment, arena settings, reagents, sample handling)
  are never statistically analysed, so they are not in the analysis data and are not grouped
- **Body weight is excluded from the procedure-based groups:** most procedures record
  body weight, but it belongs in the weight group

## Database tables

- `parameter_group`: group_id (PK), group_name, group_description
- `parameter_group_member`: group_id (PK, FK), parameter_stable_id (PK, FK)
- The parameters table also has a `group_id` column (FK), holding one group per parameter
- Parameters with no statistical results have no group (`group_id` is NULL)

A parameter can belong to more than one group
(e.g. grip strength normalised against body weight is in weight and muscle).
The `group_id` column keeps the first group listed; `parameter_group_member` keeps all of them.

## Known limitations

- Parameter IDs without a procedure code (e.g. ESLIM_005_001_701, M-G-P_004_001_001)
  can only be grouped by name
- Name-based groups are broad (e.g. "weight" includes ratios normalised against body weight)
- Parameter IDs with more than one name in the analysis data use the first name listed
- LFO parameters (e.g. "Hqp", "Esn") are left ungrouped because their names are uninformative
- OWT organ weights named without "weight" (e.g. "Left kidney") are not matched by the weight group


