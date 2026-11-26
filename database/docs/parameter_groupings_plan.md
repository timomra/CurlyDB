# Parameter groupings plan

The brief asks for groupings for: **weight, images and brain** **+ minimum 3 others**,
based on test similarity +/ naming similarity, stored in the database.

We use **20 groups**: the 3 required groups (by name) and 17 extra groups (by procedure).

## Groups

| ID | Group | How parameters are chosen | Type |
|---|---|---|---|
| 1 | weight | name contains "weight" | naming similarity (required) |
| 2 | images | name contains "image" | naming similarity (required) |
| 3 | brain | name contains "brain" | naming similarity (required) |
| 4 | bone | Body composition (DXA) and X-ray (XRY) | test similarity |
| 5 | heart | Electrocardiogram (ECG), echocardiography (ECH) and heart weight (HWT) | test similarity |
| 6 | behaviour | Open field (OFD), light-dark (LDT), Y-maze (YMZ), fear conditioning (FEA) and acoustic startle (ACS) | test similarity |
| 7 | muscle | Grip strength (GRS) | test similarity |
| 8 | metabolism | Glucose tolerance (IPG), indirect calorimetry (CAL) and insulin (INS) | test similarity |
| 9 | blood | Haematology (HEM) and clinical blood chemistry (CBC) | test similarity |
| 10 | eye | Eye morphology (EYE) | test similarity |
| 11 | hearing | Auditory brainstem response (ABR) | test similarity |
| 12 | immunology | Immunophenotyping (IMM) | test similarity |
| 13 | pathology | Histopathology (HIS), gross pathology (PAT) and tissue banking (BLK) | test similarity |
| 14 | embryo | Embryo gross morphology (GEL, GEM, GEO, GEP), microCT (EMA, EML, EMO, MAA) and OPT (EOL) | test similarity |
| 15 | placenta | Placenta gross morphology (GPL, GPM, GPO, GPP) | test similarity |
| 16 | viability and fertility | Viability (VIA, EVL, EVM, EVO, EVP) and fertility (FER) | test similarity |
| 17 | gene expression | Adult (ALZ) and embryo (ELZ) lacZ expression | test similarity |
| 18 | respiratory | Whole body plethysmography (CHL) | test similarity |
| 19 | pain | Formalin pain test (FOR) | test similarity |
| 20 | general observation | SHIRPA and dysmorphology (CSD) and welfare observation (WEL) | test similarity |

Test-similarity groups use the procedure code inside the parameter ID
(e.g. IMPC_**ECG**_001_001). Every phenotyping procedure belongs to a group;
experimental design (EXD) and housing and husbandry (HOU) are left out because
they are not phenotypes.

## Exclusion rules

- **Test-setup parameters are excluded from all groups:** parameters that record how a test
  was run rather than a phenotype (e.g. equipment, experimenter, dates, anaesthetic, sample
  handling, arena settings, room temperature), and all experimental design (EXD) parameters
- **Body weight is excluded from the procedure-based groups:** most procedures record
  body weight, but it belongs in the weight group

## Database tables

- `parameter_group`: group_id (PK), group_name, group_description
- `parameter_group_member`: group_id (PK, FK), parameter_stable_id (PK, FK)
- The parameters table also has a `group_id` column (FK), holding one group per parameter

A parameter can belong to more than one group
(e.g. grip strength normalised against body weight is in weight and muscle).
The `group_id` column keeps the first group listed; `parameter_group_member` keeps all of them.

## Known limitations

- Parameter IDs without a procedure code (e.g. ESLIM_005_001_701, M-G-P_004_001_001)
  can only be grouped by name
- Name-based groups are broad (e.g. "weight" includes ratios normalised against body weight)
- Parameter IDs with more than one name in the reference file use the first name listed
