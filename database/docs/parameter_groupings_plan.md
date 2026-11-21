# Parameter groupings plan

The brief asks for groupings for: **weight, images and brain** **+ minimum 3 others**,
based on test similarity +/ naming similarity, stored in the database.

## Groups

| Group | How parameters are chosen | Type |
|---|---|---|
| weight | name contains "weight" | naming similarity (required) |
| images | name contains "image" | naming similarity (required) |
| brain | name contains "brain" | naming similarity (required) |
| bone | DXA (body composition) and XRY (X-ray) procedures | test similarity |
| heart | ECG (electrocardiogram) procedure | test similarity |
| behaviour | OFD (open field) and GRS (grip strength) procedures | test similarity |
| metabolism | IPG (glucose tolerance) and CAL (calorimetry) procedures | test similarity |
| blood | HEM (haematology) and CBC (clinical blood chemistry) procedures | test similarity |
| eye | EYE (eye morphology) procedure | test similarity |
| hearing | ABR (auditory brainstem response) procedure | test similarity |

Test-similarity groups use the procedure code inside the parameter ID
(e.g. IMPC_**ECG**_001_001).

## Exclusion rules

- **Metadata is excluded from all groups:** parameters that record how a test was run,
  not a phenotype (names containing equipment, experimenter, date, comment, calibrat,
  manufacturer, model, restrained, strip or pixel), and all experimental design (EXD)
  parameters
- **Body weight is excluded from the procedure-based groups:** most procedures record
  body weight, but it belongs in the weight group

## Database tables

- `parameter_group`: group_id (PK), group_name, group_description
- `parameter_group_member`: group_id (PK, FK), parameter_stable_id (PK, FK)

A parameter can belong to more than one group
(e.g. grip strength normalised against body weight is in weight and behaviour).

## Steps

1. Agree the groups as a team
2. Clean the data to produce the parameter table
3. Run the grouping code on the cleaned parameter table
4. Review each group by eye and adjust the rules if needed
   (e.g. metabolism includes "Room temperature"; images includes "Showcase image")
5. Load the groups into `parameter_group` and `parameter_group_member`
6. Explain the groupings and exclusion rules in the report
