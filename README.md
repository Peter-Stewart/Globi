# Globi
Code related to analysis of the Globi / GloNAF / CABI datasets

Currently contains:
  - Globi_prep - combines CABI and GloNAF dataframes into a single globi dataframe, splits taxon name into genus/species, removes observations with no:match ID's, and creates    subsets for expert-verified interactions (data quality 4 & 5), and for interactions involving members of the opuntia genus
