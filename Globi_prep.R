# Packages ####
library(dplyr)

# Load data ####
setwd("E:/Globi/Data")
cabi <- read.csv(file = "CABI_FC_interactions_scored.csv", header = T)
glonaf <- read.csv(file = "GloNAF_interactions_toscore.csv", header = T)

# Clean up data to remove useless columns and make column names consistent ####
cabi <- cabi %>% select(-X.1, -X.2)
glonaf$data_quality <- glonaf$Data.quality
glonaf <- glonaf %>% select(-Data.quality)

# Merge the dataframe into one dataframe ####
df <- rbind(cabi,glonaf)

# Change variable classes ####
df$source_taxon_name <- as.factor(df$source_taxon_name)
df$source_taxon_external_id <- as.factor(df$source_taxon_external_id)
df$source_specimen_life_stage <- as.factor(df$source_specimen_life_stage)
df$interaction_type <- as.factor(df$interaction_type)
df$target_taxon_name <- as.factor(df$target_taxon_name)
df$target_specimen_life_stage <- as.factor(df$target_specimen_life_stage)
df$data_quality <- as.factor(df$data_quality)

# Split taxon name into genus and species columns, with separate column for binomial ####
df$source_binomial <- df$source_taxon_name
df$target_binomial <- df$target_taxon_name
df <- df %>% separate(col=source_taxon_name, into = c("S_Genus", "S_Species"),sep=" ") %>%
  separate(col=target_taxon_name, into = c("T_Genus", "T_Species"),sep = " ")

# Change names to factors ####
df$source_binomial <- as.factor(df$source_binomial)
df$target_binomial <- as.factor(df$target_binomial)
df$S_Genus <- as.factor(df$S_Genus)
df$S_Species <- as.factor(df$S_Species)
df$T_Genus <- as.factor(df$T_Genus)
df$T_Species <- as.factor(df$T_Species)

# Create subset with only expert-quality data (quality 4 and 5) ####
expert <- df %>% filter(data_quality %in% c(4,5))
summary(expert$data_quality)

# Create subset of all qualities for Opuntia only ####
opuntia <- df %>% filter(S_Genus == "Opuntia" | T_Genus == "Opuntia")

# Remove observations with no:match in species ID ####
df <- df %>% filter(source_taxon_external_id != "no:match")
expert <- expert %>% filter(source_taxon_external_id != "no:match")

# Save dataframes
save(df, file="globi_cleaned_all.Rdata")
save(expert, file="globi_cleaned_expert.Rdata")
save(opuntia, file="globi_cleaned_opuntia.Rdata")
