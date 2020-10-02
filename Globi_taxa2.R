# Packages
library(stringr)

# Load data ####
setwd("E:/Globi/Data")
expert <- get(load("globi_cleaned_expert.Rdata"))
data <- expert
rm(expert)

# Phyla dataset uses phyla listed at https://www.wikiwand.com/en/Phylum#/Known_phyla
phyla <- read.csv(file="C:/temp/phyla.csv",header=T)

# Turn phylum column into a pattern to match
p <- as.data.frame(phyla$Phylum)
pattern <- paste(unlist(t(p)), collapse="|")

# Find phylum from each taxon path
d1 <- as.data.frame(str_extract(data$source_taxon_path, pattern))
colnames(d1) <- "S_Phylum"

d2 <- as.data.frame(str_extract(data$target_taxon_path, pattern))
colnames(d2) <- "T_Phylum"

# Combine phylum information with data and look up the Kingdom
data <- cbind(data,d1)
data <- cbind(data,d2)

K1 <- phyla
K2 <- phyla

colnames(K1) <- c("S_Kingdom","S_Phylum")
colnames(K2) <- c("T_Kingdom","T_Phylum")

data <- merge(data, K1, by="S_Phylum",all.x=T)
data <- merge(data, K2, by="T_Phylum",all.x=T)

save(data,file="globi_cleaned_expert_taxized.Rdata")
