# Packages ####
library(dplyr)
library(igraph)
library(network)
library(ggnetwork)
library(ggplot2)
library(forcats)
library(scales)
library(rvest)
library(network)
library(sna)
library(wesanderson)
library(data.table)
library(viridis)

# Load data ####
setwd("E:/Globi/Data")

expert <- get(load("globi_cleaned_expert_taxized.Rdata"))
data <- expert
rm(expert)

# Create edge list from dataframe ####
data_edge <- data %>% select(source_binomial,target_binomial,interaction_type,S_Genus,T_Genus,latitude,longitude,S_Phylum,S_Kingdom,T_Phylum,T_Kingdom)

# Change factor classes
data_edge$S_Phylum <- as.factor(data_edge$S_Phylum)
data_edge$S_Kingdom <- as.factor(data_edge$S_Kingdom)
data_edge$T_Phylum <- as.factor(data_edge$T_Phylum)
data_edge$T_Kingdom <- as.factor(data_edge$T_Kingdom)

# Remove anything mentioning Anisoptera as these records are dubious (only 6 observations)
data_edge <- data_edge %>% filter(source_binomial!="Anisoptera")

# Create dataframe with binomials, phyla and kingdoms
phylo_S <- data_edge %>% select(source_binomial,S_Phylum,S_Kingdom)
phylo_T <- data_edge %>% select(target_binomial,T_Phylum,T_Kingdom)
phylo_S <- unique(phylo_S)
phylo_T <- unique(phylo_T)
phylo_S <- phylo_S %>% filter(!is.na(S_Phylum)) %>% filter(!is.na(S_Phylum))
phylo_T <- phylo_T %>% filter(!is.na(T_Phylum)) %>% filter(!is.na(T_Phylum))

# Clean up taxonomic naming issues
target_problems <- c("Cucumis sativus","Parthenocissus inserta","Prosopis glandulosa","Salix phylicifolia")
diagnosis <- phylo_T %>% filter(target_binomial %in% target_problems)
diagnosis <- diagnosis %>% filter(T_Phylum == "Magnoliophyta")
phylo_T <- phylo_T %>% filter(!target_binomial %in% target_problems)
phylo_T <- rbind(phylo_T,diagnosis)

source_problems <- c("Metzgeria fruticulosa")
diagnosis2 <- phylo_S %>% filter(source_binomial %in% source_problems)
diagnosis2 <- diagnosis2 %>% filter(S_Phylum != "Bryophyta")
phylo_S <- phylo_S %>% filter(!source_binomial %in% source_problems)
phylo_S <- rbind(phylo_S,diagnosis2)

# Aggregate data by binomials and interaction type - w will record number of times interaction occurs
data_edge$w <- 1
data_edge <- aggregate(w ~ source_binomial+target_binomial+interaction_type, data=data_edge, FUN=sum, na.rm=T)

# Merge with Phylum and Kingdom data
data_edge2 <- merge(data_edge,phylo_S,by="source_binomial",all.x = T)
data_edge2 <- merge(data_edge2,phylo_T,by="target_binomial",all.x = T)
data_edge <- data_edge2

# Remove all self ("cannibalistic") links
data_edge$source_binomial <- as.character(data_edge$source_binomial)
data_edge$target_binomial <- as.character(data_edge$target_binomial)
data_edge <- filter(data_edge, source_binomial != target_binomial)
data_edge$source_binomial <- as.factor(data_edge$source_binomial)
data_edge$target_binomial <- as.factor(data_edge$target_binomial)

# Reclassify interaction types into broader categories
data_edge$interaction_type <- fct_collapse(data_edge$interaction_type,
  Pollination_and_visitation = c("flowersVisitedBy","pollinatedBy","pollinates","visitedBy","visits","visitsFlowersOf"),
  Undefined = c("interactsWith","ecologicallyRelatedTo"),
  Trophic = c("eats","eatenBy","preyedUponBy","preysOn"),
  Adjacency = c("adjacentTo","coOccursWith","livesNear","livesUnder","livesInsideOf","livesOn"),
  Dispersal = c("dispersalVectorOf"),
  Oviposition = c("laysEggsOn"),
  Parasitism = c("endoparasiteOf","hasEctoparasite","hasEndoparasitoid","hasHost","hasParasite","hostOf","parasiteOf","parasitoidOf"),
  Pathogens = c("hasPathogen","hasVector","pathogenOf","vectorOf"),
  Other_mutualism = c("mutualistOf","symbiontOf")
)

# Create subsets for each interaction type
Pollination_and_visitation <- data_edge %>% filter(interaction_type=="Pollination_and_visitation")
Undefined <- data_edge %>% filter(interaction_type=="Undefined")
Trophic <- data_edge %>% filter(interaction_type=="Trophic")
Adjacency <- data_edge %>% filter(interaction_type=="Adjacency")
Dispersal <- data_edge %>% filter(interaction_type=="Dispersal")
#Oviposition <- data_edge %>% filter(interaction_type=="Oviposition") No oviposition recorded
Parasitism <- data_edge %>% filter(interaction_type=="Parasitism")
Pathogens <- data_edge %>% filter(interaction_type=="Pathogens")
Other_mutualism <- data_edge %>% filter(interaction_type=="Other_mutualism")

# Drop unused factor levels for each
Pollination_and_visitation <- droplevels(Pollination_and_visitation)
Undefined <- droplevels(Undefined)
Trophic <- droplevels(Trophic)
Adjacency <- droplevels(Adjacency)
Dispersal <- droplevels(Dispersal)
Parasitism <- droplevels(Parasitism)
Pathogens <- droplevels(Pathogens)
Other_mutualism <- droplevels(Other_mutualism)

# Create networks
Pollination_and_visitation_net <- network(Pollination_and_visitation[, 1:2 ], directed = TRUE)
Undefined_net <- network(Undefined[, 1:2 ], directed = TRUE)
Trophic_net <- network(Trophic[, 1:2 ], directed = TRUE)
Adjacency_net <- network(Adjacency[, 1:2 ], directed = TRUE)
Dispersal_net <- network(Dispersal[, 1:2 ], directed = TRUE)
#Oviposition_net <- network(Oviposition[, 1:2 ], directed = TRUE) # No oviposition recorded
Parasitism_net <- network(Parasitism[, 1:2 ], directed = TRUE)
Pathogens_net <- network(Pathogens[, 1:2 ], directed = TRUE)
Other_mutualism_net <- network(Other_mutualism[, 1:2 ], directed = TRUE)

# Attributes (variables) for the network
# Edge
Pollination_and_visitation_net %e% "interactions" <- Pollination_and_visitation$w
Undefined_net %e% "interactions" <- Undefined$w
Trophic_net %e% "interactions" <- Trophic$w
Adjacency_net %e% "interactions" <- Adjacency$w
Dispersal_net %e% "interactions" <- Dispersal$w
Parasitism_net %e% "interactions" <- Parasitism$w
Pathogens_net %e% "interactions" <- Pathogens$w
Other_mutualism_net %e% "interactions" <- Other_mutualism$w

# Vertex
# Freeman degree i.e. number of adjacent edges
Pollination_and_visitation_net %v% "degree" <- degree(Pollination_and_visitation_net)
Undefined_net %v% "degree" <- degree(Undefined_net)
Trophic_net %v% "degree" <- degree(Trophic_net)
Adjacency_net %v% "degree" <- degree(Adjacency_net)
Dispersal_net %v% "degree" <- degree(Dispersal_net)
Parasitism_net %v% "degree" <- degree(Parasitism_net)
Pathogens_net %v% "degree" <- degree(Pathogens_net)
Other_mutualism_net %v% "degree" <- degree(Other_mutualism_net)

# Kingdom and phylum for each vertex
A <- phylo_S
B <- phylo_T
colnames(A) <- c("binomial","Phylum","Kingdom")
colnames(B) <- c("binomial","Phylum","Kingdom")
phylo_all <- rbind(A,B)
phylo_all <- unique(phylo_all)

Pollination_and_visitation_net %v% "Phylum" <- as.character(phylo_all$Phylum)
Pollination_and_visitation_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)

Undefined_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Undefined_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Trophic_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Trophic_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Adjacency_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Adjacency_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Dispersal_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Dispersal_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Parasitism_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Parasitism_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Pathogens_net %v% "Kingdom" <- as.character(phylo_all$Kingdom)
Pathogens_net %v% "Phylum" <- as.character(phylo_all$Phylum)

Other_mutualism_net %v% "Kingdom"  <- as.character(phylo_all$Kingdom)
Other_mutualism_net %v% "Phylum"  <- as.character(phylo_all$Phylum)

# Generate colour palette
pl <- show_col(viridis_pal()(8))


# Plot networks
# Ignore warnings about "duplicated edges detected"
p_pollination <- ggplot(fortify(Pollination_and_visitation_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(alpha=interactions),color = "white") +
  geom_nodes(aes(color = Phylum, size = log10(degree))) + 
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")
p_pollination2 <- p_pollination + scale_colour_viridis_d(option="plasma",alpha=0.65)

p_undefined <- ggplot(fortify(Undefined_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(aes(color = Kingdom, size = log10(degree))) + 
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")

p_trophic <- ggplot(fortify(Trophic_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(aes(color = Kingdom, size = log10(degree))) + 
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")
ptrophic2 <- p_trophic + scale_colour_viridis_d(alpha=0.8)


p_adjacency <- ggplot(fortify(Adjacency_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(shape = 21, fill = "gold", color = "tomato") +
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")

p_dispersal <- ggplot(fortify(Dispersal_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(aes(color = Kingdom, size = log10(degree))) +
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")
pdispersal2 <- p_dispersal + scale_colour_viridis_d(alpha=0.8)



p_parasitism <- ggplot(fortify(Parasitism_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(shape = 21, fill = "gold", color = "tomato") +
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")

p_pathogens <- ggplot(fortify(Pathogens_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(shape = 21, fill = "gold", color = "tomato") +
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")

p_othermutualisms <- ggplot(fortify(Other_mutualism_net, arrow.gap = 0), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "white", alpha = 0.5) +
  geom_nodes(shape = 21, fill = "gold", color = "tomato") +
  guides(alpha = FALSE, size = FALSE) +
  theme_blank(14) +
  theme(rect = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10"),
        text = element_text(color = "white"),
        legend.position = "bottom")
