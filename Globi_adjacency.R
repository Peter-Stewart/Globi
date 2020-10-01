# Packages ####
library(dplyr)
library(circlize)
library(igraph)

# Load data ####
setwd("E:/Globi/Data")
opuntia <- get(load("globi_cleaned_opuntia.Rdata"))

# Subset to interactions involving Opuntia stricta ####
stricta <- opuntia %>% filter(S_Species == "stricta" | T_Species == "stricta")

# Create edge list from dataframe ####
stricta_edge <- stricta %>% select(source_binomial,target_binomial)
stricta_edge <- as.matrix(stricta_edge)

# Transform edge list format to adjacency matrix
g=graph.data.frame(stricta_edge)
stricta_adj <- get.adjacency(g,sparse=FALSE)
