# Script to investigate the potential intersection between the mapped line version of the network edges
# with the waterbodies
# AD
# 11/04/2024

# 0. libraries======
library(terra)
library(tidyverse)

# 1. INPUTS======
rds_directory <- "D:/Documents/research/projects/nus07_fire/data/fire_network_rds/"
intersect_files <- list.files(rds_directory, "intersect", full.names = TRUE)

# 2. PREPROCESSING====
init_rds <- intersect_files[1] %>% read_rds() %>% unwrap()
for(r in 2:length(intersect_files)){
  add_rds <- intersect_files[r] %>% read_rds() %>% unwrap()
  init_rds <- c(init_rds, add_rds)
}
init_rds <- vect(init_rds)
length(init_rds)
# 0 means that there is no edges that traverses across major waterbodies