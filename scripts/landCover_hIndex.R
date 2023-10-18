# Script to generate the Shannon heterogeneity index for land cover in the study area
# AD
# first created: 05/07/2023
# revised: 18/10/2023

# 0. libraries======
library(tidyverse)
library(sf)

# 1. Input=====
dataDir <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/"
# Prepping landCover_lookup
landCover_lookup <- paste0(dataDir, "LC_class_lookUp.csv") %>% read.csv()
landCover_lookup <- landCover_lookup %>% mutate(landClass = paste0(landCover_lookup[, 2], " ", landCover_lookup[, 3], " ", landCover_lookup[, 4])) %>% mutate(landClass = gsub("^\\s+|\\s+$", "", landClass))
landCover_lookup <- landCover_lookup %>% select(Value, landClass)

outputDir <- paste0(dataDir, "preprocessing/")

gridded_table <- paste0(outputDir, "gridID_lc14_intersect.csv") %>% read.csv()


# 2. Processing====
# Summarize by grid ID and test if the total area is equal to the supposed area (1 sq. km)
# is the 'Shape_Area' unit in square meters?
# summarize by GRID_ID to calculate the N (denominator), which is defined by the total land area contained within the corresponding Grid
totalArea_GRIDs <- gridded_table %>% group_by(GRID_ID) %>% summarize(gridArea = sum(Shape_Area))
# identify the values that corresponds to Water bodies in the lookup table
waterID <- landCover_lookup %>% filter(grepl(pattern = "Water", x = landClass)) %>% select(Value) %>% pull() %>% as.numeric()
water_n_NA_IDs <- c(waterID, 0, 255)
totalLand_GRIDs <- gridded_table %>% filter(!gridcode %in% water_n_NA_IDs) %>% group_by(GRID_ID) %>% summarize(landArea = sum(Shape_Area))
# omit grids with < 50 % land area
totalLand_GRIDs <- totalLand_GRIDs %>% left_join(totalArea_GRIDs) %>% mutate(landProp = landArea/gridArea)
# landMajor_GRIDs <- totalLand_GRIDs %>% filter(landProp > 0.5) # only include grids with more than half of its area on land
# landMajor_GRIDs <- totalLand_GRIDs %>% filter(landArea > (0.5*max(totalLand_GRIDs$gridArea)))# only include grids with more than half of maximum potential grid area on land
# # calculate relative area proportion per land cover grid ID
# landMajor_GRIDs <- landMajor_GRIDs %>% left_join(gridded_table, by = "GRID_ID") %>% mutate(classProp = Shape_Area/landArea) %>% filter(!gridcode %in% water_n_NA_IDs)
# # calculate Shannon Index and the majority class
# shannon_GRIDs <- landMajor_GRIDs %>% group_by(GRID_ID) %>% summarize(H = abs(sum(classProp* log(classProp))))
# dominantArea_prop <- landMajor_GRIDs %>% group_by(GRID_ID) %>% summarize(dominantArea = max(classProp))
# majorityClass_GRIDs <- landMajor_GRIDs %>% left_join(dominantArea_prop, by = "GRID_ID") %>% filter(classProp == dominantArea) %>% select(GRID_ID, gridcode, classProp)
# # check if there is any grid ids with more than 1 majority class
# tieArea_GRIDs <- majorityClass_GRIDs %>% filter(duplicated(majorityClass_GRIDs$GRID_ID))

# just realized that there are pixels that follow the regional ids while some only follow the global id, for simplicity, we stick to the global id
gridded_table_global <- gridded_table %>% mutate(gridcode = gridcode %/% 10 * 10) %>% ungroup() %>% group_by(GRID_ID, gridcode) %>% summarize(Shape_Area = sum(Shape_Area))
# redoing the proportion calculation and forth:
# calculate Shannon Index and the majority class
landMajor_GRIDs <- totalLand_GRIDs %>% filter(landArea > (0.5*max(totalLand_GRIDs$gridArea)))# only include grids with more than half of maximum potential grid area on land
landMajor_GRIDs <- landMajor_GRIDs %>% left_join(gridded_table_global, by = "GRID_ID") %>% mutate(classProp = Shape_Area/landArea) %>% filter(!gridcode %in% water_n_NA_IDs)
# 
shannon_GRIDs <- landMajor_GRIDs %>% group_by(GRID_ID) %>% summarize(H = abs(sum(classProp* log(classProp))))
dominantArea_prop <- landMajor_GRIDs %>% group_by(GRID_ID) %>% summarize(dominantArea = max(classProp))
majorityClass_GRIDs <- landMajor_GRIDs %>% left_join(dominantArea_prop, by = "GRID_ID") %>% filter(classProp == dominantArea) %>% select(GRID_ID, gridcode, classProp)
# 
tieArea_GRIDs <- majorityClass_GRIDs %>% filter(duplicated(majorityClass_GRIDs$GRID_ID))
# reconciling the grids by selecting a random land cover class that are vegetated (180 >=) if and only if there is no class id 190 (artificial surfaces) 
tieArea_GRIDs <- tieArea_GRIDs %>% select(GRID_ID) %>% unique() %>% left_join(majorityClass_GRIDs)
GRIDs_withArtificial <- tieArea_GRIDs %>% filter(gridcode == 190) %>% select(GRID_ID) %>% pull()
tieArea_GRIDs <- tieArea_GRIDs %>% filter(!GRID_ID %in% GRIDs_withArtificial) %>% group_by(GRID_ID) %>% summarize(gridcode = sample(gridcode, 1), classProp = mean(classProp))
# compile the reconciled tied GRIDs
reconciled_tiedGRIDs <- data.frame(GRID_ID = GRIDs_withArtificial) %>% mutate(gridcode = 190) %>% left_join(majorityClass_GRIDs) %>% bind_rows(tieArea_GRIDs)
# merge back to the majorityClass_GRIDs
majorityClass_GRIDs <- majorityClass_GRIDs %>% filter(!GRID_ID %in% reconciled_tiedGRIDs$GRID_ID) %>% bind_rows(reconciled_tiedGRIDs)
# 3. Exporting =====
# Not joining with the relevant vector layer here because the file size is too big. the Vector is to be "Lookup" in ArcGIS to generate rasters of majority land cover (categorical) and the H index (continuous)
outputTable <- majorityClass_GRIDs %>% left_join(shannon_GRIDs)
# export
outputTable %>% write.csv(paste0(outputDir, "hIndex_majority_perGrid_v2.csv"))
