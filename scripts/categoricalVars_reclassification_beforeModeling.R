# Figure 4. Response Curves of Predictors
# 03/03/2025
# AD

# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)
library(data.table)

# a. Version 2-----
# 2. INPUTS=====
landCoverDir <- "F:/storage/projects/nus07_fire/analysis/data/spatial/covariates/landCover_CCI/"
landCover_raster <- "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/dominantLandCover.tif" %>% rast() %>% as.factor()
landCover_freq <- landCover_raster %>% freq()
# Ecoregion raster attribute table
ecoregion_raster <- "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/Ecoregion.tif" %>% rast() %>% as.factor()
ecoregion_freq <- ecoregion_raster %>% freq()
# Reduced ecoregion vector layer to extract the attribute table
ecoregion_lookup <- "F:/storage/projects/nus07_fire/analysis/finalized_materials/reduced_ecoregion.shp" %>% vect() %>% values()

# 3. PREPROCESSING======
# Prepping landCover_lookup
landCover_lookup <- paste0(landCoverDir, "LC_class_lookUp.csv") %>% read.csv()
landCover_lookup <- landCover_lookup %>% mutate(landClass = paste0(landCover_lookup[, 2], " ", landCover_lookup[, 3], " ", landCover_lookup[, 4])) %>% mutate(landClass = gsub("^\\s+|\\s+$", "", landClass))
landCover_lookup <- landCover_lookup %>% select(Value, landClass)
# Indexing the landCover_freq to join with the landCover_lookup
landCover_freq <- landCover_freq %>% mutate(value = as.factor(value)) %>% rename(Value = value) %>% select(-c(layer, count)) %>% mutate(expl.val = as.numeric(Value))
# joining landCover_freq and landCover_lookup
landCover_lookup <- landCover_lookup %>% mutate(Value = as.factor(Value)) %>% right_join(landCover_freq)
# Save landCover_lookup as reference for interpretation later
write.csv(landCover_lookup, "F:/storage/projects/nus07_fire/analysis/finalized_materials/landCover_lookup.csv", row.names = FALSE)
#ADhere
# Cleaning ecoreg_lookup
ecoregion_lookup <- ecoregion_lookup %>% select(newECONAME, rasterVal) %>% rename(value = rasterVal)
# join with the freq table
ecoregion_lookup <- ecoregion_freq %>% mutate(value = as.numeric(value)) %>% left_join(ecoregion_lookup) %>% mutate(expl.val = as.numeric(as.factor(value)))
# Save landCover_lookup as reference for interpretation later
write.csv(ecoregion_lookup, "F:/storage/projects/nus07_fire/analysis/finalized_materials/ecoregion_lookup.csv", row.names = FALSE)
# 4. PROCESSING======
# reclassify land cover raster to match the expl.val as specified in landCover_lookup
reclassification_matrix <- landCover_lookup %>% select(Value, expl.val) %>% mutate(Value = as.numeric(as.character(Value))) %>% mutate(expl.val = as.numeric(expl.val)) %>% as.matrix()
landCover_raster <- landCover_raster %>% terra::classify(reclassification_matrix)
# Export raster
writeRaster(landCover_raster, "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/dominantLandCover_v2.tif")
# reclassify ecoregion raster to match the expl.val as specified in ecoregion_lookup
reclassification_matrix <- ecoregion_lookup %>% select(value, expl.val) %>% as.matrix()
ecoregion_raster <- ecoregion_raster %>% terra::classify(reclassification_matrix)
# Export raster
writeRaster(ecoregion_raster, "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/Ecoregion_v2.tif")

# b. Version 3=====
# INPUTS=====
# landCover_raster v2
landCover_raster <- rast("F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/dominantLandCover_v2.tif")
# landCover_lookup v2
landCover_lookup <- read.csv("F:/storage/projects/nus07_fire/analysis/finalized_materials/landCover_lookup.csv") %>%
  mutate(v3.val = expl.val-1+10)
# ecoregion_raster v2
ecoregion_raster <- rast("F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/Ecoregion_v2.tif")
# ecoregion_lookup v2
ecoregion_lookup <- read.csv("F:/storage/projects/nus07_fire/analysis/finalized_materials/ecoregion_lookup.csv") %>%
  mutate(v3.val = expl.val-1+10)
# reclassify landCover raster to match the v3.val as specified in landCover_lookup.
# at this point, interpretation should be based on the expl.val which is expected
# to be the factor level.
reclassification_matrix <- landCover_lookup %>% select(expl.val, v3.val) %>% as.matrix()
landCover_raster <- landCover_raster %>% terra::classify(reclassification_matrix) %>% terra::as.factor()
# Export raster
writeRaster(landCover_raster, "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/dominantLandCover_v3.tif")

# Repeat the same steps for ecoregion_raster
reclassification_matrix <- ecoregion_lookup %>% select(expl.val, v3.val) %>% as.matrix()
ecoregion_raster <- ecoregion_raster %>% terra::classify(reclassification_matrix) %>% terra::as.factor()
# Export raster
writeRaster(ecoregion_raster, "F:/storage/projects/nus07_fire/analysis/finalized_materials/na_synced_covariates_v2/Ecoregion_v3.tif")
