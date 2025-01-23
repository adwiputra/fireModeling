# Calculate average VPD until 2014
# 15/01/2025
# AD

# 1. libraries====
library(terra)
library(tidyverse)

# 2. inputs=====
monthlyVPD_dir <- "G:/chelsav2/GLOBAL/monthly/vpd/"
referenceRaster <- rast("D:/Documents/otherOpps/YSSP/projects/analysis/fireModeling/data_etc/NAsynced_reference.tif")
boundVector <- vect("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/spatial_scope_bound.shp")
# input_data <- lapply(input_data, FUN = function(x) { terra::wrap(x)})

# 3. preprocessing=====
# load all raster
vpdMonthly_raster <- list.files(monthlyVPD_dir, ".tif$", full.names = TRUE)
vpdMonthly_raster <- vpdMonthly_raster %>% grep("2015",.,  value = TRUE, invert = TRUE)
raster <- rast(vpdMonthly_raster)

# 4. PROCESSING========
# average all cells across the raster stack
meanVPD_climate <- mean(raster)
# export the global data
writeRaster(meanVPD_climate, "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/mean1980_2014_VPD_chelsa_v2_global.tif")

# Synchronize with reference raster
recMatrix <- as.matrix(data.frame(from = c(0, 1) , to = c(NA, 1)))
referenceRaster <- referenceRaster %>% terra::classify(., recMatrix)
meanVPD_climate <- meanVPD_climate %>% project(referenceRaster)
# crop
meanVPD_climate <- crop(meanVPD_climate, referenceRaster)
meanVPD_climate <- resample(meanVPD_climate, referenceRaster)
meanVPD_climate <- mask(meanVPD_climate, referenceRaster)
meanVPD_climate <- mask(meanVPD_climate, boundVector)
# sync