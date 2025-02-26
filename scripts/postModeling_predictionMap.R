# Calculate average VPD until 2014
# 15/01/2025
# AD

# 1. libraries====
library(terra)
library(tidyverse)

# 2. inputs=====
rData_dir_nTree500 <- "D:/Documents/research/projects/nus07_fire/analysis/output/falconRun/wildfire2015/"
outputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
# monthlyVPD_dir <- "G:/chelsav2/GLOBAL/monthly/vpd/"
# referenceRaster <- rast("D:/Documents/otherOpps/YSSP/projects/analysis/fireModeling/data_etc/NAsynced_reference.tif")
# boundVector <- vect("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/spatial_scope_bound.shp")
# input_data <- lapply(input_data, FUN = function(x) { terra::wrap(x)})

# 3. preprocessing=====
# load all raster
fireOriginLikelihood_raster <- list.files(rData_dir_nTree500, ".tif$", full.names = TRUE, recursive = TRUE)
fireOriginLikelihood_raster <- fireOriginLikelihood_raster %>% grep("Clamp",.,  value = TRUE, invert = TRUE) %>% grep(" ",.,  value = TRUE, invert = TRUE) 
raster <- rast(fireOriginLikelihood_raster)

# 4. PROCESSING========
# average all cells across the raster stack
median_fireOriginLikelihood <- median(raster)
# export the global data
writeRaster(median_fireOriginLikelihood, paste0(outputDir, "median_fireOriginLikelihood_nTree500.tif"))
