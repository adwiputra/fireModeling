# Merging the hotspot points from VIIRS and MODIS
# AD
# 08/01/2025

# 0. LIBRARIES======
library(terra)
library(tidyverse)

# 1. INPUTS======
firms_download_dir <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/"
viirs_shp <- vect(paste0(firms_download_dir, "fire_archive_SV-C2_359610.shp"))
modis_shp <- vect(paste0(firms_download_dir, "fire_archive_M-C61_359608.shp"))

# 2. Assess the compatibility of merging the two datasets
merge_shp <- viirs_shp %>% rbind(modis_shp)
# the merge went successfully, check the consistency
# AlreaDyRun
# modis_postMerge <- subset(merge_shp, merge_shp$INSTRUMENT != "VIIRS")  # 419512, which is equal to the originial record
# values(modis_postMerge) %>% identical(values(modis_shp)) # false, because the confidence values are now character instead of integer.

# 3. Export
merge_shp %>% writeVector("D:/Documents/research/projects/nus07_fire/data/modis_viirs_mergedInR.shp")
