# Tiny script to sync the NA cells across all of the covariates
# first created 18/07/2023

# 0. load packages
library(terra)
library(tidyverse)

# 1. INPUTS
covariateDir <- "D:/Documents/otherOpps/YSSP/projects/analysis/fireModeling/data_etc/"
referenceRaster <- "D:/Documents/otherOpps/YSSP/projects/analysis/fireModeling/data_etc/NAsynced_reference.tif"

covariateRaster <- list.files(covariateDir, pattern = ".tif$", full.names = TRUE) %>% setdiff(referenceRaster) %>% as.list()
recMatrix <- as.matrix(data.frame(from = c(0, 1) , to = c(NA, 1)))
referenceRaster <- referenceRaster %>% rast() %>% terra::classify(., recMatrix)
# 2. PROCESS
naSync_raster <- function(inRaster_file = character()){
  inRaster <- inRaster_file %>% rast()
  syncedRaster <- inRaster * referenceRaster
  writeRaster(syncedRaster, gsub(".tif", "_synced.tif", inRaster_file))
}

lapply(covariateRaster, naSync_raster)
