# Tiny script to sync the NA cells across all of the covariates
# first created 18/07/2023

# 0. load packages
library(terra)
library(tidyverse)

# 1. INPUTS======
# a. Ecoregion
a_path <- "D:/AD/fire_analysis/temp_transfer/covariates/ecoregID_raster.tif"
# b. VPD
b_path <- "D:/AD/fire_analysis/temp_transfer/covariates/mean80_2014_vpd_chelsa2.tif" # already in World Mercator coordinate reference system
# c. travelTime
c_path <- "D:/AD/fire_analysis/temp_transfer/covariates/travelTime_2015_merc.tif"
# d. human footprint index
d_path <- "D:/AD/fire_analysis/temp_transfer/covariates/humanFootprint_2014.tif"
# e. NASADEM
e_path <- "D:/AD/fire_analysis/temp_transfer/covariates/nasaDEM_zonal_average_falcon.tif"
# f. distance to water bodies and canals
f_path <- "D:/AD/fire_analysis/temp_transfer/covariates/waterDist_averaged_grid.tif"
# g. dominant landcover
g_path <- "D:/AD/fire_analysis/temp_transfer/covariates/landDiversity.tif"
# h. landcover diversity
h_path <- "D:/AD/fire_analysis/temp_transfer/covariates/dominantLand.tif"
# i. EXTRA gridID_raster_exclusive
i_path <- "D:/AD/fire_analysis/temp_transfer/gridID_raster_exclusive.tif"
# Output directory
output_dir <- "F:/temp_transfer/na_synced_covariates/"
if(!dir.exists(output_dir))dir.create(output_dir)
# preprocessing====
# Generate the minimum extent of raster to crop all inputs with
minX <- eval(parse(text= paste0("max(", paste0("xmin(rast(", paste0(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), "_path)"), ")", collapse = ","), ")")))
minY <- eval(parse(text= paste0("max(", paste0("ymin(rast(", paste0(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), "_path)"), ")", collapse = ","), ")")))
maxX <- eval(parse(text= paste0("min(", paste0("xmax(rast(", paste0(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), "_path)"), ")", collapse = ","), ")")))
maxY <- eval(parse(text= paste0("min(", paste0("ymax(rast(", paste0(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), "_path)"), ")", collapse = ","), ")")))
ref_extent <- ext(minX, maxX, minY, maxY)
covariate_rasterStack <- eval(parse(text = paste0("c(", paste0("crop(rast(", paste0(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), "_path)"), ", ref_extent)", collapse = ", "), ")")))

# processing=========
# Calculate mean to obtain layers with accummulated NAs
meanVPD_covariates <- terra::mean(covariate_rasterStack, na.rm = FALSE)
meanVPD_covariates <- meanVPD_covariates/meanVPD_covariates
# apply the na harmonization to the rasterStack
covariate_rasterStack <- covariate_rasterStack * meanVPD_covariates
# extract the raster names of all raster for export
exportRasterLayer_names <- c("Ecoregion", "VPD", "travelTime_2015", "humanFootprint_2014", "nasaDEM", "waterDist", "landDiversity", "dominantLandCover", "GRID_ID")
# append output_dir into the exportRasterLayer_names
exportRasterLayer_names <- paste0(output_dir, exportRasterLayer_names, ".tif")
# define datatypes
datatypes <- c("INT2U", "FLT4S", "INT4S", "FLT4S", "INT4S", "INT4S", "FLT4S", "INT2U", "INT4S")
# export raster per raster type
# INT2U
writeRaster(covariate_rasterStack[[which(datatypes == "INT2U")]], exportRasterLayer_names[which(datatypes == "INT2U")], overwrite = TRUE, datatype = "INT2U")
# FLT4S
writeRaster(covariate_rasterStack[[which(datatypes == "FLT4S")]], exportRasterLayer_names[which(datatypes == "FLT4S")], overwrite = TRUE, datatype = "FLT4S")
# INT4S
writeRaster(covariate_rasterStack[[which(datatypes == "INT4S")]], exportRasterLayer_names[which(datatypes == "INT4S")], overwrite = TRUE, datatype = "INT4S")
# writeRaster(covariate_rasterStack, exportRasterLayer_names, overwrite = TRUE, datatype = datatypes)
