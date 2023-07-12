# Script to run Maximum Entropy modeling through ENMeval for interpolation of ignitialPoint suitable location.
# first written: 13/07/2023
# AD
options(java.parameters = "-Xmx40024m")
# 0. LIBRARIES=======
library(ENMeval)
library(raster)
library(sf)
library(dplyr)


# 1. INPUTS=======
dataDir <- "C:/Users/dwiputra/Documents/analysis/maxEnt_run/data/"
occurrenceData <- paste0(dataDir, "pointAttributes_all.csv") %>% read.csv()
covariatesDir <- paste0(dataDir, "synced_covariates/")
testProp <- 0.2

# 2. PREPROCESSING========
set.seed(141) # enforce reproducibility
# a. partition the training vs. testing data for both the occurrence and background
# here we would like to have fully withheld testing data, which allows for a less biased evaluation of the model's transferability. Can afford this because we have quite plenty dataset.

occurrenceData <- occurrenceData %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% st_transform(crs = 3395)# epsg 3395 represents WGS 84/World Mercator
# extract spreadPoints
spreadPoints <- occurrenceData %>% filter(pointCategory == "spread") %>% st_coordinates()
# extract ignitial points
ignitialPoints <- occurrenceData %>% filter(pointCategory == "ignitial")
# sample the set aside
testIgnitials <- round(testProp * nrow(ignitialPoints))
testIgnitials <- ignitialPoints %>% sample_n(size = testIgnitials, replace = FALSE)
# define the trainIgnitials
trainIgnitials <- ignitialPoints %>% setdiff(testIgnitials)
# convert trainIgnitials and testIgnitials into coordinates
trainIgnitials <- trainIgnitials %>% st_coordinates()
testIgnitials <- testIgnitials %>% st_coordinates()
# b. generate raster stack
covariate_files <- list.files(covariatesDir, pattern = ".tif$", full.names = TRUE)
covariates <- raster::stack(covariate_files)
# define the categorical layers # ADhardcode
covariates$ecoreg <- raster::as.factor(covariates$ecoreg)
covariates$gridcode_landCover <- raster::as.factor(covariates$gridcode_landCover)
covariates$peaNonPea <- raster::as.factor(covariates$peaNonPea)
# setExtent may be required here
# specify the categorical variables

# 3. PROCESSING========
# Calibration
ignitial_maxentModel <- ENMevaluate(occs = trainIgnitials, envs = covariates, bg = spreadPoints, algorithm = "maxent.jar", partitions = "testing", occs.testing = testIgnitials, parallel = TRUE, numCores = 95, tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))
# Validation
# Best model selection
# 4. POST-PROCESSING======
# Plotting response curves
# 
