# Script to run Maximum Entropy modeling through ENMeval for interpolation of ignitialPoint suitable location.
# first written: 13/07/2023
# AD
options(java.parameters = "-Xmx90024m")
# 0. LIBRARIES=======
library(ENMeval)
library(raster)
library(sf)
library(dplyr)


# 1. INPUTS=======
dataDir <- "data/"
occurrenceData <- paste0(dataDir, "ignitial_cleanTable.csv") %>% read.csv()
backgroundData <- paste0(dataDir, "spread_cleanTable.csv") %>% read.csv()
testProp <- 0.3

# 2. PREPROCESSING========
set.seed(141) # enforce reproducibility
# define the categorical layers # ADhardcode
occurrenceData$ecoreg <- as.factor(occurrenceData$ecoreg)
occurrenceData$lcDom <- as.factor(occurrenceData$lcDom)
occurrenceData$peat <- as.factor(occurrenceData$peat)
# same for the backgroundData
backgroundData$ecoreg <- as.factor(backgroundData$ecoreg)
backgroundData$lcDom <- as.factor(backgroundData$lcDom)
backgroundData$peat <- as.factor(backgroundData$peat)
# a. partition the training vs. testing data for both the occurrence and background
# here we would like to have fully withheld testing data, which allows for a less biased evaluation of the model's transferability. Can afford this because we have quite plenty dataset.
# sample the set aside
testIgnitials <- round(testProp * nrow(occurrenceData))
testIgnitials <- occurrenceData %>% sample_n(size = testIgnitials, replace = FALSE)
# define the trainIgnitials
trainIgnitials <- occurrenceData %>% setdiff(testIgnitials)

# 3. PROCESSING========
# Calibration
ignitial_maxentModel <- ENMevaluate(occs = trainIgnitials, bg = backgroundData, algorithm = "maxnet", partitions = "testing", occs.testing = testIgnitials, categoricals = c("peat", "ecoreg", "lcDom"), tune.args = list(fc = c("L","LQ"), rm = c(0.7, 0.8, 1:5))) # parallel = TRUE, numCores = 52, fc = c("LQH","H")
# Validation
# Best model selection
# 4. POST-PROCESSING======
# Plotting response curves
# 
