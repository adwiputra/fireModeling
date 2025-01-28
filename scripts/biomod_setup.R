# BIOMOD RUNS AND TESTING
# Script first created: 24/01/2025
# AD
# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)

# INPUTS======
mainDir <- "D:/AD/fire_analysis/biomodRun_ebro/"
firePoints <- paste0(mainDir, "ignitionSpread_machLearn_input.shp") %>% vect()
absencePoints <- paste0(mainDir, "bas_sample1.shp") %>% vect()
predictorDir <- paste0(mainDir, "na_synced_covariates_v2/")
# coreNumber <- detectCores() - 15
coreNumber <- 30-19
# Preprocessing=======

# Synchronize the coordinate reference systems
firePoints <- firePoints %>% project(absencePoints)
# listing all raster inside predictorDir
myExpl <- list.files(predictorDir, pattern = ".tif$", full.names = TRUE)
myExpl <- rast(myExpl)
# Specify or reclass the first two layers as factors
myExpl[[1]] <- terra::as.factor(myExpl[[1]])
myExpl[[2]] <- terra::as.factor(myExpl[[2]])


# Preprocessing points data
values(firePoints) <- values(firePoints) %>% mutate(presAbs_BL = 1)
values(absencePoints) <- values(absencePoints) %>% mutate(presAbs_BL = 0) %>% mutate(pointCateg = "absence")

# Construct compiled responses
myRespXY <- subset(firePoints, pointCateg == "ignition", c(pointCateg, presAbs_BL), NSE=TRUE)
myRespXY <- subset(absencePoints, pointCateg == "absence", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY)
# Extracting the boolean presence (1) / absence (0) values
myResp <- values(myRespXY) %>% select(presAbs_BL) %>% pull()
# Extract coordinates
myRespXY <- myRespXY %>% terra::geom(df = TRUE) %>% select(x, y)
# # Running the example codes from biomod2: https://biomodhub.github.io/biomod2/articles/examples_1_mainFunctions.html
# data("DataSpecies")
# head(DataSpecies)
# # Selecting the name of the species in foci
myRespName <- 'wildfire2015'
# 
# # Retreive corresponding presence/absence data
# myResp <- as.numeric(DataSpecies[, myRespName])
# 
# # Obtain corresponding XY coordinates
# myRespXY <- DataSpecies[, c('X_WGS84', 'Y_WGS84')]
# 
# # Load environmental variables
# data("bioclim_current")
# myExpl <- rast(bioclim_current)


# Format Data with true absences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     filter.raster = TRUE)
myBiomodData
plot(myBiomodData)

# explicitly define categorical variables as categorical.
# as.factor(spatRaster)


# Processing======
# Registering do parallel backend
registerDoParallel(cores = coreNumber) # cores = 10 crashed the whole computer
# Run the model
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    modeling.id = 'Example',
                                    models = c('RFd'),
                                    CV.strategy = 'random',
                                    CV.nb.rep = 10,
                                    CV.perc = 0.7,
                                    OPT.strategy = 'bigboss',
                                    metric.eval = c('TSS', 'ROC'),
                                    var.import = 3,
                                    seed.val = 42,
                                    nb.cpu = coreNumber)
# Obtain evaluation scores
evaluationScores <- get_evaluations(myBiomodModelOut)
# Extracting variable importance
variable_importance <- get_variables_importance(myBiomodModelOut)

# Saving
save.image(file = paste0(mainDir, "correctedRun.RData"))

# plotting response curves
mods <- get_built_models(myBiomodModelOut, run = 'allRun')
bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = mods,
                      fixed.var = 'median')


# Retreiving the projection map
myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                  proj.name = 'first successful run',
                                  new.env = myExpl,
                                  models.chosen = 'wildfire2015_allData_allRun_RFd',
                                  nb.cpu = coreNumber,
                                  seed.val = 42)
writeRaster(myBiomodProj, paste0(mainDir, "/allDataRun_RFd_bigBoss.tif"))