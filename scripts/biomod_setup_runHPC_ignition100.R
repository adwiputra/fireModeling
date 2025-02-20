# BIOMOD RUNS AND TESTING
# Script first created: 24/01/2025 modified 18/02/2025
# AD
# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)

# Documenting time start
# t_start <- Sys.time()
# INPUTS======
# mainDir <- "D:/AD/fire_analysis/biomodRun_ebro/"
# setwd("/hpctmp//hpctmp/e0915700/")
mainDir <- "inputs/"
firePoints <- paste0(mainDir, "ignitionSpread_machLearn_input.shp") %>% vect()
absencePoints_dir <- paste0(mainDir, "basSampled_absences/")
absencePoints <- paste0(absencePoints_dir, "bas_sample1.shp") %>% vect()
predictorDir <- paste0(mainDir, "na_synced_covariates_v2/")

# Setting up the biomod2 parameters
user.RFd <- list("for_all_datasets" = list(
  type = "classification",
  ntree = 1000,
  mtry = 2,
  strata = factor(c(0, 1)),
  sampsize = NULL,
  nodesize = 10,
  maxnodes = NULL
))
# coreNumber <- detectCores() - 3 # original 19
coreNumber <- detectCores() - 10 # Run on parallel24 with 2 nodes (48) to address memory exceeded issue

# Preprocessing=======
# Synchronize the coordinate reference systems
firePoints <- firePoints %>% project(absencePoints)
values(firePoints) <- values(firePoints) %>% mutate(presAbs_BL = 1)
# listing all raster inside predictorDir
myExpl <- list.files(predictorDir, pattern = ".tif$", full.names = TRUE)
myExpl <- rast(myExpl)
# Specify or reclass the first two layers as factors
myExpl[[1]] <- terra::as.factor(myExpl[[1]])
myExpl[[2]] <- terra::as.factor(myExpl[[2]])

# Registering do parallel backend
registerDoParallel(cores = coreNumber) # cores = 10 crashed the whole computer

    # LOOP PER ABSENCE POINT SET
    for(i in 1:10){
      # load the specified absence points
      absencePoints <- paste0(absencePoints_dir, "bas_sample", i, ".shp") %>% vect()
      values(absencePoints) <- values(absencePoints) %>% mutate(presAbs_BL = 0) %>% mutate(pointCateg = "absence")
      # Construct compiled responses
      myRespXY <- subset(absencePoints, pointCateg == "absence", c(pointCateg, presAbs_BL), NSE=TRUE)
      myRespXY <- subset(firePoints, pointCateg != "absence", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY)
      # myRespXY <- subset(firePoints, pointCateg == "ignition", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY) # this line is used instead of the above line when loop is run outside the C loop
      # Extracting the boolean presence (1) / absence (0) values
      myResp <- values(myRespXY) %>% mutate(presAbs_BL = as.factor(presAbs_BL)) %>% select(presAbs_BL) %>% pull()
      # Extract coordinates
      myRespXY <- myRespXY %>% terra::geom(df = TRUE) %>% select(x, y)
      # # Running the example codes from biomod2: https://biomodhub.github.io/biomod2/articles/examples_1_mainFunctions.html
      # data("DataSpecies")
      # head(DataSpecies)
      # # Selecting the name of the species in foci
      myRespName <- 'wildfire'
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
      # myBiomodData
      # plot(myBiomodData)
      
      # explicitly define categorical variables as categorical.
      # as.factor(spatRaster)
      
      
      # Processing======
      # Run the model
      myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                          # modeling.id = paste0('sensitivity_', c, '_abs_', i, '_sim_', r),
                                          modeling.id = paste0('ADmod_fullIgnition_abs_', i),
                                          models = c('RFd'),
                                          CV.strategy = 'random',
                                          CV.nb.rep = 10, # set to 10 in non-monte carlo runs
                                          CV.perc = 0.7,
                                          # OPT.strategy = 'bigboss',
                                          OPT.strategy = 'user.defined',
                                          OPT.user.val = user.RFd,
                                          metric.eval = c('TSS', 'ROC'),
                                          var.import = 3,
                                          seed.val = 42,
                                          nb.cpu = coreNumber)
      # Obtain evaluation scores
      evaluationScores <- get_evaluations(myBiomodModelOut)
      # Extracting variable importance
      variable_importance <- get_variables_importance(myBiomodModelOut)
      
      # plotting response curves
      mods <- get_built_models(myBiomodModelOut, run = 'allRun')
      respCurves <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                            models.chosen = mods,
                            fixed.var = 'median')
      
      
      # Retreiving the projection map
      myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                        proj.name = paste0("ADmod_ignitionPct100_absenceSet_", i),
                                        new.env = myExpl,
                                        models.chosen = "wildfire_allData_allRun_RFd", # allRun refers to the fact that the selected model is the one that uses all data
                                        nb.cpu = coreNumber,
                                        seed.val = 42)
      
      # Saving
      # save.image(file = paste0(mainDir, "correctedRun_", i, ".RData"))
      save.image(file = paste0("ADmod_ignitionPct100_absenceSet", i, ".RData"))#mainDir, omitted for running at HPC
      gc()
    } # i Loop ends

doParallel::stopImplicitCluster()
# t_end <- Sys.time()
# The following lines contain the same routines outside of loop===========
# # Construct compiled responses
# myRespXY <- subset(absencePoints, pointCateg == "absence", c(pointCateg, presAbs_BL), NSE=TRUE) 
# myRespXY <- subset(firePoints, pointCateg == "ignition", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY)
# # Extracting the boolean presence (1) / absence (0) values
# myResp <- values(myRespXY) %>% select(presAbs_BL) %>% pull()
# # Extract coordinates
# myRespXY <- myRespXY %>% terra::geom(df = TRUE) %>% select(x, y)
# # # Running the example codes from biomod2: https://biomodhub.github.io/biomod2/articles/examples_1_mainFunctions.html
# # data("DataSpecies")
# # head(DataSpecies)
# # # Selecting the name of the species in foci
# myRespName <- 'wildfire'
# # 
# # # Retreive corresponding presence/absence data
# # myResp <- as.numeric(DataSpecies[, myRespName])
# # 
# # # Obtain corresponding XY coordinates
# # myRespXY <- DataSpecies[, c('X_WGS84', 'Y_WGS84')]
# # 
# # # Load environmental variables
# # data("bioclim_current")
# # myExpl <- rast(bioclim_current)
# 
# 
# # Format Data with true absences
# myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
#                                      expl.var = myExpl,
#                                      resp.xy = myRespXY,
#                                      resp.name = myRespName,
#                                      filter.raster = TRUE)
# myBiomodData
# plot(myBiomodData)
# 
# # explicitly define categorical variables as categorical.
# # as.factor(spatRaster)
# 
# 
# # Processing======
# # Registering do parallel backend
# registerDoParallel(cores = coreNumber) # cores = 10 crashed the whole computer
# # Run the model
# myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
#                                     modeling.id = 'Example',
#                                     models = c('RFd'),
#                                     CV.strategy = 'random',
#                                     CV.nb.rep = 10,
#                                     CV.perc = 0.7,
#                                     OPT.strategy = 'bigboss',
#                                     metric.eval = c('TSS', 'ROC'),
#                                     var.import = 3,
#                                     seed.val = 42,
#                                     nb.cpu = coreNumber)
# # Obtain evaluation scores
# evaluationScores <- get_evaluations(myBiomodModelOut)
# # Extracting variable importance
# variable_importance <- get_variables_importance(myBiomodModelOut)
# 

# 
# # plotting response curves
# mods <- get_built_models(myBiomodModelOut, run = 'allRun')
# bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
#                       models.chosen = mods,
#                       fixed.var = 'median')
# 
# 
# # Retreiving the projection map
# myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
#                                   proj.name = 'all_ignition_absenceSet_1',
#                                   new.env = myExpl,
#                                   models.chosen = 'wildfire_allData_allRun_RFd',
#                                   nb.cpu = coreNumber,
#                                   seed.val = 42)
# # Saving
# save.image(file = paste0(mainDir, "correctedRun.RData"))