# BIOMOD RUNS AND TESTING
# Script first created: 24/01/2025
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
# coreNumber <- detectCores() - 3 # original 19
# coreNumber <- detectCores() - 5 # Run on parallel24 with 2 nodes (48) to address memory exceeded issue
coreNumber <- detectCores() - 10
# the ignition to total input ratio
ignitionContent <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) # 1 has been run a priori 
N_input <- 75424 # fixed
# total replicates of monte carlo simulation
n_replications <- 100
# seed_r <- sample(1:1000, n_replications) # the seed number for each monte carlo simulation # Below is the sample obtained after running this line
seed_r <- c(766,165,638,185,777,301,424,622,291,35,844,59,772,477,203,616,223,566,737,588,389,482,288,155,693,919,836,119,140,582,605,700,415,92,195,77,339,830,875,104,618,285,943,188,870,947,136,126,915,816,760,392,565,36,597,287,639,435,751,379,992,669,702,133,955,445,167,289,937,172,885,784,214,430,538,450,271,204,382,732,715,745,37,121,954,685,799,441,684,779,143,196,135,858,753,624,89,307,572,173)
# coreNumber <- 30-19
# Preprocessing=======
# Synchronize the coordinate reference systems
firePoints <- firePoints %>% project(absencePoints)
# listing all raster inside predictorDir
myExpl <- list.files(predictorDir, pattern = ".tif$", full.names = TRUE)
myExpl <- rast(myExpl)
# Specify or reclass the first two layers as factors
myExpl[[1]] <- terra::as.factor(myExpl[[1]])
myExpl[[2]] <- terra::as.factor(myExpl[[2]])

# Registering do parallel backend
registerDoParallel(cores = coreNumber) # cores = 10 crashed the whole computer
# LOOP PER ignitionContent
# for(c in 1:length(ignitionContent)){#ADcheck
c = 1
  # LOOP PER MONTE CARLO SIMULATION
  for(r in 1:n_replications){
    # setting seed to maintain reproducibility
    set.seed(seed_r[r])
    # Preprocessing points data: assigning random numbers (ranNum) to the firePoints for sampling
    values(firePoints) <- values(firePoints) %>% mutate(presAbs_BL = 1) %>% mutate(ranNum = runif(nrow(firePoints), 0, 100000))
    firePoints <- firePoints %>% terra::sort(v = "ranNum") 
    # Sample the firePoints according to the ignitionContent
    n_ignition <- ignitionContent[c] * N_input
    n_ignition <- round(n_ignition)
    # generate a logical vector that matches the n_ignition
    n_ignition <- rep(TRUE, times = n_ignition) %>% c(rep(FALSE, times = N_input - n_ignition))
    # extract the ignitionPoints from firePoints according to n_ignition
    ignitionSample <- subset(firePoints, pointCateg == "ignition", c(pointCateg, presAbs_BL, ranNum), NSE = TRUE)
    ignitionSample <- subset(ignitionSample, n_ignition, c(pointCateg, presAbs_BL), NSE = TRUE)
    # extract the spreadPoints from firePoints according to N_input - n_ignition
    spreadSample <- subset(firePoints, pointCateg == "spread", c(pointCateg, presAbs_BL, ranNum), NSE = TRUE)
    n_spread <- rep(TRUE, times = N_input - length(ignitionSample)) %>% c(rep(FALSE, times = nrow(spreadSample) - N_input + length(ignitionSample)))
    spreadSample <- terra::subset(spreadSample, n_spread, c(pointCateg, presAbs_BL), NSE = TRUE)
    # merge spreadSample with ignitionSample
    presencePoints <- ignitionSample %>% rbind(spreadSample)
    # LOOP PER ABSENCE POINT SET
    for(i in 1:10){
      # load the specified absence points
      absencePoints <- paste0(absencePoints_dir, "bas_sample", i, ".shp") %>% vect()
      values(absencePoints) <- values(absencePoints) %>% mutate(presAbs_BL = 0) %>% mutate(pointCateg = "absence")
      # Construct compiled responses
      myRespXY <- subset(absencePoints, pointCateg == "absence", c(pointCateg, presAbs_BL), NSE=TRUE)
      myRespXY <- subset(presencePoints, pointCateg != "absence", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY)  #ADhere
      # myRespXY <- subset(firePoints, pointCateg == "ignition", c(pointCateg, presAbs_BL), NSE=TRUE) %>% rbind(myRespXY) # this line is used instead of the above line when loop is run outside the C loop
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
      # myBiomodData
      # plot(myBiomodData)
      
      # explicitly define categorical variables as categorical.
      # as.factor(spatRaster)
      
      
      # Processing======
      # Run the model
      myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                          modeling.id = paste0('sensitivity_', c, '_abs_', i, '_sim_', r),
                                          models = c('RFd'),
                                          CV.strategy = 'random',
                                          CV.nb.rep = 10, # set to 10 in non-monte carlo runs
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
      
      # plotting response curves
      mods <- get_built_models(myBiomodModelOut, run = 'allRun')
      respCurves <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                            models.chosen = mods,
                            fixed.var = 'median')
      
      
      # Retreiving the projection map
      myBiomodProj <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                        proj.name = paste0("ignitionPct", (ignitionContent[c]*100), "_absenceSet_", i, "_sim_", r),
                                        new.env = myExpl,
                                        models.chosen = "wildfire2015_allData_allRun_RFd", # allRun refers to the fact that the selected model is the one that uses all data
                                        nb.cpu = coreNumber,
                                        seed.val = 42)
      
      # Saving
      # save.image(file = paste0(mainDir, "correctedRun_", i, ".RData"))
      save.image(file = paste0("monteCarlo_", i, "_ignPct", (ignitionContent[c]*100), "_rep_", r, ".RData"))#mainDir, omitted for running at HPC
    } # i Loop ends
    gc()
  } # r Loop ends
# } # c Loop ends
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
# myRespName <- 'wildfire2015'
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
#                                   models.chosen = 'wildfire2015_allData_allRun_RFd',
#                                   nb.cpu = coreNumber,
#                                   seed.val = 42)
# # Saving
# save.image(file = paste0(mainDir, "correctedRun.RData"))