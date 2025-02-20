library(terra)

# Load species occurrences (6 species available)
data(DataSpecies)
head(DataSpecies)

# Select the name of the studied species
myRespName <- 'GuloGulo'

# Get corresponding presence/absence data
myResp <- as.numeric(DataSpecies[, myRespName])

# Get corresponding XY coordinates
myRespXY <- DataSpecies[, c('X_WGS84', 'Y_WGS84')]

# Load environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12)
data(bioclim_current)
myExpl <- terra::rast(bioclim_current)



# --------------------------------------------------------------- #
# Format Data with true absences
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName)

user.RFd <- list("for_all_datasets" = list(
  type = "classification",
  ntree = 1000,
  mtry = 2,
  strata = factor(c(0, 1)),
  sampsize = NULL,
  nodesize = 10,
  maxnodes = NULL
))# --------------------------------------------------------------- #
# List of all models currently available in `biomod2` (and their related package and function)
# Some of them can be tuned through the `train` function of the `caret` package 
# (and corresponding training function to be used is indicated)
myBiomodModelOut@models.options@val@options$RFd.binary.randomForest.randomForest@args.values
data(ModelsTable)
ModelsTable

allModels <- c('ANN', 'CTA', 'FDA', 'GAM', 'GBM', 'GLM'
               , 'MARS', 'MAXENT', 'MAXNET', 'RF', 'SRE', 'XGBOOST')

# default parameters
opt.d <- bm_ModelingOptions(data.type = 'binary',
                            models = allModels,
                            strategy = 'bigboss')

# tune parameters for Random Forest model
tuned.rf <- bm_Tuning(model = 'RFd',
                      tuning.fun = 'rf', ## see in ModelsTable
                      do.formula = FALSE,
                      bm.options = opt.d@options$RFd.binary.randomForest.randomForest,
                      bm.format = myBiomodData)
tuned.rf


cv.r <- bm_CrossValidation(bm.format = myBiomodData,
                           strategy = 'random',
                           nb.rep = 10,
                           perc = 0.7)

opt.d <- bm_ModelingOptions(data.type = 'binary',
                            models = 'RFd',
                            strategy = 'bigboss',    
                            bm.format = myBiomodData,
                            calib.lines = cv.r)

## You use the function bm_Tuning to tuned mtry
tuned.rf <- bm_Tuning(model = 'RFd',
                      tuning.fun = 'rf', 
                      bm.options = opt.d@options$RFd.binary.randomForest.randomForest,
                      bm.format = myBiomodData, 
                      calib.lines = cv.r)

## You change the nodesize parameter. Here for all your PAs and RUNs.
for(n in names(tuned.rf)){
  tuned.rf[[n]] <- c(tuned.rf[[n]], nodesize = 10)
}

## You can integrate it in your opt.d
user.val <- list( RFd.binary.randomForest.randomForest = tuned.rf)

opt.d <- bm_ModelingOptions(data.type = 'binary',
                            models = 'RFd',
                            strategy = 'user.defined',   
                            user.val = user.val,
                            bm.format = x_data,
                            calib.lines = cv.r)

print(opt.d, dataset = "_PA1_RUN1")
print(opt.d, dataset = "_PA1_RUN4")
print(opt.d, dataset = "_allData_allRun") # here, "_allData_allRun" should not be modified.