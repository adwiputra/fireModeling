# Stats to check the difference between 0 % and 100 % ignition
# first written 17 February 2025
# AD

# 0. LIBRARIES====
library(lme4)
library(lmerTest)
library(ggplot2)
library(data.table)
library(tidyverse)

# 1. INPUTS=====
inputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
allIgnition_varImportance <- fread(paste0(inputDir, "variableImportance_monteCarlo_ignitionPercent_100.csv"))
noIgnition_varImportance <- fread(paste0(inputDir, "variableImportance_monteCarlo_ignitionPercent_0.csv"))

# PREDICTORS
predictors <- c("vpd", "DEM", "H", "gridcode", "ecoregion", "human", "travel", "water")
# 2. PREPROCESSING=======
# A. BIND DATA FRAMES
combined_varImportance <- allIgnition_varImportance %>% bind_rows(noIgnition_varImportance)

# B. SEPARATE FOR EACH VARIABLE ANALYZED
vpd_df <- combined_varImportance %>% filter(grepl(predictors[1], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
dem_df <- combined_varImportance %>% filter(grepl(predictors[2], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
h_df <- combined_varImportance %>% filter(grepl(predictors[3], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
domLandCover_df <- combined_varImportance %>% filter(grepl(predictors[4], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
ecoregion_df <- combined_varImportance %>% filter(grepl(predictors[5], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
human_df <- combined_varImportance %>% filter(grepl(predictors[6], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
travel_df <- combined_varImportance %>% filter(grepl(predictors[7], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
water_df <- combined_varImportance %>% filter(grepl(predictors[8], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent))
# 3. PROCESSING======
# A. LMER
# A1. VPD
lmer_vpd <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = vpd_df)#,      
             # control=lmerControl(check.nobs.vs.nlev = "ignore",
             #                     check.nobs.vs.nRE  = "ignore"))
summary(lmer_vpd)
anova(lmer_vpd)

# A2. DEM
lmer_dem <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = dem_df)
summary(lmer_dem)
anova(lmer_dem)

# A3. landCover diversity (H)
lmer_h <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = h_df)
summary(lmer_h)
anova(lmer_h)

# A4. dominant land cover
lmer_domLandCover <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = domLandCover_df)
summary(lmer_domLandCover)
anova(lmer_domLandCover)

# A5. ecoregion
lmer_ecoregion <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = ecoregion_df)
summary(lmer_ecoregion)
anova(lmer_ecoregion)

# A6. human footprint
lmer_human <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = human_df)
summary(lmer_human)
anova(lmer_human)

# A7. travel time
lmer_travel <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = travel_df)
summary(lmer_travel)
anova(lmer_travel)

# A8. distance to water
lmer_water <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = water_df)
summary(lmer_water)
anova(lmer_water)

# Positive fixed effect means that the 100% ignition run has higher varImp for the predictor in focus and vice versa.
# B. T-TEST