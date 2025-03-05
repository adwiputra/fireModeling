# Figure 4. Response Curves of Predictors
# 03/03/2025
# AD

# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)
library(data.table)


# 2. INPUTS=====
inputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
responseCurves_compile_nTree500 <- fread(paste0(inputDir, "responseCurves_ignitionPercent_100_nTree500.csv"))
landCoverDir <- "F:/temp_donotDelete/data/spatial/covariates/landCover_CCI/"
categoricalVariables <- c("gridcode", "reduced_ecoregion_raster")
landCover_freq <- "F:/temp_donotDelete/finalized_materials/na_synced_covariates_v2/dominantLandCover.tif" %>% rast() %>% as.factor() %>% freq()
# Ecoregion raster attribute table
ecoregion_freq <- "F:/temp_donotDelete/finalized_materials/na_synced_covariates_v2/Ecoregion.tif" %>% rast() %>% as.factor() %>% freq()
# Reduced ecoregion vector layer to extract the attribute table
ecoregion_lookup <- "F:/temp_donotDelete/finalized_materials/reduced_ecoregion.shp" %>% vect() %>% values()
# 3. PREPROCESSING======
# Prepping landCover_lookup
landCover_lookup <- paste0(landCoverDir, "LC_class_lookUp.csv") %>% read.csv()
landCover_lookup <- landCover_lookup %>% mutate(landClass = paste0(landCover_lookup[, 2], " ", landCover_lookup[, 3], " ", landCover_lookup[, 4])) %>% mutate(landClass = gsub("^\\s+|\\s+$", "", landClass))
landCover_lookup <- landCover_lookup %>% select(Value, landClass)
# Indexing the landCover_freq to join with the landCover_lookup
landCover_freq <- landCover_freq %>% mutate(value = as.factor(value)) %>% rename(Value = value) %>% select(-c(layer, count)) %>% mutate(expl.val = as.numeric(Value))
# joining landCover_freq and landCover_lookup
landCover_lookup <- landCover_lookup %>% mutate(Value = as.factor(Value)) %>% right_join(landCover_freq)
#ADhere

# 4. PROCESSING======
# Summmarize the response curves values across the different absenceSet runs
summarized_respCurveValue <- responseCurves_compile_nTree500 %>% group_by(expl.name, expl.val, pred.name) %>% summarize(mean_predVal = mean(pred.val), standDev = sd(pred.val), median_predVal = median(pred.val)) %>% ungroup()
# The summarized data frame contains many NAs for the standard deviation. This reflects the fact that the expl.var values are not homogeneous across the different absence runs.
# Therefore, we will only implement the standard summary approach to the categorical variables, namely gridcode (dominant land cover) and ecoregion
# The continuous variables will be plotted as scatterplot + loess line
summarized_respCurveValue_categoricalOnly <- summarized_respCurveValue %>% filter(expl.name %in% categoricalVariables)

# A. LAND COVER
# Join the summarized resp Curve values and the landCover_lookup
summarized_dominantLandCover_responseCurve <- summarized_respCurveValue_categoricalOnly %>% filter(expl.name == "gridcode") %>% left_join(landCover_lookup)

# B. ECOREGION

ignitionPct <- 100
absenceSets <- 10
rData_files_nTree500 <- rData_dir_nTree500 %>% dirname() %>% list.files(pattern = "RData$", full.names = TRUE, recursive = TRUE)
# rData_files_nTree1000 naming convention = "monteCarlo_absenceSets_ignPctignitionPct_rep_monteCarloRuns.RData"
# Generate a blanko
responseCurves_compile_nTree500 <- data.frame()
# 4. PROCESSING========
# Loop absence sets
ig <- ignitionPct # i was taken by another variable stored in RData

for(a in 1:absenceSets){
  # a. load RData
  select_rData <- rData_files_nTree500 %>% grep(paste0("bigBoss_ignitionPct", ig, "_absenceSet", a, ".RData"), ., value = TRUE)
  load(select_rData)
  print(paste0("Successfully loaded a ", a, " ignition percent ", ig))
  # b. Manipulate data.frame and summarize
  # responseCurves <- responseCurves %>% filter(run != "allRun") # we do not need to filter based on the runs because run == "allRun" does not have validation scores
  responseCurves <- respCurves$tab %>% mutate(absenceSet = a) %>% mutate(ignitionPercent = ig) %>% mutate(monteCarloRun = 0)
  responseCurves_compile_nTree500 <- responseCurves_compile_nTree500 %>% bind_rows(responseCurves)
  gc()
}

# 5. EXPORT======
fwrite(responseCurves_compile_nTree500, paste0(outputDir, "responseCurves_ignitionPercent_", ig, "_nTree500.csv"))




# Below this is copied from the variable importance statistics=======
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

# 4. PLOTTING======
# generate 8 box plots, each depicting the variable importance values with 0 or 100 ignition
par(mfrow = c(2, 4))
# 1. VPD
ggplot(vpd_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("VPD") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 2. Elevation
ggplot(dem_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Elevation") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 3. Land cover diversity
ggplot(h_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Land cover diversity") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 4. Dominant land cover
ggplot(domLandCover_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Dominant land cover") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 5. Ecoregion
ggplot(ecoregion_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Ecoregion") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 6. Human footprint
ggplot(human_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Human footprint") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 7. Travel time to nearest city
ggplot(travel_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Travel time to the nearest city") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 8. Distance to water# 2. Elevation
ggplot(water_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Distance to water") + theme_classic() + theme(plot.title = element_text(hjust=0.5))



# An alternative using facet.wrap
combined_varImportance %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>%
  # pivot_longer(everything()) %>%
  ggplot(aes(x = ignitionPercent, y = var.imp, group = )) + 
  geom_boxplot(fill='#A4A4A4', color="black") +
  facet_grid(~ expl.var) + facet_wrap( ~ expl.var, nrow = 2) + theme_gray()

