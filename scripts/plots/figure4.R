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
responseCurves_compile_nTree500 <- fread(paste0(inputDir, "medianResponseCurves_ignitionPercent_100_nTree500.csv"))
landCover_lookup <- read.csv("F:/temp_donotDelete/finalized_materials/landCover_lookup.csv")
ecoregion_lookup <- read.csv("F:/temp_donotDelete/finalized_materials/ecoregion_lookup.csv")#ADhere


# ADtemp=======
# Import landCover raster
landCover_raster <- rast("F:/temp_donotDelete/finalized_materials/na_synced_covariates_v2/originalValues_factor/dominantLandCover.tif") %>% terra::as.factor()
# Import ecoregion raster
ecoregion_raster <- rast("F:/temp_donotDelete/finalized_materials/na_synced_covariates_v2/Ecoregion_v2.tif") %>% terra::as.factor()
# ADtemp----

# PREDICTORS
predictors <- c("vpd", "DEM", "H", "gridcode", "ecoregion", "human", "travel", "water")
# 2. PREPROCESSING=======
# A. SEPARATE FOR EACH VARIABLE ANALYZED
vpd_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[1], expl.name)) 
dem_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[2], expl.name)) 
h_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[3], expl.name)) 
domLandCover_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[4], expl.name)) 
ecoregion_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[5], expl.name))
human_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[6], expl.name)) 
travel_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[7], expl.name)) 
water_df <- responseCurves_compile_nTree500 %>% filter(grepl(predictors[8], expl.name)) 
# B. Join categorical variables df with the corresponding lookup tables
# b1. domLandCover
landCover_lookup <- landCover_lookup %>% arrange(expl.val)
lookup_toJoin <- landCover_lookup %>% mutate(expl.val.fact = levels(as.factor(as.character(landCover_lookup$expl.val))))
domLandCover_df <- domLandCover_df %>% left_join(lookup_toJoin, by = "expl.val")
# domLandCover_df <- domLandCover_df %>% mutate(expl.val.fact = as.factor(as.character(expl.val))) %>% left_join(lookup_toJoin, by = "expl.val.fact")
# b2. ecoregion
ecoregion_lookup <- ecoregion_lookup %>% arrange(expl.val)
lookup_toJoin <- ecoregion_lookup %>% mutate(expl.val.fact = levels(as.factor(as.character(ecoregion_lookup$expl.val))))
ecoregion_df <- ecoregion_df %>% left_join(lookup_toJoin, by = "expl.val")
# ecoregion_df <- ecoregion_df %>% mutate(expl.val.fact = as.factor(as.character(expl.val))) %>% left_join(lookup_toJoin, by = "expl.val.fact")
# 3. PROCESSING======
# 4. PLOTTING======
# generate 8 box plots, each depicting the variable importance values with 0 or 100 ignition
# par(mfrow = c(2, 4))
# 1. VPD
png(file = paste0(inputDir, "plots/figure4_vpd.png"), width=1300, height = 1280, units = "px")
ggplot(vpd_df, aes(x=expl.val*0.01, y=pred.val)) + # 0.1 is the scaling factor; however the values would only make sense if it is interpreted directly as Pa before multiplied by scaling factor.
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "VPD (hPa)") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 2. Elevation
png(file = paste0(inputDir, "plots/figure4_elevation.png"), width=1300, height = 1280, units = "px")
ggplot(dem_df, aes(x=expl.val, y=pred.val)) +
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "Elevation (m)") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 3. Land cover diversity
png(file = paste0(inputDir, "plots/figure4_landCovH.png"), width=1300, height = 1280, units = "px")
ggplot(h_df, aes(x=expl.val, y=pred.val)) +
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "Land cover diversity index") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()

# 6. Human footprint
png(file = paste0(inputDir, "plots/figure4_humanFootprint.png"), width=1300, height = 1280, units = "px")
ggplot(human_df, aes(x=expl.val, y=pred.val)) +
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "Human footprint index") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 7. Travel time to nearest city
png(file = paste0(inputDir, "plots/figure4_travel.png"), width=1300, height = 1280, units = "px")
ggplot(travel_df, aes(x=expl.val, y=pred.val)) +
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "Travel time to the nearest city (minutes)") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 8. Distance to water
png(file = paste0(inputDir, "plots/figure4_water.png"), width=1300, height = 1280, units = "px")
ggplot(water_df, aes(x=expl.val*0.001, y=pred.val)) +
  geom_point(alpha=0.06, size=7) +   geom_smooth(fill="#FFFF43") +   labs(y="", x = "Distance to water (km)") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()

# Categorical response curves=====
# 4. Dominant land cover
# a. summarize the values per ecoregion types
summarized_fireIgnitionResponse <- domLandCover_df %>% group_by(landClass) %>% summarize(meanResp = mean(pred.val), standDev = sd(pred.val)) %>% ungroup()
# Removing trailing double space
summarized_fireIgnitionResponse <- summarized_fireIgnitionResponse %>% mutate(landClass = gsub("  ", " ", landClass))
# 3. PLOTTING=========
# a. Bar plot with standard deviation; adopted with modification from https://r-graph-gallery.com/4-barplot-with-error-bar.html 
png(file = paste0(inputDir, "plots/figure4_landCovDominant_bar.png"), width=2100, height = 1280, units = "px")
ggplot(summarized_fireIgnitionResponse) +
  geom_bar( aes(y=reorder(landClass, meanResp), x=meanResp), stat="identity", fill="gray", alpha=0.7) +
  geom_errorbar(aes(y=reorder(landClass, meanResp), xmin=meanResp-standDev, xmax=meanResp+standDev), width=0.5, colour="black", alpha=0.9, linewidth=0.8) +
  ylab("Dominant land cover class") + xlab(element_blank()) + theme_minimal() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 
png(file = paste0(inputDir, "plots/figure4_landCovDominant.png"), width=2100, height = 1280, units = "px")
ggplot(domLandCover_df, aes(x=pred.val, y=landClass)) +
  geom_point() +   labs(y="", x = "Predictor value") +
  ggtitle("Dominant land cover") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()
# 5. Ecoregion
summarized_fireIgnitionResponse <- ecoregion_df %>% group_by(newECONAME) %>% summarize(meanResp = mean(pred.val), standDev = sd(pred.val)) %>% ungroup()
# 3. PLOTTING=========
# a. Bar plot with standard deviation; adopted with modification from https://r-graph-gallery.com/4-barplot-with-error-bar.html 
png(file = paste0(inputDir, "plots/figure4_ecoregion_bar.png"), width=2100, height = 1350, units = "px")
ggplot(summarized_fireIgnitionResponse) +
  geom_bar( aes(y=reorder(newECONAME, meanResp), x=meanResp), stat="identity", fill="gray", alpha=0.7) +
  geom_errorbar(aes(y=reorder(newECONAME, meanResp), xmin=meanResp-standDev, xmax=meanResp+standDev), width=0.5, colour="black", alpha=0.9, linewidth=0.8) +
  ylab("Ecoregion") + xlab(element_blank()) + theme_minimal() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=30))
dev.off()
# 
png(file = paste0(inputDir, "plots/figure4_ecoregion.png"), width=2100, height = 1280, units = "px")
ggplot(ecoregion_df, aes(x=pred.val, y=newECONAME)) +
  geom_point() +   labs(y="", x = "Predictor value") +
  ggtitle("Ecoregion") + theme_classic() +
  theme(axis.title=element_text(size=66), axis.text=element_text(size=45))
dev.off()


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


# Loess line plotting template
# Create scatterplot with loess curve
# ggplot(economics, aes(x = date, y = unemploy)) +
#   geom_point(alpha=0.06, size=7) +
#   geom_smooth(fill="#FFFF43") +
#   labs(y="", y = "Predictor value") +
#   ggtitle("USA - Unemployed People (1967-2015)")