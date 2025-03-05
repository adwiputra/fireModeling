# Generate the summarized response curves
# 26/02/2025
# AD

# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)
library(data.table)


# 2. INPUTS=====
outputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
rData_dir_nTree500 <- "D:/Documents/research/projects/nus07_fire/analysis/output/hpcRun/finalized_randomForest/nTree500_ignition100_run/wildfire.bigBoss/"

# 3. PREPROCESSING======
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
