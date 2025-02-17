# Evaluating biomod run outputs
# Script first created: 17/02/2025
# AD
# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)
library(data.table)


# 2. INPUTS=====
outputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
rData_dir <- "D:/Documents/research/projects/nus07_fire/analysis/output/falconRun/wildfire2015/"
summarized <- FALSE # should the three permutations outcome be averaged?
# 3. PREPROCESSING======
ignitionPct <- 100
absenceSets <- 10
rData_files <- rData_dir %>% list.files(pattern = "RData$", full.names = TRUE, recursive = TRUE)
# rData_files naming convention = "monteCarlo_absenceSets_ignPctignitionPct_rep_monteCarloRuns.RData"
# Generate a blanko
variableImportance_compile <- data.frame()
# 4. PROCESSING========
# Loop absence sets
ig <- ignitionPct # i was taken by another variable stored in RData
# for(a in 1:3){#ADtemp
for(a in 1:absenceSets){
    # a. load RData
    select_rData <- rData_files %>% grep(paste0("correctedRun_", a, ".RData"), ., value = TRUE)
    load(select_rData)
    print(paste0("Successfully loaded a ", a, " ignition percent ", ig))
    # b. Manipulate data.frame and summarize
    if(summarized) variable_importance <- variable_importance %>% filter(run == "allRun") %>% group_by(expl.var, run) %>% summarize(var.imp = mean(var.imp)) %>% ungroup() else{
      variable_importance <- variable_importance %>% filter(run == "allRun")
    }
    variable_importance <- variable_importance %>% mutate(absenceSet = a) %>% mutate(ignitionPercent = ig) %>% mutate(monteCarloRun = 0)
    variableImportance_compile <- variableImportance_compile %>% bind_rows(variable_importance)
    gc()
}
# 5. EXPORT======
if(summarized) fwrite(variableImportance_compile, paste0(outputDir, "summarizedVariableImportance_monteCarlo_ignitionPercent_", ig, ".csv")) else{
  fwrite(variableImportance_compile, paste0(outputDir, "variableImportance_monteCarlo_ignitionPercent_", ig, ".csv"))
}