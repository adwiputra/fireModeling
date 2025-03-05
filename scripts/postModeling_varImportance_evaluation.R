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
rData_dir <- "D:/Documents/research/projects/nus07_fire/analysis/output/hpcRun/finalized_randomForest/nTree500_ignition0_run/"
summarized <- TRUE # should the three permutations outcome be averaged?
# 3. PREPROCESSING======
ignitionPct <- 0
absenceSets <- 10
monteCarloRuns <- 100
rData_files <- rData_dir %>% list.files(pattern = "RData$", full.names = TRUE, recursive = TRUE)
# rData_files naming convention = "monteCarlo_absenceSets_ignPctignitionPct_rep_monteCarloRuns.RData"
# Generate a blanko
variableImportance_compile <- data.frame()
# 4. PROCESSING========
# Loop absence sets
ig <- ignitionPct # i was taken by another variable stored in RData
# for(a in 1:3){#ADtemp
for(a in 1:absenceSets){
  # a <- 1 #ADtemp
  # Loop monteCarloRuns
  # for(m in 1:5){#ADtemp
  for(m in 1:monteCarloRuns){
    # m <- 1 monteCarlo_1_ignPct0_rep_1_nTree500
    # a. load RData
    select_rData <- rData_files %>% grep(paste0("monteCarlo_", a, "_ignPct", ig, "_rep_", m, "_nTree500.RData"), ., value = TRUE)
    load(select_rData)
    print(paste0("Successfully loaded a ", a, " m ", m))
    if(summarized) variable_importance <- variable_importance %>% filter(run == "allRun") %>% group_by(expl.var, run) %>% summarize(var.imp = mean(var.imp)) %>% ungroup() else{
      variable_importance <- variable_importance %>% filter(run == "allRun")
    }
    variable_importance <- variable_importance %>% mutate(absenceSet = a) %>% mutate(ignitionPercent = ig) %>% mutate(monteCarloRun = m)
    variableImportance_compile <- variableImportance_compile %>% bind_rows(variable_importance)
    gc()
  }
}
print(paste0("This line is to check if error during load will halt processing")) # no, the process was not halted despite the error during the load
# 5. EXPORT======
if(summarized) fwrite(variableImportance_compile, paste0(outputDir, "summarizedVariableImportance_monteCarlo_ignitionPercent_", ig, ".csv")) else{
  fwrite(variableImportance_compile, paste0(outputDir, "variableImportance_monteCarlo_ignitionPercent_", ig, ".csv"))
}