# Evaluating biomod run outputs
# Script first created: 26/02/2025
# AD
# 1. libraries==========
library(tidyverse)
library(doParallel)
library(terra)
library(biomod2)
library(data.table)


# 2. INPUTS=====
outputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
rData_dir_nTree1000 <- "D:/Documents/research/projects/nus07_fire/analysis/output_old/hpcRun/finalized_randomForest/"
rData_dir_nTree500 <- "D:/Documents/research/projects/nus07_fire/analysis/output/hpcRun/origin/"

# 3. PREPROCESSING======
ignitionPct <- 100
absenceSets <- 10
rData_files_nTree1000 <- rData_dir_nTree1000 %>% list.files(pattern = "RData$", full.names = TRUE, recursive = TRUE)
rData_files_nTree500 <- rData_dir_nTree500 %>% list.files(pattern = "RData$", full.names = TRUE, recursive = TRUE)
# rData_files_nTree1000 naming convention = "monteCarlo_absenceSets_ignPctignitionPct_rep_monteCarloRuns.RData"
# Generate a blanko
evaluationScores_compile_nTree1000 <- data.frame()
evaluationScores_compile_nTree500 <- data.frame()
# 4. PROCESSING========
# Loop absence sets
ig <- ignitionPct # i was taken by another variable stored in RData
# for(a in 1:3){#ADtemp
for(a in 1:absenceSets){
    # a. load RData
    select_rData <- rData_files_nTree1000 %>% grep(paste0("ADmod_ignitionPct100_absenceSet", a, ".RData"), ., value = TRUE)
    load(select_rData)
    print(paste0("Successfully loaded a ", a, " ignition percent ", ig))
    # b. Manipulate data.frame and summarize
    # evaluationScores <- evaluationScores %>% filter(run != "allRun") # we do not need to filter based on the runs because run == "allRun" does not have validation scores
    evaluationScores <- evaluationScores %>% mutate(absenceSet = a) %>% mutate(ignitionPercent = ig) %>% mutate(monteCarloRun = 0)
    evaluationScores_compile_nTree1000 <- evaluationScores_compile_nTree1000 %>% bind_rows(evaluationScores)
    gc()
}

# Repeat for nTree = 500
for(a in 1:absenceSets){
  # a. load RData
  # select_rData <- rData_files_nTree500 %>% grep(paste0("correctedRun_", a, ".RData"), ., value = TRUE)
  select_rData <- rData_files_nTree500 %>% grep(paste0("absenceSet", a, ".RData"), ., value = TRUE) %>% grep(paste0("reclassedCat"), ., value = TRUE)
  load(select_rData)
  print(paste0("Successfully loaded a ", a, " ignition percent ", ig))
  # b. Manipulate data.frame and summarize
  # evaluationScores <- evaluationScores %>% filter(run != "allRun") # we do not need to filter based on the runs because run == "allRun" does not have validation scores
  evaluationScores <- evaluationScores %>% mutate(absenceSet = a) %>% mutate(ignitionPercent = ig) %>% mutate(monteCarloRun = 0)
  evaluationScores_compile_nTree500 <- evaluationScores_compile_nTree500 %>% bind_rows(evaluationScores)
  gc()
}

# 5. EXPORT======
fwrite(evaluationScores_compile_nTree1000, paste0(outputDir, "evaluationScores_ignitionPercent_", ig, "_nTree1000.csv"))
fwrite(evaluationScores_compile_nTree500, paste0(outputDir, "evaluationScores_ignitionPercent_", ig, "_nTree500.csv"))

# 6. Import====
evaluationScores_compile_nTree1000 <- paste0(outputDir, "evaluationScores_ignitionPercent_", ig, "_nTree1000.csv") %>% fread()
evaluationScores_compile_nTree500 <- paste0(outputDir, "evaluationScores_ignitionPercent_", ig, "_nTree500.csv") %>% fread()


evaluationScores_compile_nTree1000 %>% filter(metric.eval == "TSS") %>% select(validation) %>% pull() %>% mean(na.rm = TRUE)
evaluationScores_compile_nTree500 %>% filter(metric.eval == "TSS") %>% select(validation) %>% pull() %>% mean(na.rm = TRUE)
