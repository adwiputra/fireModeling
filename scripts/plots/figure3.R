# Permutation importance of predictors used in 100 percent ignition models

# 0. LIBRARIES====
library(tidyverse)
library(data.table)

# 1. INPUTS=====
inputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
allIgnition_varImportance <- fread(paste0(inputDir, "summarizedVariableImportance_monteCarlo_ignitionPercent_100.csv"))

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# 2. Preprocessing======
# a. Reshaping the data.frame
# allIgnition_varImportance <- allIgnition_varImportance %>% pivot_wider(names_from = c(rand), values_from = var.imp, names_prefix = "permutation_")
# b. Add mean and standard deviation calculated based on the different random permutations (n = 3)
# allIgnition_varImportance <- allIgnition_varImportance %>% rowwise() %>% mutate(mean = mean(c(permutation_1, permutation_2, permutation_3))) %>% mutate(sd = sd(c(permutation_1, permutation_2, permutation_3))) #ADomit
# c. Because the sd seems very small, we opt to summarize the data so that each variable is represented by 1 bar and the sd is calculated based on the 10 absence sets
# Summmarize the permutation importance across the different absenceSet runs
summarized_varImportance <- allIgnition_varImportance %>% group_by(expl.var) %>% summarize(meanVarImp = mean(var.imp), standDev = sd(var.imp))
# Arrange from the highest meanVarImp
summarized_varImportance <- summarized_varImportance %>% arrange(-meanVarImp) %>% mutate(displayedName = c("Ecoregion", "VPD", "Dominant
Land Cover", "Elevation", "Travel Time to
the Nearest City",
                                                                                                           "Human
Footprint Index", "Land Cover
Diversity", "Distance to
Water"))

# 3. PLOTTING=========
# a. Bar plot with standard deviation; adopted with modification from https://r-graph-gallery.com/4-barplot-with-error-bar.html 
png(file = paste0(inputDir, "plots/jun2025/jun30/figure3_varImportance.png"), width=1950, height = 1280, units = "px")
ggplot(summarized_varImportance) +
  geom_bar( aes(x=reorder(displayedName, -meanVarImp), y=meanVarImp), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar(aes(x=reorder(displayedName, -meanVarImp), ymin=meanVarImp-standDev, ymax=meanVarImp+standDev), width=0.5, colour="black", alpha=0.9, linewidth=0.8) + theme_minimal() +
  ylab("Permutation importance") + xlab(element_blank()) + theme(axis.title=element_text(size=66), axis.text=element_text(size=32))
dev.off()
