# Permutation importance of predictors used in 100 percent ignition models

# 0. LIBRARIES====
library(tidyverse)
library(data.table)
library(terra)
library(ggpointdensity)
library(viridis)
# 1. INPUTS=====
inputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
# inputDir_vector <- "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/"
inputDir_vector <- "F:/temp_donotDelete/finalized_materials/"
ignitionSpread_pointSummary <- fread(paste0(inputDir, "ignitionSpread_summary.csv"))
convexHull_polygon <- vect(paste0(inputDir_vector, "convexHull_final2015fireNetwork.shp")) %>% values()



# 2. Preprocessing======
# joining the data.frames to extract the area of convex hulls
ignitionArea_summary <- convexHull_polygon %>% rename(graph_id = id) %>% left_join(ignitionSpread_pointSummary)
# allIgnition_varImportance <- allIgnition_varImportance %>% pivot_wider(names_from = c(rand), values_from = var.imp, names_prefix = "permutation_")
# # b. Add mean and standard deviation calculated based on the different random permutations (n = 3)
# # allIgnition_varImportance <- allIgnition_varImportance %>% rowwise() %>% mutate(mean = mean(c(permutation_1, permutation_2, permutation_3))) %>% mutate(sd = sd(c(permutation_1, permutation_2, permutation_3))) #ADomit
# # c. Because the sd seems very small, we opt to summarize the data so that each variable is represented by 1 bar and the sd is calculated based on the 10 absence sets
# # Summmarize the permutation importance across the different absenceSet runs
# summarized_varImportance <- allIgnition_varImportance %>% group_by(expl.var) %>% summarize(meanVarImp = mean(var.imp), standDev = sd(var.imp))
# # Arrange from the highest meanVarImp
# summarized_varImportance <- summarized_varImportance %>% arrange(-meanVarImp) %>% mutate(displayedName = c("Ecoregion", "VPD", "Dominant Land Cover", "Elevation", "Travel Time to
# the Nearest City",
#                                                                                                            "Human Footprint
# Index", "Land Cover
# Diversity", "Distance to Water"))

# 3. PLOTTING=========
# a. Scatter plot for ignition / spread with loess line adopted with modification from: https://ademos.people.uic.edu/Chapter10.html
scatter_ignitionSpread = ggplot(data = ignitionSpread_pointSummary, aes(x = n_ignitions, y = n_spread))+
  geom_point(alpha = 0.08)+
  # geom_smooth(color='black')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("Number of origin points")+ylab("Number of spread points") #+
  # scale_x_continuous(breaks = seq(0, 50, 5))+
  # scale_y_continuous(breaks = seq(50, 90, 5))
# pointDensity_ignitionSpread = ggplot(data = ignitionSpread_pointSummary, aes(x = n_ignitions, y = n_spread))+
#   geom_pointdensity() +
#   scale_colour_viridis_c(breaks = c(100, 500, 1500, 2500, 5000, 10000)) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
#   xlab("Number of origin points")+ylab("Number of spread points") #+
# b. Scatter plot for ignition / convexHull area with loess line adopted with modification from: https://ademos.people.uic.edu/Chapter10.html
scatter_ignitionConvHull = ggplot(data = ignitionArea_summary, aes(x = n_ignitions, y = km_area))+
  geom_point(alpha = 0.08)+
  # geom_smooth(color='black')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("Number of origin points")+ylab("Area of convex hull (square km)") 
