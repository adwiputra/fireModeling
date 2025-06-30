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
# inputDir_vector <- "F:/temp_donotDelete/finalized_materials/"
inputDir_vector <- "E:/temp_transfer/finalized_materials/"
ignitionSpread_pointSummary <- fread(paste0(inputDir_vector, "ignitionSpread_summary.csv"))
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
# a. Scatter plot for ignition / spread
scatter_ignitionSpread = ggplot(data = ignitionSpread_pointSummary, aes(x = log(n_ignitions), y = log(n_spread))) +
  geom_point(alpha = 0.08)+
  # geom_smooth(color='black')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("log10(Number of origin points)")+ylab("log10(Number of spread points)") #+
  # scale_x_continuous(breaks = seq(0, 50, 5))+
  # scale_y_continuous(breaks = seq(50, 90, 5))
# pointDensity_ignitionSpread = ggplot(data = ignitionSpread_pointSummary, aes(x = n_ignitions, y = n_spread))+
#   geom_pointdensity() +
#   scale_colour_viridis_c(breaks = c(100, 500, 1500, 2500, 5000, 10000)) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
#   xlab("Number of origin points")+ylab("Number of spread points") #+
# b. Scatter plot for ignition / convexHull area
scatter_ignitionConvHull = ggplot(data = ignitionArea_summary, aes(x = log(n_ignitions), y = log(km_area)))+
  geom_point(alpha = 0.08)
  # geom_smooth(color='black')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("log10(Number of origin points)")+ylab("log10(km Area of convex hull)")

# c. Hexbin for ignition / spread
png(file = paste0(inputDir, "plots/jun2025/jun30/figure2a_hexBin.png"), width=1400, height = 1300, units = "px")
hexBin_ignitionSpread = ggplot(data = ignitionSpread_pointSummary, aes(x = log(n_ignitions), y = log(n_spread))) +
  geom_hex(bins = 15) + scale_fill_viridis() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("log10(Number of origin points)")+ylab("log10(Number of spread points)")+
  labs(fill = "Count")+
  theme(axis.title=element_text(size=50), axis.text=element_text(size=30),
        legend.position = c(.95, 0.1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.25, "cm")
  )
hexBin_ignitionSpread
#+
dev.off()
# d. Hexbin for ignition / spread
png(file = paste0(inputDir, "plots/jun2025/jun30/figure2b_hexBin.png"), width=1400, height = 1300, units = "px")
hexBin_ignitionConvHull = ggplot(data = ignitionArea_summary, aes(x = log(n_ignitions), y = log(km_area))) +
  geom_hex(bins = 15) + scale_fill_viridis() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  xlab("log10(Number of origin points)")+ylab("log10(km Area of convex hull)")+
  labs(fill = "Count")+
  theme(axis.title=element_text(size=50), axis.text=element_text(size=30),
        legend.position = c(.95, 0.1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 30),
        legend.key.size = unit(1.25, "cm")
  )
hexBin_ignitionConvHull
dev.off()
# Linear model of ignition points vs. conv hull
# lm_convHull <- lm(log(km_area) ~ sqrt(n_ignitions), data = ignitionArea_summary)
# summary(lm_convHull)
# 
# ignitionArea_summary <- ignitionArea_summary %>% mutate(expVariable = n_ignitions^(1/4))
# lm_convHull <- lm(log(km_area) ~ sqrt(n_ignitions), data = ignitionArea_summary)
