# Generate absence data based on the polygon: 'absence_spatialBound_intersect_dissolve_4km.shp'
# through the implementation of Balanced acceptable samples (BAS), followed by HIP (Halton iterative partitioning)
# 23/01/2025
# AD

# 0. LIBRARIES========
library(spbal)
library(sf)
library(ggplot2)
library(gridExtra)

# 1. INPUTS=======
output_dir <- "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/"
shp_studySite <- st_read("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/absence_spatialBound_intersect_dissolve_4km.shp")
bb <- BoundingBox(shapefile = shp_studySite)
n_samples <- 75424 # matches the number of ignition points

# 2. PROCESSING=========
seeds <- c(411, 4, 11, 1, 5, 8, 20, 41, 21, 19)

for(i in 1:10){
  set.seed(seeds[i])
  
  # Iteration 1
  bas_sample1 <- BAS(shapefile = shp_studySite,
                     n = n_samples,
                     minRadius = 4000, # in meters
                     boundingbox = bb)
  
  bas_sample1 <- bas_sample1$sample
  
  st_write(bas_sample1, paste0(output_dir, "bas_sample", i, ".shp"))
}
# Started at 6 pm 23/01/2025