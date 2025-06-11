# Generate absence data based on the polygon: 'absence_spatialBound_intersect_dissolve_4km.shp'
# through the implementation of Balanced acceptable samples (BAS), followed by HIP (Halton iterative partitioning)
# 23/01/2025
# AD

# 0. LIBRARIES========
library(spbal)
library(sf)
library(ggplot2)
library(gridExtra)
library(tidyverse)

# 1. INPUTS=======
output_dir <- "E:/temp_transfer/finalized_materials/basSampled_absences/"
if(!dir.exists(output_dir))dir.create(output_dir)
shp_studySite <- st_read(paste0(dirname(output_dir), "/absence_spatialBound_intersect_dissolve_4km.shp"))
bb <- BoundingBox(shapefile = shp_studySite)
n_samples <- read.csv(paste0(dirname(output_dir), "/ignitionSpread_machLearn_input.csv")) %>% filter(pointCategory == "ignition") %>%
  select(LATITUDE, LONGITUDE) %>% unique() %>% nrow()#set to 75424 previously # matches the number of ignition points

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