# Additional script to execute join folowing the calculation of H index and the dominant LC
# Land cover CCI
# AD
# 05/07/2023

# libraries
library(sf)
library(tidyverse)

# 1. input

tess_shp <- "D:/dwiputra/landCover_cci/tesselation_1km.shp" %>% st_read()
toJoin_table <- "D:/dwiputra/landCover_cci/hIndex_majority_perGrid.csv" %>% read.csv()

# 2. Filter shp to only include the land dominated grids
tess_shp <- tess_shp %>% filter(GRID_ID %in% toJoin_table$GRID_ID) %>% left_join(toJoin_table)

# 3. Export
tess_shp %>% st_write("D:/dwiputra/landCover_cci/tessLand_joined.shp", drivers = "ESRI Shapefile", append = FALSE)
