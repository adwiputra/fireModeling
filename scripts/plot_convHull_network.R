# Script to map the network spatially and apply additional filtering to obtain the ignitial points and the spread from the mapped fire networks
# AD
# 11/04/2024

# 0. LIBRARIES====
library(terra)
library(igraph)
library(data.table)
library(tidyverse)

# 1. INPUTS========
# a. Since there is no edge intersecting major water bodies, we can load the .RData from the previous analysis
# load("largeData_loaded.RData")
output_dir <- "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/"
load(paste0(output_dir, "partA_rawGraph_2025.RData"))
lc2014_raster <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v2.0.7.tif" %>% rast()
permWater_vector <- paste0(output_dir, "perm_waterBodies_patched_201415.shp") %>% vect()
# b. assign network ids to the inNear_table

# 2. Map convHull per network id=======
# First network as 'blanko'
n = 1
n_point_ids <- V(fireGraph_decompose[[n]])$name %>% as.integer()
n_point_coordinates <- attribute_allPoints %>% filter(pointID %in% n_point_ids) %>% select(LONGITUDE, LATITUDE, pointCategory, pointID)
n_points <- n_point_coordinates %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84")
convHull_all <- convHull(n_points)
# LOOP
for(n in 2:length(fireGraph_decompose)){
  n_point_ids <- V(fireGraph_decompose[[n]])$name %>% as.integer()
  n_point_coordinates <- attribute_allPoints %>% filter(pointID %in% n_point_ids) %>% select(LONGITUDE, LATITUDE, pointCategory, pointID)
  n_points <- n_point_coordinates %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84")
  convHull_add <- convHull(n_points)
  convHull_all <- rbind(convHull_all, convHull_add)
}
# 3. Export====
values(convHull_all) <- data.frame(id = 1:n, km_area = expanse(convHull_all, unit = "km"))
convHull_all %>% writeVector(paste0(output_dir, "convexHull_allFireNetwork.shp"), overwrite = TRUE) # savekeeping in case of anything
gc()
# 4. Run zonal statistics to filter out convexHulls with >= 50% bareland (value = 200) OR urban area (value == 190) as well as networks that occur on seas========
# a. Filter out networks with convHull area < 1 km
large_fireGraph_convHull <- convHull_all %>% subset(km_area >= 1, NSE = TRUE)

# b. total non-NoData cells per polygon
lc2014_raster <- lc2014_raster %>% crop(large_fireGraph_convHull)
lc2014_processedRaster <- (lc2014_raster != 0) & (lc2014_raster != 210)
denom_table_nCells <- lc2014_processedRaster %>% terra::extract(large_fireGraph_convHull, fun = sum)
# c. total barren or urban area
lc2014_processedRaster <- (lc2014_raster == 200) | (lc2014_raster == 190)
numerator_table_nCells <- lc2014_processedRaster %>% terra::extract(large_fireGraph_convHull, fun = sum)
# d. Obtain barren or urban area proportion
urbanBarrenProp_table_nCells <- denom_table_nCells %>% rename_at(2, ~"denom") %>% left_join(numerator_table_nCells, by = "ID") %>% rename_at(3, ~"nume") %>%  mutate(nCell_prop = nume/denom)
# e. Filtering
retained_fireGraph_IDs <- urbanBarrenProp_table_nCells %>% filter(denom > 4) %>% filter(nCell_prop < 0.1) %>% select(ID) %>% pull() #ID here refers to the rowID instad of graph ID
# 0.4, 0.3333 not enough to filter out all false positives; 0.1 provides good balance (OKI is retained)
# 5. Export retained convHull
retained_fireGraph_convHull <- large_fireGraph_convHull[retained_fireGraph_IDs]
# retained_fireGraph_convHull %>% writeVector(paste0(output_dir, "convexHull_filteredFireNetwork.shp"), overwrite = TRUE) #ADrun export
# Steps in ArcGIS Pro: select by location to identify potential intersection between the filtered convex hull with permWater_vector.

# Load ArcGIS modified retained_fireGraph_convHull
retained_fireGraph_convHull <- vect(paste0(output_dir, "convexHull_filteredFireNetwork.shp"))
# Identify any overlap with water bodies based on the CCI land cover 300m and the 30 m land cover water data
lc2014_processedRaster <- lc2014_raster == 210
waterCells_retained_convHull <- lc2014_processedRaster %>% terra::extract(retained_fireGraph_convHull, fun = sum) %>% rename_at(2, ~"waterCells")
# Assess if the labeled convex Hulls are inclusive of the extract outcomes
values(retained_fireGraph_convHull) <- waterCells_retained_convHull %>% select(waterCells) %>% bind_cols(values(retained_fireGraph_convHull), .) %>% mutate(water_int = case_when(waterCells > 0 ~ 1,
                                                                                                                                                                                  TRUE ~ water_int))

# Crop permWater_vector according to the water-intersecting filtered convex Hulls to speed up processing
water_intersect_convHull <-  retained_fireGraph_convHull %>% subset(water_int == 1, NSE = TRUE)
water_intersect_convHull %>% writeVector(paste0(output_dir, "water_intersecting_convexHulls.shp")) # Export to continue processing in ArcGIS Pro because R keeps crashing when running the few lines below
# The three lines below are commented out because it crashes R constantly when run
# permWater_vector <- permWater_vector %>% project(retained_fireGraph_convHull)
# permWater_vector <- permWater_vector %>% crop(water_intersect_convHull)
# permWater_vector %>% writeVector(paste0(output_dir, "permWater_crop.shp"))
permWater_vector <- paste0(output_dir, "perm_waterBodies_clipped.shp") %>% vect()

# filter the nearTable based on the retained_fireGraph_IDs
# Extract the point ids that compose the retained_fireGraph_IDs
retained_fireGraph_IDs <- retained_fireGraph_convHull$id
retained_fireGraphs <- fireGraph_decompose[retained_fireGraph_IDs]
allPoints_retained_fireGraph_IDs <- retained_fireGraphs %>% sapply(function(grph) V(grph)[]) %>% unlist() %>% names() %>% as.numeric()
# Subset the nearTable further to speed up potential water intersection processing
waterIntersect_fireGraph_IDs <- water_intersect_convHull$id
water_fireGraphs <- fireGraph_decompose[waterIntersect_fireGraph_IDs]
points_water_fireGraph_IDs <- water_fireGraphs %>% sapply(function(grph) V(grph)[]) %>% unlist() %>% names() %>% as.numeric() # %>% unique() # redundant because the output is already unique
# nearTable after the convexHull filters
onceFiltered_nearTable <- nearTable_allPoints %>% filter(IN_FID %in% allPoints_retained_fireGraph_IDs & NEAR_FID %in% allPoints_retained_fireGraph_IDs)
# nearTable that requires further water intersection check
waterIntersectCheck_nearTable <- onceFiltered_nearTable %>% filter(IN_FID %in% points_water_fireGraph_IDs & NEAR_FID %in% points_water_fireGraph_IDs)
save.image("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/partB_waterFiltering_2025.RData")
# Assign coordinates to the nearTable
waterIntersectCheck_nearTable <- attribute_allPoints %>% select(pointID, LONGITUDE, LATITUDE) %>% rename_at(1, ~"IN_FID") %>% right_join(waterIntersectCheck_nearTable)
waterIntersectCheck_nearTable <- attribute_allPoints %>% select(pointID, LONGITUDE, LATITUDE) %>% rename_all(~c("NEAR_FID", "LON_near", "LAT_near")) %>% right_join(waterIntersectCheck_nearTable)
# required column names: OBJECTID, LONGITUDE, LATITUDE, LON_near, LAT_near
waterIntersectCheck_nearTable <- waterIntersectCheck_nearTable %>% mutate(OBJECTID = 1:nrow(waterIntersectCheck_nearTable))
waterIntersectCheck_nearTable %>% fwrite(paste0(output_dir, "waterIntersectCheck_nearTable.csv"))

# Run filter_traverseSea_subset.R at tejo7 #ADhere; needs to assign coordinates to the nearTable for further processing in TEJO7
# water_intersection %>% filter((is.na(water_int) & waterCells > 0))  # yields 130 records; mostly due to seasonal water presence detected in the land cover map.
# Intersect 'retained_fireGraph_convHull' with 'permWater_vector'
# Sync coord. reference system is necessary to prevent false negative: no intersection between the two
# permWater_vector <- permWater_vector %>% project(retained_fireGraph_convHull)
# waterIntersecting_convHull <- retained_fireGraph_convHull %>% terra::intersect(permWater_vector)
# # Assign attribute table
# values(retained_fireGraph_convHull) <- data.frame(id = retained_fireGraph_IDs, km_area = expanse(retained_fireGraph_convHull, unit = "km"))
# Filter out networks with convHull area < 1 km
# retained_fireGraph_convHull <- retained_fireGraph_convHull %>% subset(km_area >= 1, NSE = TRUE)


# 6. To extract the ignitial and spread points of the retained fire network.
# a. Update the 'retained_fireGraph_IDs' to reflect the last filtered IDs
retained_fireGraph_IDs <- retained_fireGraph_convHull$id
fireGraph_decompose_ret <- fireGraph_decompose[retained_fireGraph_IDs]
# b. Identify the ignitial points and the spread points
# identify the potential ignitial points per graph
ignitialPoints_filtered <- fireGraph_decompose_ret %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
ignitialPoints_filtered_FID <- ignitialPoints_filtered %>% unlist() %>% names() %>% as.numeric() %>% unique()

# identify the spread points
spreadPoints_filtered <- fireGraph_decompose_ret %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") != 0])
spreadPoints_filtered_FID <- spreadPoints_filtered %>% unlist() %>% names() %>% as.numeric() %>% unique()
# test the accuracy of the identified ignitial FIDs by testing if there are any rows with the ignitial FID as the NEAR_ instead of the IN_
# if correct, then there should not be.
check_nearTable <- nearTable_allPoints %>% filter(NEAR_FID %in% ignitialPoints_filtered_FID)

# 7. Generate and export ignitial and spread point spatial vector
# ignitial points
n_point_coordinates <- attribute_allPoints %>% filter(pointID %in% ignitialPoints_filtered_FID) %>% select(LONGITUDE, LATITUDE, pointCategory, pointID)
n_points <- n_point_coordinates %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84")
n_points %>% writeVector("output/ignitialPoints_filtered.shp", overwrite = TRUE)
# spread points
n_point_coordinates <- attribute_allPoints %>% filter(pointID %in% spreadPoints_filtered_FID) %>% select(LONGITUDE, LATITUDE, pointCategory, pointID)
n_points <- n_point_coordinates %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84")
n_points %>% writeVector("output/spreadPoints_filtered.shp", overwrite = TRUE)

# Checking the consistency of the filtered point locations with the spatial location of the convexHull
# n_point_coordinates <- attribute_allPoints %>% filter(pointID %in% ignitialPoints_filtered_FID) %>% select(LONGITUDE, LATITUDE, pointCategory, pointID)
# n_points <- n_point_coordinates %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84")
# identical(n_points, terra::intersect(n_points, retained_fireGraph_convHull))
# # AD omit later
# testGraph <- fireGraph[[1]]
# btw <- degree(testGraph, mode = "in")
# # want to display: the origin, the most in and out
# btw_dum <- (btw - (btw-1))*3
# btw_dum[btw == 0] <- 14
# plot(testGraph, vertex.size = btw_dum, vertex.label = NA, edge.arrow.size = 0.2, edge.color = "gray", vertex.color = "steelblue", layout = layout_with_lgl)

#
