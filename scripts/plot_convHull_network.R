# Script to map the network spatially and apply additional filtering to obtain the ignitial points and the spread from the mapped fire networks
# AD
# 11/04/2024

# 0. LIBRARIES====
library(terra)
library(igraph)
library(tidyverse)

# 1. INPUTS========
# a. Since there is no edge intersecting major water bodies, we can load the .RData from the previous analysis
load("largeData_loaded.RData")
lc2014_raster <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v2.0.7.tif" %>% rast()
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
convHull_all %>% writeVector("output/convexHull_allFireNetwork.shp")

# 4. Run zonal statistics to filter out convexHulls with >= 50% bareland (value = 200) OR urban area (value == 190) as well as networks that occur on seas========
# a. total non-NoData cells per polygon
lc2014_raster <- lc2014_raster %>% crop(convHull_all)
lc2014_processedRaster <- (lc2014_raster != 0) & (lc2014_raster != 210)
denom_table_nCells <- lc2014_processedRaster %>% terra::extract(convHull_all, fun = sum)
# b. total barren or urban area
lc2014_processedRaster <- (lc2014_raster == 200) | (lc2014_raster == 190)
numerator_table_nCells <- lc2014_processedRaster %>% terra::extract(convHull_all, fun = sum)
# c. Obtain barren or urban area proportion
urbanBarrenProp_table_nCells <- denom_table_nCells %>% rename_at(2, ~"denom") %>% left_join(numerator_table_nCells, by = "ID") %>% rename_at(3, ~"nume") %>%  mutate(nCell_prop = nume/denom)
# d. Filtering
retained_fireGraph_IDs <- urbanBarrenProp_table_nCells %>% filter(denom > 4) %>% filter(nCell_prop < 0.1) %>% select(ID) %>% pull()
# 0.4, 0.3333 not enough to filter out all false positives; 0.1 provides good balance (OKI is retained)
# 5. Export retained convHull
retained_fireGraph_convHull <- convHull_all[retained_fireGraph_IDs]
# Assign attribute table
values(retained_fireGraph_convHull) <- data.frame(id = retained_fireGraph_IDs, km_area = expanse(retained_fireGraph_convHull, unit = "km"))
# Filter out networks with convHull area < 1 km
retained_fireGraph_convHull <- retained_fireGraph_convHull %>% subset(km_area >= 1, NSE = TRUE)
retained_fireGraph_convHull %>% writeVector("output/convexHull_filteredFireNetwork.shp", overwrite = TRUE)

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
