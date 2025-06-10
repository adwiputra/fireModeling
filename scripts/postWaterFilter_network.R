# Processing MODIS and VIIRS points into aspatial graph
# AD
# first written: 23/06/2023; updated 13/01/2025

# 0. LIBRARIES======
library(igraph)
library(data.table)
library(terra)
library(hms)
library(tidyverse)


# 1. INPUTS=====
# a. Load all output from 'plot_convHull_network.R'
load("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/partB_waterFiltering_2025.RData")
# b. Load output from 'filter_traverSea_subset.R'
waterChecked_nearTable <- paste0(output_dir, "fromTejo_transfer/waterChecked_mergeParts_nearTable_w84.rds") %>% readRDS()
attribute_allPoints <- read_rds("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/pointAttributes_all.rds")
# c. 2014 land cover
lc2014_raster <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v2.0.7.tif" %>% rast()
# d. Define output_dir
output_dir <- "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/"

# 2. FUNCTIONS========
# a. A function to filter decomposed graph according to the date(s) of the ignition points # revised 10/06/2025
graph_temporalFilter <- function(in_graph = fireGraph_decompose){
  ignitionPoints_graphs <- in_graph %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
  n_ignitions_graph <- lapply(X = ignitionPoints_graphs, length) %>% unlist()
  ignitionPoints_id_unlist <- ignitionPoints_graphs %>% unlist() %>% names() %>% as.numeric()
  # Generate data.frame
  ignitions_df <- data.frame(ignition_pointID = ignitionPoints_id_unlist, graph_id = rep(1:length(in_graph), n_ignitions_graph))
  # join with 'attribute_allPoints'
  ignitions_df <- attribute_allPoints %>% rename_at(1, ~"ignition_pointID") %>% select(ignition_pointID, ACQ_DATE) %>% right_join(ignitions_df) %>% arrange(graph_id)
  ignitions_df <- ignitions_df %>% group_by(graph_id) %>% summarise(earliest_origin = min(ACQ_DATE), last_origin = max(ACQ_DATE))
  # filter based on date
  graph_id_toKeep <- ignitions_df %>% filter(earliest_origin >= as.POSIXct("2015/01/01", format = "%Y/%m/%d", tz = "UTC") & last_origin < as.POSIXct("2016/01/01", format = "%Y/%m/%d", tz = "UTC")) %>% select(graph_id) %>% unique() %>% pull()
  # if(before) graph_id_toKeep <- ignitions_df %>% filter(ACQ_DATE < temporal_threshold) %>% select(graph_id) %>% unique() %>% pull() else graph_id_toKeep <- ignitions_df %>% filter(ACQ_DATE >= temporal_threshold) %>% select(graph_id) %>% unique() %>% pull()
  
  # subset input graph
  return(in_graph[graph_id_toKeep]) # return graph
}

# 3. PREPROCESSING======
# a. Generate filtered nearTable by binding the nearTables
nonWaterIntersecting_nearTable <- onceFiltered_nearTable %>% setdiff(waterIntersectCheck_nearTable)
vars_keep <- names(nonWaterIntersecting_nearTable)
waterChecked_nearTable <- waterChecked_nearTable %>% select(all_of(vars_keep))
# solve an issue with unintentionally converted 'inNear_tDiff'
waterChecked_nearTable <- waterChecked_nearTable %>% mutate(inNear_tDiff = NEAR_TIME-IN_TIME)
# binding nearTables
nonWaterIntersecting_nearTable <- nonWaterIntersecting_nearTable %>% bind_rows(waterChecked_nearTable)
# remove all except nonWaterIntersecting_nearTable
rm(list = setdiff(ls(), c("nonWaterIntersecting_nearTable","graph_temporalFilter", "attribute_allPoints", "lc2014_raster", "output_dir")))
gc()
# check for duplicates
nonWaterIntersecting_nearTable <- as.data.table(nonWaterIntersecting_nearTable) # for efficiency, we convert it into a data.table object
# duplicated_check <- duplicated(nonWaterIntersecting_nearTable) #ADrun
# 4. PROCESSING=======
# a. reconstruct graph based on the nearTable# Generate graph
fireGraph = graph_from_data_frame(nonWaterIntersecting_nearTable[, c("IN_FID", "NEAR_FID")], directed = TRUE)
# b. decompose the graph collection while removing isolate vertices
fireGraph_decompose <- fireGraph %>% decompose(min.vertices = 3) # 3 original; 2 is sufficient to remove isolates, but produce too many small graphs
verticesCounts <- sapply(fireGraph_decompose, vcount)# can also use function(grph) V(grph) %>% length())
# c. filter decomposed graph according to the ignition point dates; omitting graphs with origin points occurring not in 2015.
# The temporal thresholds were hardcoded in the function.
fireGraph_decompose <- graph_temporalFilter(fireGraph_decompose)
# d. convex hull generation, apply filtering again
# scripts for this section was taken from 'plot_convHull_network.R'=======
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

values(convHull_all) <- data.frame(id = 1:n, km_area = expanse(convHull_all, unit = "km"))
gc()
# 4. Run zonal statistics to filter out convexHulls with >= 50% bareland (value = 200) OR urban area (value == 190) as well as networks that occur on seas========
# a. Filter out networks with convHull area < 1 km
large_fireGraph_convHull <- convHull_all %>% subset(km_area >= 1, NSE = TRUE)
# retained_fireGraph_convHull %>% writeVector("output/convexHull_filteredFireNetwork.shp", overwrite = TRUE)

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
retained_fireGraph_convHull %>% writeVector(paste0(output_dir, "convexHull_final2015fireNetwork.shp"))
# d. identify ignition and spread points
# Filter the final graph subset
# filter the nearTable based on the retained_fireGraph_IDs
# Extract the point ids that compose the retained_fireGraph_IDs
retained_fireGraph_IDs <- retained_fireGraph_convHull$id # retained_fireGraph_IDs changed
finalGraph_decompose <- fireGraph_decompose[retained_fireGraph_IDs]
retained_fireGraph_IDs <- sort(retained_fireGraph_IDs)
# Extract point informations from 'finalGraph_decompose'
# ignition points
ignitionPoints_finalGraphs <- finalGraph_decompose %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
n_ignitions_graph <- lapply(X = ignitionPoints_finalGraphs, length) %>% unlist()
ignitionPoints_id_unlist <- ignitionPoints_finalGraphs %>% unlist() %>% names() %>% as.numeric()
# spread points
spreadPoints_finalGraphs <- finalGraph_decompose %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") != 0])
n_spreads_graph <- lapply(X = spreadPoints_finalGraphs, length) %>% unlist()
spreadPoints_id_unlist <- spreadPoints_finalGraphs %>% unlist() %>% names() %>% as.numeric()
# Generate data.frame
ignitionSpread_df <- data.frame(pointID = ignitionPoints_id_unlist, graph_id = rep(retained_fireGraph_IDs, n_ignitions_graph), pointCategory = "ignition") %>% bind_rows(data.frame(pointID = spreadPoints_id_unlist, graph_id = rep(retained_fireGraph_IDs, n_spreads_graph), pointCategory = "spread"), .)
# join with 'attribute_allPoints'
ignitionSpread_df <- attribute_allPoints %>% select(-pointCategory) %>% right_join(ignitionSpread_df) %>% arrange(graph_id)
# Generate a summary data.frame for a potential side analysis of the relationship between the number of ignition and spread points or lackthereof.
ignitionSpread_summary_df <- data.frame(n_ignitions = n_ignitions_graph, n_spread = n_spreads_graph, graph_id = retained_fireGraph_IDs)
# save all results
save.image(paste0(output_dir, "final_graphAnalysis.RData"))
fwrite(ignitionSpread_summary_df, file = paste0(output_dir, "ignitionSpread_summary.csv"))
# Enforce coordinate uniqueness
# check if all coordinates are exclusive
if(identical(nrow(ignitionSpread_df), nrow(unique(ignitionSpread_df[, c("LONGITUDE", "LATITUDE")])))) {
  print("no duplicated point coordinates")
  # ignitionSpread_df  %>% write.csv(outputPointTable_name)
} else{
  print("Found some duplicated point coordinates")
  # resolve duplicated point coordinates by only retaining the ignitial points of such cases
  # identify pointID with duplicated coordinates
  duplicatedPointIDs <- ignitionSpread_df %>% filter(duplicated(ignitionSpread_df[, c("LONGITUDE", "LATITUDE")])) %>% select(LONGITUDE, LATITUDE) %>% left_join(ignitionSpread_df)
  duplicatedPointIDs %>% fwrite(paste0(output_dir, "duplicatedLocation_ignitionSpread.csv"), row.names = FALSE)
  # remove the 42 rows that contain duplicated LON LAT from the ignitionSpread_df
  ignitionSpread_df <- ignitionSpread_df %>% setdiff(duplicatedPointIDs)
  # solve the duplicatedPointIDs by slicing the ignitialPoint row, if any
  duplicatedPointIDs <- duplicatedPointIDs %>% group_by(LONGITUDE, LATITUDE) %>% arrange(pointCategory) %>% slice(1) %>% ungroup()
  ignitionSpread_df <- ignitionSpread_df %>% bind_rows(duplicatedPointIDs)
  if(identical(nrow(ignitionSpread_df), nrow(unique(ignitionSpread_df[, c("LONGITUDE", "LATITUDE")])))) {
    print("Solved the duplicated point coordinates")
    # convert date and time format into the format recognizable by ArcGIS Pro
    ignitionSpread_df <- ignitionSpread_df %>% mutate(temporalString = paste0(as.character(ACQ_DATE), " ", as.character(as_hms(ACQ_TIME)))) 
    ignitionSpread_df %>% vect(., geom = c("LONGITUDE", "LATITUDE"), crs = "+proj=longlat +datum=WGS84") %>% writeVector(paste0(output_dir, "ignitionSpread_machLearn_input.shp"))
    ignitionSpread_df %>% fwrite(paste0(output_dir, "ignitionSpread_machLearn_input.csv"), row.names = FALSE)
    
  } else{
    stop("Some duplicates still exist. Please check")
  }
}
# ADhere
# ADrun test if igraph's integrity is maintained despite the skipped rows in a data.frame
# t1 <- data.frame(from = c(1, 2, 5, 3, 9, 2), to = c(2, 3, 6, 4, 10, 11))
# t_graph <- graph_from_data_frame(t1, directed = TRUE)
# t_decompose <- t_graph %>% decompose()