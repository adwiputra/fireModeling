# 0. libraries======
# # paralleling stuff for the first three lines
library(foreach)
library(data.table)
library(doParallel)
library(tcltk)
library(sf)
library(terra)
library(tidyverse)
# 
# cores = detectCores()
# cl = makeCluster(cores, output = "")
# registerDoParallel(cl)
# # 1. Inputs========
setwd("C:/Users/yowargan/Documents/fin_2025/")
# mergeParts_nearTable <- readRDS("C:/Users/yowargan/Documents/eoy_2023/analysis/fireModeling-main/data/toTEJO7_waterIntersection/merged_nearTable_onlyCoords_labelledWater.rds")
mergeParts_nearTable_water <- fread("C:/Users/yowargan/Documents/fin_2025/toTejo_transfer/waterIntersectCheck_nearTable.csv")
# ADchecked Test the consistency of the OID_s in 'OID_' and 'OID_near'
# test_OIDs <- mergeParts_nearTable %>% select(OID_) %>% unique() %>% slice(1:5) %>% pull()
# from_OIDs <- mergeParts_nearTable %>% filter(OID_ %in% test_OIDs) %>% select(OID_, LATITUDE, LONGITUDE) %>% unique()
# to_OIDs <- mergeParts_nearTable %>% filter(OID_near %in% test_OIDs) %>% select(OID_near, LAT_near, LON_near) %>% unique()
# names(to_OIDs) <- names(from_OIDs)
# bound_OIDs <- from_OIDs %>% rbind(to_OIDs) %>% unique()
# dataDir <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/"
# outputDir <- paste0(dataDir, "preprocessing/")
# outputTable <- read.csv(paste0(outputDir, "hIndex_majority_perGrid_v2.csv"))
# 
# # Join the outputTable to the vector file
# gridVector <- st_read("D:/Documents/otherOpps/YSSP/projects/analysis/mapping/fireAnalysis/temp/temp_pairWise_refinedExtent.shp") %>% left_join(outputTable)
# gridVector_landOnly <- gridVector %>% filter(!is.na(gridcode) & !is.na(classProp))
# st_write(gridVector_landOnly, "D:/Documents/otherOpps/YSSP/projects/analysis/mapping/fireAnalysis/temp/landOnlyGrids_refined_v2_r.shp", driver = "ESRI Shapefile", append = FALSE)


# ADCheck This line is to indicate that the run will be conducted on a subset of the data

# Processing===========
crdref <- "+proj=longlat +datum=WGS84"
# permanent water layer to apply intersect with
site_permanentWater_polygon <- vect("C:/Users/yowargan/Documents/fin_2025/toTejo_transfer/perm_waterBodies_clipped.shp")

gc()
# check the accuracy
# mergeParts_nearTable column names: OBJECTID, LATITUDE, LONGITUDE, LAT_near, LON_near
# split the data
totalRows_n <- nrow(mergeParts_nearTable_water)
runs_n <- 10000
split_n <- ceiling(totalRows_n/runs_n)
group_id <- rep(1:runs_n, each = split_n)[1:nrow(mergeParts_nearTable_water)]
# create a blanko table to record the intersecting edges' OBJECTIDs
omitEdge_OBJECTID <- data.frame(OBJECTID = 0)
# insert dummy looping variables
mergeParts_nearTable_water <- mergeParts_nearTable_water %>% mutate(group_id = group_id)
for(r in 1:runs_n){
  print(paste0("Running ", r, "th run out of ", runs_n, "th runs..."))
  # needs to transform the data frame into matrix: id, lon, lat where id = OBJECTID; lon = LONGITUDE and LON_near; lat = LATITUDE and LAT_near
  lonlat <- mergeParts_nearTable_water %>%  filter(group_id == r) %>% select(OBJECTID, LONGITUDE, LATITUDE) %>% rename_at(1:3, ~c("object", "x", "y"))
  lonlat <- mergeParts_nearTable_water %>%  filter(group_id == r) %>% select(OBJECTID, LON_near, LAT_near) %>% rename_at(1:3, ~c("object", "x", "y")) %>% 
    rbind(lonlat) %>% arrange(object) %>% mutate(part = 1) %>% select(object, part, x, y) %>% as.matrix()
  gc()
  slicedTable <- mergeParts_nearTable_water %>% filter(group_id == r) %>% select(OBJECTID) %>% data.frame()
  allEdge_w84 <- vect(lonlat, type="lines", crs=crdref,  atts = slicedTable) #  atts = slicedTable
  intersectingEdge <- allEdge_w84 %>% terra::intersect(site_permanentWater_polygon) # %>% writeVector("data/intersectWater_a.shp", filetype = "ÃSRI Shapefile")
  # saveRDS(intersectingEdge, paste0("H:/intersect_correct_", r))
  # Record the OBJECTID of the intersecting lines for removal from mergeParts_nearTable
  if(nrow(values(intersectingEdge)) != 0) omitEdge_OBJECTID <- values(intersectingEdge) %>% select(OBJECTID) %>% unique() %>% rbind(omitEdge_OBJECTID)
  gc()
}
# Save omitEdge_OBJECTID
omitEdge_OBJECTID <- omitEdge_OBJECTID %>% filter(OBJECTID != 0)
saveRDS(omitEdge_OBJECTID, "waterIntersect_OBJECTID_w84.rds")

# Filter mergeParts_nearTable
filtered_mergeParts_nearTable <- mergeParts_nearTable_water %>% filter(!OBJECTID %in% omitEdge_OBJECTID$OBJECTID)
saveRDS(filtered_mergeParts_nearTable, "waterChecked_mergeParts_nearTable_w84.rds")
# part b
# mergeParts_nearTable column names: OBJECTID, LATITUDE, LONGITUDE, LAT_near, LON_near
# needs to transform the data frame into matrix: id, lon, lat where id = OBJECTID; lon = LONGITUDE and LON_near; lat = LATITUDE and LAT_near
# lonlat <- mergeParts_nearTable %>% slice((nrow(mergeParts_nearTable)/2): nrow(mergeParts_nearTable)) %>% select(OBJECTID, LONGITUDE, LATITUDE) %>% rename_at(1:3, ~c("id", "lon", "lat"))
#lonlat <- mergeParts_nearTable %>% slice((nrow(mergeParts_nearTable)/2): nrow(mergeParts_nearTable)) %>%  select(OBJECTID, LON_near, LAT_near) %>% rename_at(1:3, ~c("id", "lon", "lat")) %>% rbind(lonlat) %>% as.matrix()
#gc()
#allEdge_w84 <- vect(lonlat, type="lines", crs=crdref) %>% rbind(allEdge_w84) %>% vect(atts = bind_rows(mergeParts_nearTable_a, mergeParts_nearTable_b))
# b. use terra function to construct lines that connect all pairs of points that consist of at least one point located near permanent waterbodies
#overlap_allEdge_permanentWaster_w84 <- allEdge_w84 %>% intersect(site_permanentWater_polygon_w84)
# c. overlay the lines with the polygon and remove edges intersecting the permanent waterbodies from the nearTable
