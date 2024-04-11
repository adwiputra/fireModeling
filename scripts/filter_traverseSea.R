# 0. libraries======
# Script to run the table joins that provide the nearTable point coordinates
# 29/11/2023
#AD

# 0. LIBRARIES====
library(data.table)
# paralleling stuff for the first three lines
library(foreach)
library(doParallel)
library(tcltk)

library(sf)
library(terra)
library(tidyverse)

cores = detectCores()-5
cl = makeCluster(cores, output = "")
registerDoParallel(cl)

# 1. INPUTS=====
coordsLookup <- "data/table/allMerge_fireJan14dec16.csv" %>% fread()
oidName <- c("OID_")
coordsLookup <- coordsLookup %>% rename_at(1, ~oidName)
majorNearTable <- "data/table/nearTable_allMerge_fireJan14dec16.csv" %>% fread()

nearTable_headerNames <- majorNearTable %>% names()

# 2. PREPROCESSING====
# NEED TO REPAIR OID AS WELL
# IN_FID (OID_), LATITUDE, LONGITUDE
# NEAR_FID (OID_), LATITUDE, LONGITUDE
inPart_nearTable <- majorNearTable %>% select(IN_FID) %>% rename_at(1, ~c("OID_")) %>% left_join(coordsLookup)
nearPart_nearTable <- majorNearTable %>% select(NEAR_FID) %>% rename_at(1, ~"OID_") %>% left_join(coordsLookup)

# checking the consistency of the ordering
if(identical(majorNearTable$IN_FID, inPart_nearTable$OID_) & identical(majorNearTable$NEAR_FID, nearPart_nearTable$OID_)) print("All point IDs are consistently arranged according to the source")

# Renaming columns before merging
nearPart_nearTable <- nearPart_nearTable %>% rename_at(vars(OID_, LATITUDE, LONGITUDE), ~c("OID_near", "LAT_near", "LON_near"))
mergeParts_nearTable <- nearPart_nearTable %>% bind_cols(inPart_nearTable)

# 3. Processing====
# Generate spatialLinesDataframe from mergeParts_nearTable

# Exporting
# mergeParts_nearTable %>% mutate(OID = 1:nrow(mergeParts_nearTable)) %>% write.csv("data/merged_nearTable_withCoords.csv", row.names = FALSE)
rm(inPart_nearTable, nearPart_nearTable, majorNearTable)
gc()
mergeParts_nearTable <- mergeParts_nearTable %>% mutate(OBJECTID = 1:nrow(mergeParts_nearTable)) %>% select(OBJECTID, OID_, LATITUDE, LONGITUDE, OID_near, LAT_near, LON_near) # %>% fwrite("data/merged_nearTable_onlyCoords.csv")
# clearing up memory space
gc()

# Processing===========
# a. Filter the nearTable to only include points located near (1.5 km) permanent waterbodies
# permanent water layer to apply intersect with
site_permanentWater_polygon <- vect("globalPermWater_polyMerge_patched_w84.shp") # %>% project(crdref)

# check the accuracy
crdref <- "+proj=longlat +datum=WGS84"
# mergeParts_nearTable column names: OBJECTID, LATITUDE, LONGITUDE, LAT_near, LON_near
# split the data
totalRows_n <- nrow(mergeParts_nearTable)
runs_n <- 100
split_n <- ceiling(totalRows_n/runs_n)
group_id <- rep(1:100, each = split_n)[1:nrow(mergeParts_nearTable)]
# insert dummy looping variables
mergeParts_nearTable <- mergeParts_nearTable %>% mutate(group_id = group_id)
for(r in 1:runs_n){
  print(paste0("Running ", r, "th run out of ", runs_n, "th runs..."))
  # needs to transform the data frame into matrix: id, lon, lat where id = OBJECTID; lon = LONGITUDE and LON_near; lat = LATITUDE and LAT_near
  lonlat <- mergeParts_nearTable %>%  filter(group_id == r) %>% select(OBJECTID, LONGITUDE, LATITUDE) %>% rename_at(1:3, ~c("id", "lon", "lat"))
  lonlat <- mergeParts_nearTable %>%  filter(group_id == r) %>% select(OBJECTID, LON_near, LAT_near) %>% rename_at(1:3, ~c("id", "lon", "lat")) %>% 
    rbind(lonlat) %>% as.matrix()
  gc()
  # lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7, -115.4)
  # lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6, 37.6)
  # lonlat <- cbind(id=rep(1:4, each = 2), part=1, lon, lat)
  slicedTable <- mergeParts_nearTable %>% filter(group_id == r) %>% select(OBJECTID)
  allEdge_w84 <- vect(lonlat, type="lines", crs=crdref, atts = slicedTable)
  intersectingEdge <- allEdge_w84 %>% terra::intersect(site_permanentWater_polygon) # %>% writeVector("data/intersectWater_a.shp", filetype = "ËSRI Shapefile")
  saveRDS(intersectingEdge, paste0("H:/intersect_correct_", r))
  saveRDS(allEdge_w84, paste0("H:/allEdge_w84_", r))
}
# part b
# mergeParts_nearTable column names: OBJECTID, LATITUDE, LONGITUDE, LAT_near, LON_near
# needs to transform the data frame into matrix: id, lon, lat where id = OBJECTID; lon = LONGITUDE and LON_near; lat = LATITUDE and LAT_near
lonlat <- mergeParts_nearTable %>% slice((nrow(mergeParts_nearTable)/2): nrow(mergeParts_nearTable)) %>% select(OBJECTID, LONGITUDE, LATITUDE) %>% rename_at(1:3, ~c("id", "lon", "lat"))
lonlat <- mergeParts_nearTable %>% slice((nrow(mergeParts_nearTable)/2): nrow(mergeParts_nearTable)) %>%  select(OBJECTID, LON_near, LAT_near) %>% rename_at(1:3, ~c("id", "lon", "lat")) %>% rbind(lonlat) %>% as.matrix()
gc()
allEdge_w84 <- vect(lonlat, type="lines", crs=crdref) %>% rbind(allEdge_w84) %>% vect(atts = bind_rows(mergeParts_nearTable_a, mergeParts_nearTable_b))
# b. use terra function to construct lines that connect all pairs of points that consist of at least one point located near permanent waterbodies
overlap_allEdge_permanentWater_w84 <- allEdge_w84 %>% intersect(site_permanentWater_polygon_w84)
# c. overlay the lines with the polygon and remove edges intersecting the permanent waterbodies from the nearTable

# Post-processing==========
# d. Export