# 0. libraries======
# paralleling stuff for the first three lines
library(foreach)
library(doParallel)
library(tcltk)
library(sf)
library(terra)
library(tidyverse)

cores = detectCores()
cl = makeCluster(cores, output = "")
registerDoParallel(cl)
# 1. Inputs========
dataDir <- "D:/Documents/otherOpps/YSSP/projects/analysis/data/spatial/covariates/landCover_CCI/"
outputDir <- paste0(dataDir, "preprocessing/")
outputTable <- read.csv(paste0(outputDir, "hIndex_majority_perGrid_v2.csv"))

# Join the outputTable to the vector file
gridVector <- st_read("D:/Documents/otherOpps/YSSP/projects/analysis/mapping/fireAnalysis/temp/temp_pairWise_refinedExtent.shp") %>% left_join(outputTable)
gridVector_landOnly <- gridVector %>% filter(!is.na(gridcode) & !is.na(classProp))
st_write(gridVector_landOnly, "D:/Documents/otherOpps/YSSP/projects/analysis/mapping/fireAnalysis/temp/landOnlyGrids_refined_v2_r.shp", driver = "ESRI Shapefile", append = FALSE)


# ADCheck This line is to indicate that the run will be conducted on a subset of the data

# Processing===========
# a. Filter the nearTable to only include points located near (1.5 km) permanent waterbodies
# check the accuracy
crdref <- "+proj=longlat +datum=WGS84"
site_permanentWater_polygon <- vect(system.file("data/globalPermWater_polyMerge_patched.shp", package="terra"))
site_permanentWater_polygon_w84 <- site_permanentWater_polygon_w84 %>% project(crdref)
# b. use terra function to construct lines that connect all pairs of points that consist of at least one point located near permanent waterbodies
# mergeParts_nearTable column names: OBJECTID, LATITUDE, LONGITUDE, LAT_near, LON_near
# needs to transform the data frame into matrix: id, lon, lat where id = OBJECTID; lon = LONGITUDE and LON_near; lat = LATITUDE and LAT_near
lonlat <- mergeParts_nearTable %>% select(OBJECTID, LONGITUDE, LATITUDE) %>% rename_at(1:3, ~c("id", "lon", "lat"))
lonlat <- mergeParts_nearTable %>% select(OBJECTID, LON_near, LAT_near) %>% rename_at(1:3, ~c("id", "lon", "lat")) %>% rbind(lonlat) %>% as.matrix()
# lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7, -115.4)
# lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6, 37.6)
# lonlat <- cbind(id=rep(1:4, each = 2), part=1, lon, lat)
allEdge_w84 <- vect(lonlat, type="lines", crs=crdref)
overlap_allEdge_permanentWaster_w84 <- allEdge_w84 %>% intersect(site_permanentWater_polygon_w84)
# c. overlay the lines with the polygon and remove edges intersecting the permanent waterbodies from the nearTable

# Post-processing==========
# d. Export