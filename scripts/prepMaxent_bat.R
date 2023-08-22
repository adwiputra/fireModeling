# Pre-maxent filtering to only include points, inclusive of both ignitial and spread points, that:
# 1. have no NA values at any of the covariates
# 2. belong to different lat lon

# Related to point no. 2, when ignitial and spread point fall in the same lat lon grid, prioritize ignitial for it has less number of points and it is the main object of interest

# 0. LIBRARIES======
library(tidyverse)

# 1. INPUTS====
testProp <- 0.3
load("~/analysis/fireModeling/largeData_loaded.RData") # load the Rdata containing the previously processed data: the outputs of "networkCreate_andPrune.R"
# the table of extracted covariate and location values at point locations
covGrid_extract <- "D:/dwiputra/extractCov_pointsAll.csv" %>% read.csv() %>% select(-1) # removed OID to avoid confusion given the unmatching OID and pointID
# load the attribute_allPoint data source to allow join for ID synchronizing
dataDir <- "D:/dwiputra/"
fid_lookup <- read.csv(paste0(dataDir, "allMerge_fireJan14dec16.csv"))
# test if the FID match 2 lines below. THEY DIDN'T
# matchCheck <- attribute_allPoints %>% select(pointID, LATITUDE, LONGITUDE, BRIGHTNESS) %>% rename_at(vars(LATITUDE, LONGITUDE), ~c("y_", "x_"))
# matchCheck <- covGrid_extract_noNA %>% select(OID, LATITUDE, LONGITUDE, BRIGHTNESS) %>%  rename_at(1, ~"pointID") %>% left_join(matchCheck, by = "pointID") %>% mutate(xyDiff_test = LATITUDE+LONGITUDE-y_-x_)

# 2. Preprocessing=======
# joining the older attribute_allPoints with the new table that contains the predictor values
attribute_allPoints_withCov <- fid_lookup %>% full_join(covGrid_extract) # full join will tell if there is unmatching point
# filtering out all of the rows that contain NA
attribute_allPoints_withCov_withoutNA <- attribute_allPoints_withCov %>% filter(!if_any(everything(), is.na))
# rename the OID column into pointID
attribute_allPoints_withCov_withoutNA <- attribute_allPoints_withCov_withoutNA %>% rename_at(1, ~"pointID")

# 3. Processing====
# implementing objective no. 2 to remove duplicates, i.e. different points that fall within the same latgrid and longrid
# assign all points the spread and ignitial labels
# merge based on the LATgrd and LONgrd with the priority rule: ignitial > spread
# identify duplicated point IDs within the same LATgrd LONgrd
attribute_allPoints_withCov_withoutNA <- attribute_allPoints_withCov_withoutNA %>% mutate(pointCategory = case_when(pointID %in% spreadPoints_FID ~ "spread", TRUE ~ NA)) %>% mutate(pointCategory = case_when(pointID %in% ignitialPoints_FID ~ "ignitial", TRUE ~ pointCategory))
# merge based on the LATgrd and LONgrd with the priority rule: ignitial > spread
gridAggregated_duplicated <- attribute_allPoints_withCov_withoutNA %>% filter(duplicated(attribute_allPoints_withCov_withoutNA[, c("LONgrd", "LATgrd")])) %>% select(LONgrd, LATgrd) %>% unique() %>% left_join(attribute_allPoints_withCov_withoutNA)
# resolve duplicated grid coordinates by only retaining the ignitial points of such cases
# identify pointID with duplicated coordinates
# duplicatedPointIDs <- outPointTable %>% filter(duplicated(outPointTable[, c("LONgrd", "LATgrd")])) %>% select(LONgrd, LATgrd) %>% left_join(outPointTable)
# remove the 42 rows that contain duplicated LON LAT from the outPointTable
gridAggregated_noDuplicates <- attribute_allPoints_withCov_withoutNA %>% setdiff(gridAggregated_duplicated)
# solve the duplicatedPointIDs by slicing the ignitialPoint row, if any
gridAggregated_duplicated <- gridAggregated_duplicated %>% group_by(LONgrd, LATgrd) %>% arrange(pointCategory) %>% filter(!is.na(pointCategory)) %>% slice(1) %>% ungroup()
# clean occurrence table
cleanOcc_table <- gridAggregated_duplicated %>% bind_rows(gridAggregated_noDuplicates)
# separate into ignitial and spread
ignitial_cleanTable <- cleanOcc_table %>% filter(pointCategory == "ignitial")
spread_cleanTable <- cleanOcc_table %>% filter(pointCategory == "spread")

# train Test partition for occurrenceData
occurrenceData <- ignitial_cleanTable %>% select(LONgrd, LATgrd) %>% mutate(Species = "fire") %>% rename_at(vars(LONgrd, LATgrd), ~c("Long", "Lat")) %>% select(Species, Long, Lat)
# sample the set aside
testIgnitials <- round(testProp * nrow(occurrenceData))
testIgnitials <- occurrenceData %>% sample_n(size = testIgnitials, replace = FALSE)
# define the trainIgnitials
trainIgnitials <- occurrenceData %>% setdiff(testIgnitials)
# export for maxent.jar run
trainIgnitials %>% write.csv("C:/Users/dwiputra/Downloads/maxent/trainLatLon.csv", row.names = FALSE)
testIgnitials %>% write.csv("C:/Users/dwiputra/Downloads/maxent/testLatLon.csv", row.names = FALSE)
