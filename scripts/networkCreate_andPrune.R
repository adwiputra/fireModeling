# Processing MODIS and VIIRS points into aspatial graph
# AD
# first written: 23/06/2023

# 0. LIBRARIES======
library(tidyverse)
library(igraph)

# 1. INPUTS====
dataDir <- "D:/dwiputra/"
nearTable_allPoints <- read.csv(paste0(dataDir, "nearTable_allMerge_fireJan14dec16.csv"))
attribute_allPoints <- read.csv(paste0(dataDir, "allMerge_fireJan14dec16.csv"))

# 2. OUTPUT PARAMETERIZATION====
outputPointTable_name <- paste0(dataDir, "pointAttributes_all.csv")

# PRPROCESSING=========
# time info preprocessing
attribute_allPoints <- attribute_allPoints %>% mutate(ACQ_TIME = sprintf("%04d", ACQ_TIME)) %>% mutate(ACQ_TIME = paste0(substring(ACQ_TIME, 1, 2), ":", substring(ACQ_TIME, 3, 4)))
# Convert the ACQ_TIME into time difference
attribute_allPoints <- attribute_allPoints %>% mutate(ACQ_TIME = as.difftime(ACQ_TIME, format = "%H:%M"))
                                                        
# mutate ACQ_Date
attribute_allPoints <- attribute_allPoints %>% mutate(ACQ_DATE = as.POSIXct(ACQ_DATE, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")) %>% mutate(ACT_TIME = ACQ_DATE + ACQ_TIME)

# form a time lookup tbl
acqTime_lookup <- attribute_allPoints %>% select(1, ACT_TIME) %>% rename_at(1, ~"OID")
# renaming columns of the nearTable_allPoints
nearTable_allPoints <- acqTime_lookup %>% rename_at(1, ~paste0(names(nearTable_allPoints)[2])) %>% right_join(nearTable_allPoints) %>% rename(IN_TIME = ACT_TIME)
nearTable_allPoints <- acqTime_lookup %>% rename_at(1, ~paste0(names(nearTable_allPoints)[3])) %>% right_join(nearTable_allPoints) %>% rename(NEAR_TIME = ACT_TIME)

# calculate time difference. Convention: time difference = NEAR_TIME - IN_TIME. Thus, a positive time difference occurs when NEAR_ is later than IN_
nearTable_allPoints <- nearTable_allPoints %>% mutate(inNear_tDiff = NEAR_TIME - IN_TIME)
# filter out edges with negative time differences
nearTable_allPoints <- nearTable_allPoints %>% filter(inNear_tDiff > as.difftime(0, units = "hours"))
# filter out edges with time differences beyond 1 week
nearTable_allPoints <- nearTable_allPoints %>% filter(inNear_tDiff < as.difftime(1, units = "weeks"))

# PROCESSING====
# Generate graph
fireGraph = graph_from_data_frame(nearTable_allPoints[, c("IN_FID", "NEAR_FID")], directed = TRUE)

# Decompose the graph collection while removing isolate vertices
fireGraph_decompose <- fireGraph %>% decompose(min.vertices = 3) # 2 is sufficient to remove isolates, but produce too many small graphs
verticesCounts <- sapply(fireGraph_decompose, vcount)# can also use function(grph) V(grph) %>% length())

# identify the potential ignitial points per graph
ignitialPoints <- fireGraph_decompose %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
ignitialPoints_FID <- ignitialPoints %>% unlist() %>% names() %>% as.numeric() %>% unique()

# identify the spread points
spreadPoints <- fireGraph_decompose %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") != 0])
spreadPoints_FID <- spreadPoints %>% unlist() %>% names() %>% as.numeric() %>% unique()

# test the accuracy of the identified ignitial FIDs by testing if there are any rows with the ignitial FID as the NEAR_ instead of the IN_
# if correct, then there should not be.
check_nearTable <- nearTable_allPoints %>% filter(NEAR_FID %in% ignitialPoints_FID)

# check the exclusivity of spreadPoints and ignitialPoints [both are equal to their respective vector lengths, so no problem here]
# > spreadPoints_FID %>% setdiff(ignitialPoints_FID) %>% length()
# [1] 1287811
# > ?setdiff
# > ignitialPoints_FID %>% setdiff(spreadPoints_FID) %>% length()
# [1] 230314
if(!identical(length(ignitialPoints_FID) + length(spreadPoints_FID), (ignitialPoints_FID %>% c(spreadPoints_FID) %>% unique() %>% length()))) stop("spreadPoints and ignitialPoints have shared members") else{
  # assign labels: "ignitial" and "spread" to the respective points in attribute_allPoints
  attribute_allPoints <- attribute_allPoints %>% rename_at(1, ~"pointID") %>% mutate(pointCategory = case_when(pointID %in% ignitialPoints_FID ~ "ignitial",
                                                                                                               pointID %in% spreadPoints_FID ~ "spead",
                                                                                                               TRUE ~ NA)) %>% mutate(pointCategory = as.factor(pointCategory))
  # check if all coordinates are exclusive
  outPointTable <- attribute_allPoints %>% filter(!is.na(pointCategory))
  if(identical(nrow(outPointTable), nrow(unique(outPointTable[, c("LONGITUDE", "LATITUDE")])))) {
    print("no duplicated point coordinates")
    outPointTable  %>% write.csv(outputPointTable_name)
  } else{
    print("Found some duplicated point coordinates")
    # resolve duplicated point coordinates by only retaining the ignitial points of such cases
    # identify pointID with duplicated coordinates
    duplicatedPointIDs <- outPointTable %>% filter(duplicated(outPointTable[, c("LONGITUDE", "LATITUDE")])) %>% select(LONGITUDE, LATITUDE) %>% left_join(outPointTable)
    # remove the 42 rows that contain duplicated LON LAT from the outPointTable
    outPointTable <- outPointTable %>% setdiff(duplicatedPointIDs)
    # solve the duplicatedPointIDs by slicing the ignitialPoint row, if any
    duplicatedPointIDs <- duplicatedPointIDs %>% group_by(LONGITUDE, LATITUDE) %>% arrange(pointCategory) %>% slice(1) %>% ungroup()
    outPointTable <- outPointTable %>% bind_rows(duplicatedPointIDs)
    if(identical(nrow(outPointTable), nrow(unique(outPointTable[, c("LONGITUDE", "LATITUDE")])))) {
      print("no duplicated point coordinates")
      outPointTable  %>% write.csv(outputPointTable_name)
    } else{
      stop("Some duplicates still exist. Please check")
    }
  }
}