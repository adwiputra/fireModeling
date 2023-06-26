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
t_nearTable_allPoints <- acqTime_lookup %>% rename_at(1, ~paste0(names(nearTable_allPoints)[2])) %>% right_join(nearTable_allPoints) %>% rename(IN_TIME = ACT_TIME)
t_nearTable_allPoints <- acqTime_lookup %>% rename_at(1, ~paste0(names(nearTable_allPoints)[3])) %>% right_join(t_nearTable_allPoints) %>% rename(NEAR_TIME = ACT_TIME)

# calculate time difference. Convention: time difference = NEAR_TIME - IN_TIME. Thus, a positive time difference occurs when NEAR_ is later than IN_
t_nearTable_allPoints <- t_nearTable_allPoints %>% mutate(inNear_tDiff = NEAR_TIME - IN_TIME)
# filter out edges with negative time differences
t_nearTable_allPoints <- t_nearTable_allPoints %>% filter(inNear_tDiff > as.difftime(0, units = "hours"))
# filter out edges with time differences beyond 1 week
t_nearTable_allPoints <- t_nearTable_allPoints %>% filter(inNear_tDiff < as.difftime(1, units = "weeks"))

# PROCESSING====
# Generate graph
fireGraph = graph_from_data_frame(t_nearTable_allPoints[, c("IN_FID", "NEAR_FID")], directed = TRUE)
