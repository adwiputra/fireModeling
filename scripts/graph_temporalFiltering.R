# Code to filter out graphs that ignited after 2015
# AD
# first written: 10th Jan 2025

# 1. LIBRARIES===
library(data.table)
library(igraph)
library(tidyverse)

# 2. INPUTS=====
load("D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/partB_waterFiltering_2025.RData")

# 3. PROCESSING=====
# a. identify ignition points of each graph
ignitionPoints_graphs <- fireGraph_decompose %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
n_ignitions_graph <- lapply(X = ignitionPoints_graphs, length) %>% unlist()
ignitionPoints_id_unlist <- ignitionPoints_graphs %>% unlist() %>% names() %>% as.numeric()
# Generate data.frame
ignitions_df <- data.frame(ignition_pointID = ignitionPoints_id_unlist, graph_id = rep(1:length(fireGraph_decompose), n_ignitions_graph))
# join with 'attribute_allPoints'
ignitions_df <- attribute_allPoints %>% rename_at(1, ~"ignition_pointID") %>% select(ignition_pointID, ACQ_DATE) %>% right_join(ignitions_df) %>% arrange(graph_id)
# filter based on date
ignitions_df <- ignitions_df %>% filter(ACQ_DATE < as.POSIXct("2016/01/01", format = "%Y/%m/%d", tz = "UTC"))
# ignitialPoints_filtered_FID <- ignitialPoints_filtered %>% unlist() %>% names() %>% as.numeric() %>% unique()
graph_id_toKeep <- ignitions_df %>% filter(ACQ_DATE < as.POSIXct("2016/01/01", format = "%Y/%m/%d", tz = "UTC")) %>% select(graph_id) %>% unique() %>% pull()
# subset input graph
temporalFiltered_graph <- fireGraph_decompose[graph_id_toKeep] # return graph
# b. Identify the ignitial points and the spread points
# identify the potential ignitial points per graph

# function form

graph_temporalFilter <- function(in_graph = fireGraph_decompose, temporal_threshold = as.POSIXct("2016/01/01", format = "%Y/%m/%d", tz = "UTC")){
  ignitionPoints_graphs <- in_graph %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") == 0])
  n_ignitions_graph <- lapply(X = ignitionPoints_graphs, length) %>% unlist()
  ignitionPoints_id_unlist <- ignitionPoints_graphs %>% unlist() %>% names() %>% as.numeric()
  # Generate data.frame
  ignitions_df <- data.frame(ignition_pointID = ignitionPoints_id_unlist, graph_id = rep(1:length(in_graph), n_ignitions_graph))
  # join with 'attribute_allPoints'
  ignitions_df <- attribute_allPoints %>% rename_at(1, ~"ignition_pointID") %>% select(ignition_pointID, ACQ_DATE) %>% right_join(ignitions_df) %>% arrange(graph_id)
  # filter based on date
  graph_id_toKeep <- ignitions_df %>% filter(ACQ_DATE < temporal_threshold) %>% select(graph_id) %>% unique() %>% pull()
  # subset input graph
  return(in_graph[graph_id_toKeep]) # return graph
}


# identify the spread points
spreadPoints_filtered <- fireGraph_decompose_ret %>% sapply(function(grph) V(grph)[degree(grph, v = V(grph), mode = "in") != 0])
spreadPoints_filtered_FID <- spreadPoints_filtered %>% unlist() %>% names() %>% as.numeric() %>% unique()
# test the accuracy of the identified ignitial FIDs by testing if there are any rows with the ignitial FID as the NEAR_ instead of the IN_
# if correct, then there should not be.
check_nearTable <- nearTable_allPoints %>% filter(NEAR_FID %in% ignitialPoints_filtered_FID)
# b. extract the earliest ignition time of each graph
# c. filter graph according to b