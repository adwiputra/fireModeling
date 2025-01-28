# Ecoregion class merging to reduce the number of factors in the layer
# 27/01/2025
# AD

# 0. libraries========
library(terra)
library(tidyverse)

# 1. INPUTS=======
ecoregion_input <- "G:/storage/temporary/data/spatial/covariates/ecoregion_SEA/ecoreg_seaOnly_worldMerc_clip.shp" %>% vect()

# 2. PREPROCESSING----
# Manual omission of rastervalue 9 and 30 from the polygon due to the incomplete omission based on spatial intersects
ecoregion_input <- ecoregion_input %>% subset(ECO_NAME != "Carolines tropical moist forests" & ECO_NAME != "Palau tropical moist forests", NSE = TRUE)
original_attributeTable <- values(ecoregion_input)
# height indicators include: "highland", "lowland", "montane", "alpine"
toMatch <- c("highland", "lowland", "montane", "alpine", "sub")
contains_height <- original_attributeTable %>% filter(grepl(paste(toMatch,collapse="|"), original_attributeTable$ECO_NAME)) %>% select(ECO_NAME) %>% unique()
no_height <- original_attributeTable %>% filter(!ECO_NAME %in% contains_height$ECO_NAME) %>% select(ECO_NAME) %>% unique()

ecoName_heightToNoHeight <- contains_height %>% pull() %>% gsub(paste(toMatch,collapse="|"), "", .) %>% gsub("  ", " ", .) %>% gsub("  ", " ", .)
# Correct the " - "
ecoName_heightToNoHeight <- ecoName_heightToNoHeight %>% gsub(" - ", " ", .)
# Address the naming inconsistencies
ecoName_heightToNoHeight <- ecoName_heightToNoHeight %>% gsub("Papuan Central Range", "Central Range Papuan", .)
# 3. PROCESSING=======
# needs to later aggregate (dissolve) the polygon
# merging and sorting to see the potential to dissolve the names following the removal of the height indicators # ADrun
# mergeTrial <- no_height %>% pull() %>% c(ecoName_heightToNoHeight) %>% sort()

# a. Develop the data.frame to be bound with the original dataset
dataFrame_heightToNoHeight <- contains_height %>% bind_cols(ecoName_heightToNoHeight) %>% rename_at(2, ~"newECONAME")
# b. Join the table to the original data
values(ecoregion_input) <- original_attributeTable %>% left_join(dataFrame_heightToNoHeight) %>% mutate(newECONAME = case_when(is.na(newECONAME) ~ ECO_NAME,
                                                                                                                               TRUE ~ newECONAME)) %>%
  mutate(newECONAME = case_when(newECONAME == "Vogelkop rain forests" ~ "Vogelkop-Aru rain forests",
                                newECONAME == "South China-Vietnam tropical evergreen forests" ~ "Borneo rain forests",
                                TRUE ~ newECONAME))

# c. Aggregate the polygon based on the new column, 'newECONAME'
ecoregion_output <- ecoregion_input %>% terra::aggregate(by = "newECONAME", dissolve = TRUE)
values(ecoregion_output) <- values(ecoregion_output) %>% mutate(rasterVal = 1:nrow(ecoregion_output))

# 4. EXPORT===========
writeVector(ecoregion_output, "D:/Documents/research/projects/nus07_fire/analysis/finalized_materials/reduced_ecoregion.shp", overwrite = TRUE)

