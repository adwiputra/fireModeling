# Stats to check the difference between 0 % and 100 % ignition
# first written 17 February 2025
# AD

# 0. LIBRARIES====
library(lme4)
library(lmerTest)
library(ggplot2)
library(data.table)
library(tidyverse)

# 1. INPUTS=====
inputDir <- "D:/Documents/research/projects/nus07_fire/analysis/output/"
allIgnition_varImportance <- fread(paste0(inputDir, "summarizedVariableImportance_monteCarlo_ignitionPercent_100.csv"))
noIgnition_varImportance <- fread(paste0(inputDir, "summarizedVariableImportance_monteCarlo_ignitionPercent_0.csv"))

# PREDICTORS
predictors <- c("vpd", "DEM", "H", "gridcode", "ecoregion", "human", "travel", "water")
# 2. PREPROCESSING=======
# A. BIND DATA FRAMES
combined_varImportance <- allIgnition_varImportance %>% bind_rows(noIgnition_varImportance) %>% mutate(expl.var = case_when(grepl("vpd", expl.var) ~ "VPD",
                                                                                                                            grepl("DEM", expl.var) ~ "Elevation",
                                                                                                                            grepl("H", expl.var) ~ "Land cover diversity",
                                                                                                                            grepl("gridcode", expl.var) ~ "Dominant land cover",
                                                                                                                            grepl("ecoregion", expl.var) ~ "Ecoregion",
                                                                                                                            grepl("human", expl.var) ~ "Human footprint",
                                                                                                                            grepl("travel", expl.var) ~ "Travel time to the nearest city",
                                                                                                                            grepl("water", expl.var) ~ "Distance to water bodies",
                                                                                                                            TRUE ~ expl.var
                                                                                                                            ))

# B. SEPARATE FOR EACH VARIABLE ANALYZED
vpd_df <- combined_varImportance %>% filter(grepl("VPD", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "VPD")
dem_df <- combined_varImportance %>% filter(grepl("Elevation", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Elevation")
h_df <- combined_varImportance %>% filter(grepl(predictors[3], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Land cover diversity")
domLandCover_df <- combined_varImportance %>% filter(grepl("Dominant land cover", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Dominant land cover")
ecoregion_df <- combined_varImportance %>% filter(grepl("Ecoregion", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Ecoregion")
human_df <- combined_varImportance %>% filter(grepl("Human footprint", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Human footprint")
travel_df <- combined_varImportance %>% filter(grepl("Travel time to the nearest city", expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Travel time to city")
water_df <- combined_varImportance %>% filter(grepl(predictors[8], expl.var)) %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>% mutate(expl.var = "Distance to water bodies")
# 3. PROCESSING======
# A. LMER
# A1. VPD
lmer_vpd <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = vpd_df)#,      
             # control=lmerControl(check.nobs.vs.nlev = "ignore",
             #                     check.nobs.vs.nRE  = "ignore"))
summary(lmer_vpd)
anova(lmer_vpd)

# A2. DEM
lmer_dem <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = dem_df)
summary(lmer_dem)
anova(lmer_dem)

# A3. landCover diversity (H)
lmer_h <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = h_df)
summary(lmer_h)
anova(lmer_h)

# A4. dominant land cover
lmer_domLandCover <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = domLandCover_df)
summary(lmer_domLandCover)
anova(lmer_domLandCover)

# A5. ecoregion
lmer_ecoregion <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = ecoregion_df)
summary(lmer_ecoregion)
anova(lmer_ecoregion)

# A6. human footprint
lmer_human <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = human_df)
summary(lmer_human)
anova(lmer_human)

# A7. travel time
lmer_travel <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = travel_df)
summary(lmer_travel)
anova(lmer_travel)

# A8. distance to water
lmer_water <- lmer(var.imp ~ ignitionPercent + (1|absenceSet), data = water_df)
summary(lmer_water)
anova(lmer_water)

# Positive fixed effect means that the 100% ignition run has higher varImp for the predictor in focus and vice versa.

# 4. PLOTTING======
# generate 8 box plots, each depicting the variable importance values with 0 or 100 ignition
par(mfrow = c(2, 4))
# 1. VPD
ggplot(vpd_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("VPD") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 2. Elevation
ggplot(dem_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Elevation") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 3. Land cover diversity
ggplot(h_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Land cover diversity") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 4. Dominant land cover
ggplot(domLandCover_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Dominant land cover") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 5. Ecoregion
ggplot(ecoregion_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Ecoregion") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 6. Human footprint
ggplot(human_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Human footprint") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 7. Travel time to nearest city
ggplot(travel_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Travel time to the nearest city") + theme_classic() + theme(plot.title = element_text(hjust=0.5))
# 8. Distance to water# 2. Elevation
ggplot(water_df, aes(x= ignitionPercent, y=var.imp)) +
  geom_boxplot(fill='#A4A4A4', color="black")+ ylab(element_blank()) + xlab(element_blank()) +
  ggtitle("Distance to water") + theme_classic() + theme(plot.title = element_text(hjust=0.5))



# An alternative using facet.wrap
combined_varImportance %>% mutate(ignitionPercent = as.factor(ignitionPercent)) %>%
  # pivot_longer(everything()) %>%
  ggplot(aes(x = ignitionPercent, y = var.imp, group = )) + 
  geom_boxplot(fill='#A4A4A4', color="black") +
  facet_grid(~ factor(expl.var, levels = c("Ecoregion", "VPD", "Elevation", "Distance to water bodies", "Dominant land cover", "Human footprint", "Land cover diversity", "Travel time to the nearest city"))) + facet_wrap( ~ factor(expl.var, levels = c("Ecoregion", "VPD", "Elevation", "Distance to water bodies", "Dominant land cover", "Human footprint", "Land cover diversity", "Travel time to the nearest city")), nrow = 2) + theme_gray() + ylab("Variable importance") + xlab("Origin points input content (%)")
