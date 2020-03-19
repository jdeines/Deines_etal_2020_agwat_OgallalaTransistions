## landCapabilityClass_currentLandUse_withSubclasses.R
#' This is intended to analyze the current land use as a function of soil capability class and subclass
#' to determine whether soil capability class is a reasonable metric for estimating future transitions.
#' 
#' code by Sam Zipper

library(tidyverse)
library(here)

rootDir <- here::here()
subfolder <- 'data/tabular/supplementFiles'
subdir <- 'soil_lcc_20190623_subclassFixed_cdlCrp'

helperDataFolder <- 'data/helper'

## read in data
# GW depletion data to subset counties investigated
df.irr <- readr::read_csv(paste0(rootDir, '/', subfolder, '/county_irrStats_current_projectedLoss_vRSESUB_20190305.csv'))
irr.counties <- df.irr$masterid[is.finite(df.irr$irrLoss2100_m2)]

# legend containing crop type for CDL
CDL_key <- readr::read_csv(paste0(rootDir, '/', helperDataFolder, '/', "CDL_key_2014.csv"))

# colors for plots
col.corn <- rgb(CDL_key$RED[CDL_key$CLASS_NAME %in% "Corn"], CDL_key$GREEN[CDL_key$CLASS_NAME %in% "Corn"], CDL_key$BLUE[CDL_key$CLASS_NAME %in% "Corn"])
col.grass <- rgb(CDL_key$RED[CDL_key$CLASS_NAME %in% "Forest"], CDL_key$GREEN[CDL_key$CLASS_NAME %in% "Forest"], CDL_key$BLUE[CDL_key$CLASS_NAME %in% "Forest"])

files.all <- list.files(paste0(rootDir, '/', subfolder, '/',subdir), full.names = TRUE)
for (f in 1:length(files.all)){
  
  # read in file
  df.f <- 
    files.all[f] %>% 
    readr::read_csv() %>% 
    # get rid of useless columns and dummy 0s
    dplyr::select(-c(`system:index`, .geo)) %>% 
    subset(masterid != 0) %>% 
    replace(is.na(.), 0)
  
  # combine
  if (f == 1){
    df <- df.f
  } else {
    df <- rbind(df, df.f)
  }
  
  # status update
  print(paste0("Read in file ", f, " complete"))
  
}

# melt into long form
df.long <- 
  df %>% 
  reshape2::melt(id = c("Year", "masterid", "irrStatus", "nirrcapcl", "nirrcapscl"), 
                 value.name = "area_m2", variable.name = "value") %>% 
  subset(area_m2 > 0 & masterid %in% irr.counties & irrStatus == 0)

df.long$value <- as.numeric(as.character(df.long$value))
df.long <- dplyr::left_join(df.long, CDL_key, by = c("value"="VALUE"))

## The question to investigate is: on what soil classification is DrylandAgriculture viable and where is pasture necessary?
# relevant CDL CLASS_NAME fields: c("GrassPasture", "Durum Wheat", "Spring Wheat", "Winter Wheat")
unique(df.long$CLASS_NAME)

# vectors for options considered dryland crops (Bill Golden email on 5/24: wheat, corn, sorghum, soybeans)
dryland.class <- c("Corn", "Pop or Orn Corn", 
                   "Durum Wheat", "Dbl Crop WinWht/Corn", 
                   "Dbl Crop Oats/Corn", "Spring Wheat", "Dbl Crop WinWht/Sorghum", 
                   "Dbl Crop Barley/Corn", "Winter Wheat", 
                   "Dbl Crop Soybeans/Oats", "Dbl Crop Corn/Soybeans", "Dbl Crop WinWht/Soybeans", 
                   "Sorghum", "Soybeans", "Fallow/Idle Cropland")

pasture.class <- c("Grass/Pasture")  # "Shrubland"

# what % of area are in different categories?
total.area <- sum(df.long$area_m2)

sum(df.long$area_m2[df.long$CLASS_NAME %in% dryland.class])/total.area
sum(df.long$area_m2[df.long$CLASS_NAME %in% pasture.class])/total.area
sum(df.long$area_m2[df.long$CLASS_NAME %in% "Shrubland"])/total.area

df.uncategorized <- 
  df.long %>% 
  subset(!(CLASS_NAME %in% c(dryland.class, pasture.class))) %>% 
  group_by(CLASS_NAME) %>% 
  summarize(area.sum = sum(area_m2),
            area.prc = sum(area_m2)/total.area) %>% 
  arrange(desc(area.prc))

# add a LandUse category that broadly groups DrylandAg vs. GrassPasture
df.long <- 
  df.long %>% 
  subset(CLASS_NAME %in% c(dryland.class, pasture.class)) %>% 
  transform(LandUse = ifelse(CLASS_NAME %in% dryland.class, "DrylandAg", "GrassPasture"))

## description of SSURGO nonirrigated soil land capability classification (1 = best, 8 = worst)
# Class 1 soils have slight limitations that restrict their use.
# Class 2 soils have moderate limitations that reduce the choice of plants or require moderate conservation practices.
# Class 3 soils have severe limitations that reduce the choice of plants or require special conservation practices, or both.
# Class 4 soils have very severe limitations that restrict the choice of plants or require very careful management, or both.
# Class 5 soils have little or no hazard of erosion but have other limitations, impractical to remove, that limit their use mainly to pasture, range, forestland, or wildlife food and cover.
# Class 6 soils have severe limitations that make them generally unsuited to cultivation and that limit their use mainly to pasture, range, forestland, or wildlife food and cover.
# Class 7 soils have very severe limitations that make them unsuited to cultivation and that restrict their use mainly to grazing, forestland, or wildlife.
# Class 8 soils and miscellaneous areas have limitations that preclude their use for commercial plant production and limit their use to recreation, wildlife, or water supply or for esthetic purposes.

## description of SSURGO nonirrigated soil subclass
#  e = erosion limitation
#  c = too cold/dry
#  w = soil wetness
#  s = stony

# set class and subclass as factors
df.long$nirrcapcl <- factor(df.long$nirrcapcl)
df.long$nirrcapscl <- factor(df.long$nirrcapscl, levels=c("0"="0", "C"="1", "E"="2", "S"="3", "W"="4"))
df.long$LandUse <- factor(df.long$LandUse)

# subset to relevant fields and summarize total area by soil class
df.LandUse.summary <-
  df.long %>% 
  dplyr::group_by(nirrcapcl, LandUse) %>% 
  dplyr::summarize(AreaInClass_m2 = sum(area_m2))

# investigate all class/subclass combos
df.wide <- 
  df.long %>% 
  dplyr::group_by(nirrcapcl, nirrcapscl, LandUse) %>% 
  dplyr::summarize(AreaInClass_m2 = sum(area_m2)) %>% 
  reshape2::dcast(nirrcapcl + nirrcapscl ~ LandUse) %>% 
  transform(DrylandAg_prc = DrylandAg/(DrylandAg+GrassPasture))

ggplot(df.wide, 
       aes(y=factor(nirrcapcl), x=factor(nirrcapscl), fill=DrylandAg_prc)) +
  geom_raster() +
  scale_y_discrete(name="Soil Capability Class (lower = better)") +
  scale_x_discrete(name="Soil Capability Subclass",
                   labels=c("0", "C", "E", "S", "W")) +
  scale_fill_gradient2(name="% Dryland Ag", limits=c(0,1), breaks=seq(0,1,0.2),
                       midpoint=0.5, low=col.grass, high=col.corn, mid = "grey85", label=scales::percent) +
  theme_bw() +
  theme(legend.title=element_text(hjust=0.5),
        legend.background=element_blank(),
        panel.grid=element_blank()) +
  ggsave(paste0(rootDir, "/figure/03.40_figs_supp_soilSubClasses/landCapabilityClass_currentLandUse_withSubclasses.png"),
         width=120, height=95, units="mm")
