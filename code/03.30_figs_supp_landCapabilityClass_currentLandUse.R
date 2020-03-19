## landCapabilityClass_currentLandUse.R
#' This is intended to analyze the current land use as a function of soil capability class
#' to determine whether soil capability class is a reasonable metric for estimating future transitions.

#' 
#' code by Sam Zipper

library(tidyverse)
library(here)

rootDir <- here::here()
subfolder <- 'data/tabular/supplementFiles/landCapabilityClass_currentLandUse'
helperDataFolder <- 'data/helper'

## read in data
# legend containing crop type for CDL
CDL_key <- readr::read_csv(paste0(rootDir, '/', helperDataFolder, '/', "CDL_key_2014.csv"))

files.all <- list.files(paste0(rootDir, '/', subfolder), full.names = TRUE)
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
  reshape2::melt(id = c("Year", "masterid", "irrStatus", "nirrcapcl"), 
                 value.name = "area_m2", variable.name = "value") %>% 
  subset(area_m2 > 0)

df.long$value <- as.numeric(as.character(df.long$value))
df.long <- dplyr::left_join(df.long, CDL_key, by = c("value"="VALUE"))

## The question to investigate is: on what soil classification is dryland agriculture viable and where is pasture necessary?
# relevant CDL CLASS_NAME fields: c("Grass/Pasture", "Durum Wheat", "Spring Wheat", "Winter Wheat")
unique(df.long$CLASS_NAME)

# vectors for options considered dryland crops (Bill Golden email on 5/24: wheat, corn, sorghum, soybeans)
dryland.class <- c("Corn", "Pop or Orn Corn", 
                   "Durum Wheat", "Dbl Crop WinWht/Corn", 
                   "Dbl Crop Oats/Corn", "Spring Wheat", "Dbl Crop WinWht/Sorghum", 
                   "Dbl Crop Barley/Corn", "Winter Wheat", 
                   "Dbl Crop Soybeans/Oats", "Dbl Crop Corn/Soybeans", "Dbl Crop WinWht/Soybeans", 
                   "Sorghum", "Soybeans")

pasture.class <- c("Grass/Pasture")  # "Shrubland"

# add a LandUse category that broadly groups dryland ag vs. grass/pasture
df.long <- 
  df.long %>% 
  subset(CLASS_NAME %in% c(dryland.class, pasture.class)) %>% 
  transform(LandUse = ifelse(CLASS_NAME %in% dryland.class, "Dryland Ag", "Grass/Pasture"))

# description of SSURGO nonirrigated soil land capability classification (1 = best, 8 = worst)
# Class 1 soils have slight limitations that restrict their use.
# Class 2 soils have moderate limitations that reduce the choice of plants or require moderate conservation practices.
# Class 3 soils have severe limitations that reduce the choice of plants or require special conservation practices, or both.
# Class 4 soils have very severe limitations that restrict the choice of plants or require very careful management, or both.
# Class 5 soils have little or no hazard of erosion but have other limitations, impractical to remove, that limit their use mainly to pasture, range, forestland, or wildlife food and cover.
# Class 6 soils have severe limitations that make them generally unsuited to cultivation and that limit their use mainly to pasture, range, forestland, or wildlife food and cover.
# Class 7 soils have very severe limitations that make them unsuited to cultivation and that restrict their use mainly to grazing, forestland, or wildlife.
# Class 8 soils and miscellaneous areas have limitations that preclude their use for commercial plant production and limit their use to recreation, wildlife, or water supply or for esthetic purposes.

# subset to relevant fields and summarize total area by soil class
df.LandUse.summary <-
  df.long %>% 
  subset(irrStatus == 0) %>% 
  dplyr::group_by(nirrcapcl, LandUse) %>% 
  dplyr::summarize(AreaInClass_m2 = sum(area_m2))

ggplot(df.LandUse.summary, aes(x=LandUse, y=AreaInClass_m2/(1000*1000), fill=factor(nirrcapcl))) +
  geom_col() +
  scale_x_discrete(name="Current Land Use") +
  scale_y_continuous(name="Current Non-Irrigated Area [km\u00B2]") +
  scale_fill_discrete(name="Soil\nCapability\nClass") +
  theme_bw() +
  theme(legend.title=element_text(hjust=0.5),
        legend.background=element_blank(),
        panel.grid=element_blank()) +
  ggsave(paste0(rootDir, '/figure/03.30_figs_supp_landCapabilityClasses/landCapabilityClass_currentLandUse.png'),
         width=120, height=95, units="mm")

dryland.area <- sum(df.LandUse.summary$AreaInClass_m2[df.LandUse.summary$LandUse=="Dryland Ag"])
pasture.area <- sum(df.LandUse.summary$AreaInClass_m2[df.LandUse.summary$LandUse=="Grass/Pasture"])

# % of dryland ag in each class
sum(df.LandUse.summary$AreaInClass_m2[df.LandUse.summary$LandUse=="Dryland Ag" & df.LandUse.summary$nirrcapcl < 5])/dryland.area
sum(df.LandUse.summary$AreaInClass_m2[df.LandUse.summary$LandUse=="Grass/Pasture" & df.LandUse.summary$nirrcapcl < 5])/pasture.area
