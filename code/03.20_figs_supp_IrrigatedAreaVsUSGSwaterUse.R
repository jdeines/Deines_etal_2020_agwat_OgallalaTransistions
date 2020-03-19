## IrrigatedAreaVsUSGSwaterUse.R
#' This script is meant to be a 'sanity check' on the assumption that
#' all areas with significant depletion are irrigated by groundwater.
#' 
#' It compares the percent of a county expected to be depleted by 2100
#' to the percent of total irrigation volume from groundwater. Points
#' plotting below the 1:1 line are reasonable.
#' 
#' code by Sam Zipper

require(ggplot2)
require(dplyr)
library(tidyr)
require(sf)
require(reshape2)
require(gridExtra)
library(here)

rootDir <- here::here()
subfolder <- 'data/tabular/supplementFiles/'

path_project <- paste0(rootDir, '/', subfolder)

## water use data
df_use_all <- read.csv(paste0(path_project, '/',"USGS_01_DownloadWaterUseData.csv"), 
                       stringsAsFactors=F) %>% 
  subset(year==2015)
df_use_all[df_use_all=="-"] <- NaN

# relevant categories:
# [123] "Irrigation..Crop.self.supplied.groundwater.withdrawals.for.crops..fresh..in.Mgal.d"                 
# [124] "Irrigation..Crop.self.supplied.surface.water.withdrawals.for.crops..fresh..in.Mgal.d"               
# [125] "Irrigation..Crop.total.self.supplied.withdrawals.for.crops..fresh..in.Mgal.d"                       
# [126] "Irrigation..Crop.consumptive.use.for.crops..fresh..in.Mgal.d"                                       
# [127] "Irrigation..Crop.conveyance.loss.for.crops..in.Mgal.d"                                              
# [128] "Irrigation..Crop.sprinkler.irrigation.for.crops..in.thousand.acres"                                 
# [129] "Irrigation..Crop.microirrigation.for.crops..in.thousand.acres"                                      
# [130] "Irrigation..Crop.surface.irrigation.for.crops..in.thousand.acres"                                   
# [131] "Irrigation..Crop.total.irrigation.for.crops..in.thousand.acres"                                     
# [132] "Irrigation..Crop.reclaimed.wastewater.for.crops..in.Mgal.d"    

unique(df_use_all[,123])
unique(df_use_all[,124])
unique(df_use_all[,125])
unique(df_use_all[,126])

# extract relevant categories
df_use <- data.frame(masterid = df_use_all$masterid,
                     year = df_use_all$year,
                     state_name = df_use_all$state_name,
                     irrigation_gw_Mgal.d = as.numeric(df_use_all$Irrigation..Total.self.supplied.groundwater.withdrawals..fresh..in.Mgal.d),
                     irrigation_sw_Mgal.d = as.numeric(df_use_all$Irrigation..Total.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d)) %>% 
  transform(irrigation_gw_prc = irrigation_gw_Mgal.d/(irrigation_gw_Mgal.d+irrigation_sw_Mgal.d))

qplot(df_use$irrigation_gw_prc)

df_use$masterid <- sprintf("%05d", df_use$masterid)


## irrigated area data
df_area <- read.csv(paste0(path_project, "county_irrStats_current_projectedLoss_vRSESUB_20190305.csv"), stringsAsFactors=F)
df_area$area_loss_prc <- df_area$irrLoss2100_m2/df_area$currentIrr20152017_m2
df_area$masterid <- sprintf("%05d", df_area$masterid)

## combine and map
sf_counties <- 
  paste0(rootDir, "/data/gis/boundaries/tigris_2012_counties_100_inMinBoundBuff.geojson") %>% 
  sf::st_read(stringsAsFactors=F) %>% 
  dplyr::left_join(df_area, by="masterid") %>% 
  dplyr::left_join(df_use, by="masterid") %>% 
  mutate(gwPrc_lt_depletionPrc = irrigation_gw_prc <= area_loss_prc)


sf_counties_long <-
  sf_counties %>% 
  dplyr::select(geometry, area_loss_prc, irrigation_gw_prc, state_name) %>% 
  tidyr::gather(., key = variable, value = value, area_loss_prc:irrigation_gw_prc)

p.maps <- 
  ggplot(sf_counties_long, aes(fill=value)) +
  geom_sf() +
  facet_wrap(~variable, 
             labeller=as_labeller(c("area_loss_prc"="% Irrigated Area lost by 2100", 
                                    "irrigation_gw_prc"="% Irrigation from Groundwater"))) +
  scale_x_continuous(name="Longitude") +
  scale_y_continuous(name="Latitude") +
  scale_fill_gradient(name=NULL, limits=c(0,1), labels = scales::percent,
                      low="gray65", high="blue", na.value="white") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.background=element_blank(),
        panel.grid.major=element_line(colour="transparent"))
p.maps

p.scatter <- 
  ggplot(sf_counties, aes(x=irrigation_gw_prc, y=area_loss_prc, color=state_name)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point() +
  scale_x_continuous(name="% Irrigation from Groundwater", limits=c(0,1), labels = scales::percent) +
  scale_y_continuous(name="% Irrigated Area lost by 2100", limits=c(0,1), labels = scales::percent) +
  scale_color_discrete(name="State") +
  theme_bw() +
  theme(legend.position=c(0,1),
        legend.justification=c(0,1),
        legend.background=element_blank(),
        panel.grid=element_blank())
p.scatter

ggsave(paste0(rootDir, "/figure/03.20_figs_supp_groundwater/IrrigatedAreaVsUSGSwaterUse.png"), 
              grid.arrange(p.maps,
                           p.scatter, 
                           nrow=1,
                           widths=c(1.5,1)),
              width=240, height=95, units="mm")

       