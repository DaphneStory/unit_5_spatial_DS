#DPS

#04.08.25


library(marmap)
library(tidyverse)
library(raster)
library(mapdata)

summary(bath_m_raw)
class(bath_m_raw)

bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)

##get rid of land

bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z))
head(bath_m)
tail(bath_m)


##save 

GOM_bath_map = ggplot() +
  geom_raster(data=bath_m, aes(x=x, y=y, fill=depth_m)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group =group), fill="black") +
  coord_fixed(ratio=1.3, xlim=lon_bounds, ylim=lat_bounds, expand=F) +
  scale_fill_gradientn(colors = c("black", "darkblue", "lightblue"),
                       values = scales::rescale(c(-6000, -300, 0)),
                       name = "Depth (m)")

##plot

GOM_bath_map

##add contours

GOM_bath_map_contours = ggplot() +
  geom_raster(data= bath_m, aes(x=x, y=y, fill = depth_m) ) +
  geom_contour(data= bath_m, aes(x=x, y=y, z=depth_m),
               breaks = c(-100), 
               linewidth = 0.25,
               colour = "lightgray") +
  geom_contour(data= bath_m, aes(x=x, y=y, z=depth_m),
               breaks = c(-200), 
               linewidth = 0.5,
               colour = "lightgray") +
  geom_polygon(data = world_map, aes(x=long, y=lat, group =group), fill="black") +
  coord_fixed(ratio=1.3, xlim=lon_bounds, ylim=lat_bounds, expand=F) +
  scale_fill_gradientn(colors = c("black", "darkblue", "lightblue"),
                       values = scales::rescale(c(-6000, -300, 0)),
                       name = "Depth (m)")


##match chl and bathy

bath_m_raster = marmap::as.raster(bath_m_raw)
chl_GOM_raster

names(bath_m_raster) = "bath_m"  

bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster,
                                       crs = crs(chl_GOM_raster)) ##crs only if they didn't match up

##layer one on top of the other

raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)

plot(raster_stack)

##cast raster as data frame

stack_df = data.frame( raster::rasterToPoints(raster_stack))
head(stack_df)

oligo_chl_a = 0.1 #mg/m^3 # from oreiley paper
eutro_chl_a = 1.67 #mg/m^3

library(dplyr)
stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a< oligo_chl_a ~"oligotrophic",
                                   chl_a >= oligo_chl_a & chl_a <eutro_chl_a ~ "mesotrophic",
                                   chl_a >= eutro_chl_a ~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index))

summary(stack_df)
head(stack_df)

table(stack_df$trophic_index)

#summary stats
trophic_status = stack_df %>%
  filter(!is.na(trophic_index)) %>%
  group_by(trophic_index) %>%
  summarize(n=n(),
            mean_depth_m = mean(bath_m)) %>%
  mutate(proportion = n/sum(n))

ggplot() +
  geom_boxplot(aes(x=trophic_index, y=bath_m), data=stack_df)

trophic_map = ggplot() +
  geom_raster(data=stack_df, aes(x=x, y=y, fill = trophic_index)) +
  geom_polygon(data = world_map, aes(x=long, y=lat, group =group), fill="black") +
  coord_fixed(ratio=1.3, xlim=lon_bounds, ylim=lat_bounds, expand=F)
  


################
# Spatial Vector Points

library(sf)

USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat/",
                       layer="North_Atlantic_Right_Whale_Critical_Habitat")
head(USA_crit_hab)

##st_ stands for spatial type

st_crs(USA_crit_hab)$epsg

##translate to WGS84 = 4326

USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326)

##load in data sets

carcass= read.csv('data/RW_carcasses_2017.csv')
head(carcass)
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)


##turn lat and long into polygons

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union=F) %>% # maintains original order of the points
  st_cast(to="POLYGON")
  
head(CAN_crit_hab_sf)

#bind can and usa together

USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf$country = "USA"
USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(country, habitat, geometry)
head(USA_crit_hab_sf)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)
head(crit_hab)
