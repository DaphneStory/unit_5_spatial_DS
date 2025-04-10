#DPS

#4.10.25


#use eval = F to show bits of code without running them in the final project

library(marmap)
library(tidyverse)
library(raster)
library(mapdata)


#plot end of last class
lat_bounds = c(39, 53) # S to N?
lon_bounds = c(-72, -54)# W to E


world_map = map_data("worldHires", ylim=lat_bounds, xlim=lon_bounds)

crit_map = ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill = "black") +
  geom_sf(data=crit_hab, aes(fill=country), alpha = 0.5) +
  geom_point(data = carcass, aes(x=Longitude, y = Latitude, color = Carcass.condition.at.first.observation)) +
  coord_sf(xlim= lon_bounds, ylim=lat_bounds)

ggsave(plot=crit_map, filename = "figures/crit_map.pdf")



############

# AIS


library(tidyverse)
library(sf)
library(mapdata)
library(lubridate)


ais_day = read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)
dim(ais_day)

lat_bounds = c(25, 34)
lon_bounds = c(-82, -76)
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

ais_map_pts = ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group = group), fill = "darkgrey") +
  geom_sf(data=USA_crit_hab, alpha=0.5, fill = "pink") +
  geom_point(data = ais_day, aes(x=LON, y=LAT)) +
  coord_sf(xlim=lon_bounds, ylim=lat_bounds)+
  theme_classic()

ggsave(plot=ais_map_pts, filename = "figures/ais_map_pts.pdf")

##which points fall inside of the polygon == which AIS points occur in the SEUS crit hab

Sys.time()
ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs = 4269) %>% ##AIS uses NAD83 CRS
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))
Sys.time()


law_breakers = ships_RW_intersect %>%
  filter(Length>20, 
         SOG > 10) ## ship length greater than 65 feet and speeed over ground greater than 10 knots
head(law_breakers)
dim(law_breakers)


length(unique(law_breakers$CallSign))

illegal_paths = law_breakers %>%
  mutate(data_time = lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(data_time) %>%
  group_by(CallSign) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") %>%
  st_make_valid()  ##gets rid of single point lines


##plot

law_breaking_map = ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgreen") +
  geom_sf(data=USA_crit_hab_sf, alpha = 0.5, fill="darkslategrey") +
  geom_sf(data=illegal_paths, aes(color=CallSign, fill=CallSign)  ) +
  coord_sf(xlim=lon_bounds, ylim=lat_bounds)


##count number of kilometers of illegal path
illegal_paths_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry)) 

tot_illegal_path = sum(illegal_paths_lengths$track_length_m)
tot_illegal_path
