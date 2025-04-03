#DPS

#04.03.25


library(tidyverse)
library(raster)
library(mapdata)
library(marmap)


chl_raster = raster('data/AQUA_MODIS.20020701_20230731.L3m.MC.CHL.chlor_a.9km.nc')

names(chl_raster) = "chl_a"

chl_pts = raster::rasterToPoints(chl_raster, spatial=TRUE)
class(chl_pts)

chl_df = data.frame(chl_pts)
summary(chl_df)
head(chl_df)


hist(chl_df$chl_a)
hist(log10(chl_df$chl_a))


##create a color scale

cols = rainbow(7, rev= T)[-1]


global_chl_map = ggplot() +
  geom_raster(data = chl_df, aes(x=x, y=y, fill=log10(chl_a))) +
  scale_fill_gradientn(colors = cols,
                       limits=c(-1.5, 0.75),
                       oob = scales::squish,    ##oob = out of bounds ##anything that is smaller than -1.5 is blue and larger than 0.75 is red
                       name="log10(chl_a)") +
  ggtitle("Chl a July Climatology") +
  theme_classic()

ggsave(global_chl_map, filename = "figures/chl_a_climatology.pdf",
       device="pdf",
       height = 5,
       width = 9)

##GOM
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)


chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))

chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial=T))
head(chl_GOM_df)


##Formally map in the land

world_map = map_data("worldHires")
head(world_map)


GOM_chl_map = ggplot() +
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a))) +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_fixed(1.3, xlim = lon_bounds, ylim=lat_bounds,, expand=F) +
  scale_fill_gradientn(colors=cols, limits=c(-1,1.75)) +
  theme_bw()

ggsave(GOM_chl_map, filename = "figures/Gulf_of_Maine_climatology.pdf",
       device="pdf",
       height = 5,
       width = 9)


##Bring in bathymetry data

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1],
                                   lon2 = lon_bounds[2],
                                   lat1 = lat_bounds[1],
                                   lat2 = lat_bounds[2],
                                   resolution = 4) ## 4 arcminutes

bath_m_raw
