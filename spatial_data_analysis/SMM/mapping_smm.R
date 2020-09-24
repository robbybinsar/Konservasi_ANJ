library(ggmap)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(RColorBrewer)
library(png)
library(grid)
library(gridExtra)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(raster)
library(rgdal)
library(dplyr)

# read data points
tarsius_coor <- read.csv("./spatial_data_analysis/SMM/tarsius_coordinates.csv")
tarsius_coor$date <- as.Date(tarsius_coor$date, format = "%d-%b-%y")

#read buffer 50m
tar_buff <- readOGR(dsn = "./spatial_data_analysis/SMM/tarsius_smm/buffer", layer = "tar_buffer")
tar_buff_sf <- st_read(dsn = "./spatial_data_analysis/SMM/tarsius_smm/buffer", layer = "tar_buffer")

tar_buff_sf <- tar_buff_sf %>% relocate(spesies)

# convert data points to sf/sp object
tarsius_coor_sf <- st_as_sf(tarsius_coor, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
st_crs(tarsius_coor_sf)
#sp class
tarsius_coor_sp <- SpatialPointsDataFrame(tarsius_coor[,1:2], tarsius_coor)
crs(tarsius_coor_sp) <- CRS('+init=EPSG:4326')

tarsius_coor_sf <- tarsius_coor_sf %>% relocate(spesies)

#map data points to interactive tmap
tmap_mode("view")
tm_basemap(server = providers$Esri.WorldImagery)+
  tm_shape(tar_buff_sf) + tm_polygons(alpha = 0.3)+
  tm_shape(tar_buff_sf) + tm_bubbles(col = "darkblue", size = 1, alpha =1) +
  tm_mouse_coordinates() + tm_scale_bar()

#tm_shape(tarsius_coor_sf) + tm_bubbles(size = 24.9*50, alpha = 0.5, style = "fixed") + #24.9x/metric
#map data points to static map
 #using ggmap and ggplot
  smm_base <- get_map(location = tarsius_coor[1,1:2], zoom = 17, scale = 1, maptype = "satellite", source = "google")
  ggmap(smm_base)
  