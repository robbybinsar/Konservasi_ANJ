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

#read SMM HCV shapefiles
SMM_hcv <- st_read(dsn = "./spatial_data_analysis/SMM/20200428 - Peta Potensi Area HCV", layer = "20200428_HA_AREA_HCV_POTENSIAL")

# read data points
tarsius_coor <- read.csv("./spatial_data_analysis/SMM/tarsius_coordinates.csv")
tarsius_coor$date <- as.Date(tarsius_coor$date, format = "%d-%b-%y")

# convert data points to sf/sp object
tarsius_coor_sf <- st_as_sf(tarsius_coor, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
tarsius_coor_sf <- tarsius_coor_sf %>% relocate(spesies)
tarsius_coor_sf <- st_transform(tarsius_coor_sf, crs = crs(SMM_hcv, asText = TRUE))

#create buffer 50 meters
tars_buf <- st_buffer(tarsius_coor_sf, dist = 50)

#map data points to interactive tmap
tmap_mode("view")
tm_basemap(server = providers$Esri.WorldImagery)+
  tm_shape(SMM_hcv) + tm_polygons(alpha = 0.6, border.col = "yellow", col = "NAMA_NKT", popup.vars = TRUE) +
  tm_shape(tars_buf) + tm_polygons(alpha = 0.3)+
  tm_shape(tarsius_coor_sf) + tm_dots(col = "darkblue") +
  tm_mouse_coordinates() + tm_scale_bar()

#map data points to static map
 #using ggmap and ggplot
  smm_base <- get_map(location = tarsius_coor[1,1:2], zoom = 17, scale = 1, maptype = "satellite", source = "google")
  ggmap(smm_base)
  