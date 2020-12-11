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

#read HGU SMM
SMM_HGU <- st_read(dsn = "./spatial_data_analysis/SMM/shp_HGU_SMM", layer = "HGU_SMM")

#read SMM HCV shapefiles
SMM_hcv <- st_read(dsn = "./spatial_data_analysis/SMM/20200428 - Peta Potensi Area HCV", layer = "20200428_HA_AREA_HCV_POTENSIAL")

# read data points
priority_species <- read.csv("./spatial_data_analysis/SMM/priority_species.csv")
priority_species$date <- as.Date(priority_species$date, format = "%d-%b-%y")

# convert data points to sf object
priority_sf <- st_as_sf(priority_species, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
priority_sf <- priority_sf %>% relocate(spesies)
priority_sf <- st_transform(priority_sf, crs = crs(SMM_hcv, asText = TRUE))

#create buffer 30 meters
priority_buffer <- st_buffer(priority_sf, dist = 50, endCapStyle = "ROUND")

#map data points to interactive tmap
tmap_mode("view")
tm_basemap(server = providers$Esri.WorldImagery)+
  tm_shape(SMM_HGU) + tm_polygons(alpha = 0.1, border.col = "green") +
  tm_shape(SMM_hcv) + tm_polygons(alpha = 0.1, border.col = "yellow", col = "NAMA_NKT", 
                                  popup.vars = TRUE, legend.show = FALSE) +
  tm_shape(priority_buffer) + tm_polygons(alpha = 0.3)+
  tm_shape(priority_sf) + tm_dots(popup.vars = TRUE) +
  tm_mouse_coordinates() + tm_scale_bar()
  