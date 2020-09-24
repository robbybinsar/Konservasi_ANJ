library(ggmap)
library(dplyr)

 # using basemap layer from GGMAP
kalcoor <- c(lon = 110.200, lat = -1.42)
kal_base <- get_map(location = kalcoor, zoom = 12, scale = 1, maptype = "terrain", source = "stamen")
ggmap(kal_base)

#read shp kal
  #method 1 using st_read : get sf class object good to use for ggplot
  library(sf)
    #HGU
    kal_shp1 <- st_read("./spatial_data_analysis/KAL/KAL SHP/HGU KAL.shp")
    st_crs(kal_shp1)
    summary(kal_shp1)
    plot(kal_shp1)
    #HCV
    kal_hcv <- st_read("./spatial_data_analysis/KAL/HCV/KAL_HCV_AREA.shp")
  # method 2 using readOGR : get sp object for tmap
  library(rgdal)
    # HGU
    kal_shp2 <- readOGR(dsn = "./spatial_data_analysis/KAL/KAL SHP", layer = "HGU KAL")  
    plot(kal_shp2)
    #HCV
    kal_hcv1 <- readOGR(dsn = "./spatial_data_analysis/KAL/HCV", layer = "KAL_HCV_AREA")
    
# read data points for orang utan encounter
orgutan <- read.csv("./spatial_data_analysis/KAL/ex_data_orangutan.csv")
orgutan <- orgutan %>% relocate(spesies) %>% select(-X)
orgutan_sf <- st_as_sf(orgutan, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
orgutan_sp <- SpatialPointsDataFrame(coords = orgutan[,2:3], data = orgutan, proj4string = CRS('+init=EPSG:4326'))
# read data points for pakan orang utan
  
# plot using ggmap + ggplot
  library(ggplot2)
  library(ggspatial)
  library(ggmap)
  library(RColorBrewer)
  library(png)
  library(grid)
  library(gridExtra)
  anjpng <- readPNG("./spatial_data_analysis/anj.png")
    
  ggmap(kal_base, base_layer = ggplot(data = kal_shp1)) +
    geom_sf(color = "black",inherit.aes = F, alpha = 0.2, lwd = 0.3, show.legend = F, fill = "cornsilk") +
    xlab("Longitude") + ylab("Latitude") + 
    ggtitle("PT Kayung Agro Lestari", subtitle = "High Conservation Value Area") + annotation_scale(location = "br") + 
    annotation_north_arrow(height = unit(1, "cm"), width = unit(0.8, "cm"),
                           pad_y = unit(0.8, "cm"), pad_x = unit(2.5, "cm"), 
                           style = north_arrow_fancy_orienteering, location = "br") +
    geom_sf(data = kal_hcv, alpha = 0.5, aes(fill = HCV, color = ENG_NAME), inherit.aes = T) +
    scale_color_manual(values = c(brewer.pal(n=9, "Set1"), "#4D004B", "#810F7C", "#BFD3E6", "#9EBCDA", "#8C96C6", "#88419D"))+
    scale_fill_brewer(palette = "Set1")+
    theme(panel.grid = element_line(colour = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.ontop = T, panel.background = element_blank(), legend.spacing = unit(0.1, "cm"), 
          legend.background = element_rect(fill = "grey")) +
    labs(caption = "21-09-2020") +
    annotation_custom(rasterGrob(anjpng, width = 0.5, height = 0.5), xmin = 110.27, xmax = 110.32, ymin = -1.33, ymax = -1.31)+
    coord_sf(crs = st_crs(4326))

# map using tmap
  library(tmap)
  library(tmaptools)
  library(leaflet)
  
  #interactive map
  tmap_mode("view")
  tm_basemap(server = providers$Esri.WorldImagery)+
  tm_shape(kal_shp1) + tm_polygons(border.col = "yellow", alpha = 0, group = "KAL BORDER")+
    tm_shape(kal_hcv1) + tm_fill(col = "ENG_NAME", alpha = 0.5, title = "Lokasi", group = "KAL HCV AREA",
                                 popup.vars = c("Lokasi: " = "ENG_NAME", "HCV Categories: " = "HCV",
                                                "Hectares:" = "HECTARES"), border.col = "red")+
    tm_shape(kal_hcv1) + tm_borders(col = "red", group = "KAL HCV BORDERS")+
    tm_shape(orgutan_sp) + tm_dots(col = "blue")+
    tm_mouse_coordinates() + tm_scale_bar()
  
  # plot map mode
  tmap_mode("plot")
  osm_kal <- read_osm(kal_shp1, ext = 1.5, type = "esri-topo", xlim = c(-0.05, 1.08), ylim = c(-0.17,1.05), relative= T)
  tm_shape(osm_kal) + tm_rgb() +
  tm_shape(kal_shp1, projection = 32749) + tm_polygons(alpha = 0.4, col = "cornsilk",legend.show = F) +
    tm_shape(kal_hcv) + tm_fill(col = "HCV", alpha = 0.7, palette = "Dark2", area = "HECTARES") + 
    tm_legend(legend.position = c(0.7,0.05), legend.outside = F) +
    tm_grid(projection = 4326, alpha = 0.7) +
    tm_compass(position = c(0.24, 0.1), just = 2, size = 0.9) + 
    tm_layout(main.title = "PT Kayung Agro Lestari", main.title.position = c(0.45,0), main.title.size = 1.5, 
              legend.outside.size = 0.36, scale = 1, legend.title.fontface = "bold", legend.text.fontface = "bold",
              main.title.fontface = "bold", legend.text.size = 0.7, title = "High Conservation Value Area",
              title.position = c(0.65,0.95), title.bg.color = "lightblue") +
    tm_scale_bar(breaks = c(0,2,4,6), position = c("left", "bottom"), text.size = 0.6) + 
    tm_xlab("Longitude", size = 1) + tm_ylab("Latitude", size = 1, space = -0.5) +
    tm_credits("23-09-2020", position = c(0.9,0)) + 
    tm_logo("./spatial_data_analysis/anj.png", height = 1.5, position = c(-0.01,0.91))
      
  

  