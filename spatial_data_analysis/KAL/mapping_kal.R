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
set.seed(8)
orgutan <- orgutan %>% relocate(spesies) %>% select(-X) %>% 
  mutate(date  = sample(seq(as.Date('2020/10/01'), as.Date('2020/12/28'), by="day"), 12)) %>%
  mutate(month = months(date)) %>% transform(lon = runif(12, min = 110.11327, max = 110.14557), 
                                             lat = runif(12, min = -1.40080, max = -1.35671))
orgutan_sf <- st_as_sf(orgutan, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
orgutan_sp <- SpatialPointsDataFrame(coords = orgutan[,2:3], data = orgutan, proj4string = CRS('+init=EPSG:4326'))
# read data points for pakan orang utan
set.seed(13)
#pakan <- data.frame(pakan = rep("pakan", 18),spesies = sample(LETTERS, 18, replace = T), nama_lokal= sample(letters, 18, replace = T),
                    #lon = runif(18, min = 110.11327, max = 110.14557), lat = runif(18, min = -1.40080, max = -1.35671))
#pakan_sp <- SpatialPointsDataFrame(coords = pakan[,4:5], data = pakan, proj4string = CRS('+init=EPSG:4326'))

pakan_sp <- readOGR(dsn = "./spatial_data_analysis/KAL/ex_pakan", layer = "ex_pakan_orgutan")
pakan_sp_buffer <- readOGR(dsn = "./spatial_data_analysis/KAL/ex_pakan", layer = "ex_pakan_buffer")

pakan_sf <- st_as_sf(pakan_sp)
pakan_sf_buffer <- st_as_sf(pakan_sp_buffer)

#read in raster file
library(raster)
raster_kal <- raster("spatial_data_analysis/KAL/123/LC08_L1TP_121061_20190904_20190917_01_T1.tif")

# plot using ggmap + ggplot
  library(ggplot2)
  library(ggspatial)
  library(ggmap)
  library(RColorBrewer)
  library(png)
  library(grid)
  library(gridExtra)
  anjpng <- readPNG("./spatial_data_analysis/anj.png")
  #1
  ggmap(kal_base, base_layer = ggplot(data = kal_shp1)) +
    geom_sf(color = "black",inherit.aes = F, alpha = 0.2, lwd = 0.3, show.legend = F, fill = "cornsilk") +
    xlab("Longitude") + ylab("Latitude") + 
    ggtitle("PT Kayung Agro Lestari", subtitle = "High Conservation Value Area") + annotation_scale(location = "br") + 
    annotation_north_arrow(height = unit(1, "cm"), width = unit(0.8, "cm"),
                           pad_y = unit(0.8, "cm"), pad_x = unit(2.5, "cm"), 
                           style = north_arrow_fancy_orienteering, location = "br") +
    geom_sf(data = kal_hcv, alpha = 0.5, aes(fill = HCV, color = ENG_NAME), inherit.aes = T) + 
    labs(color = "Lokasi")+
    scale_color_manual(values = c(brewer.pal(n=9, "Set1"), "#4D004B", "#810F7C", "#BFD3E6", "#9EBCDA", "#8C96C6", "#88419D"))+
    scale_fill_brewer(palette = "Set1")+
    theme(panel.grid = element_line(colour = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.ontop = T, panel.background = element_blank(), legend.spacing = unit(0.1, "cm"), 
          legend.background = element_blank()) +
    labs(caption = "21-09-2020") +
    annotation_custom(rasterGrob(anjpng, width = 0.5, height = 0.5), xmin = 110.27, xmax = 110.32, ymin = -1.33, ymax = -1.31)+
    coord_sf(crs = st_crs(4326))
  
    #2
    hcv_657 <- c(lon = 110.13358, lat = -1.3809)
    hcv_657_base <- get_map(location = hcv_657, zoom = 14, scale = 1, maptype = "satellite", source = "google")
    ggmap(hcv_657_base)
    ggmap(hcv_657_base, base_layer = ggplot(data = kal_shp1)) +
    geom_sf(color = "grey88",inherit.aes = F, alpha = 0, lwd = 0.6, show.legend = F) +
    xlab("Longitude") + ylab("Latitude") + 
    ggtitle("PT Kayung Agro Lestari", subtitle = "Sebaran Orang utan dan Tumbuhan Pakan") + 
      annotation_scale(location = "br", pad_y = unit(0.7, "cm"), pad_x = unit(1.3,"cm")) + 
    annotation_north_arrow(height = unit(1, "cm"), width = unit(0.8, "cm"),
                           pad_y = unit(1.2, "cm"), pad_x = unit(2.5, "cm"), 
                           style = north_arrow_fancy_orienteering, location = "br") +
    geom_sf(data = kal_hcv[c(3,15),], alpha = 0.4, aes(fill = ENG_NAME), inherit.aes = F, color = "black", lwd = 1) + 
      labs(fill = "Lokasi")+
    scale_color_manual(values = c(brewer.pal(n=9, "Set1"), "#4D004B", "#810F7C", "#BFD3E6", "#9EBCDA", "#8C96C6", "#88419D"))+
    scale_fill_brewer(palette = "Set2")+
    geom_sf_text(data = kal_hcv[c(3,15),], aes(label = HCV), color = "white", size = 3)+
    geom_sf(data = orgutan_sf, inherit.aes = F, aes(color = month, size = diameter_sarang_meter)) + 
      scale_size(breaks = c(1.5,2,3)) +
    labs(color = "Bulan Observasi", size = "Diameter Sarang (m)")+
    geom_sf(data = pakan_sf, inherit.aes = F, aes(shape = pakan), col = "magenta") + labs(shape = "Pakan (Radius 250 m)") +
      scale_shape_manual(values = 3)+
    geom_sf(data = pakan_sf_buffer, alpha = 0, color = "black")+
    theme(panel.grid = element_line(colour = gray(0.5), linetype = "dashed", size = 0.2), 
            panel.ontop = T, panel.background = element_blank(), legend.spacing = unit(0.1, "cm"), 
            legend.background = element_blank()) +
    labs(caption = "21-09-2020") +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2), size = guide_legend(order = 3),
           shape = guide_legend(order = 4))+
    coord_sf(crs = st_crs(4326))
    

# map using tmap
  library(tmap)
  library(tmaptools)
  library(leaflet)
  
  #interactive map
  tmap_mode("view")
  tm_basemap(server = providers$Esri.WorldImagery)+
  tm_shape(kal_shp1) + tm_polygons(border.col = "yellow", alpha = 0, group = "KAL BORDER")+
    tm_shape(kal_hcv1) + tm_fill(col = "ENG_NAME", alpha = 0.8, title = "Lokasi", group = "KAL HCV AREA",
                                 popup.vars = c("Lokasi: " = "ENG_NAME", "HCV Categories: " = "HCV",
                                                "Hectares:" = "HECTARES"), border.col = "red")+
    tm_text(text = "HCV", size = 0.7, size.lowerbound = F) +
    tm_shape(kal_hcv1) + tm_borders(col = "red", group = "KAL HCV BORDERS")+
    tm_shape(pakan_sp_buffer) + tm_polygons(alpha = 0.2, group = "BUFFER PAKAN r:250 m") +
    tm_shape(orgutan_sp) + tm_dots(col = "month", size = "diameter_sarang_meter", scale = 0.3,
                                      popup.vars  = TRUE, palette = "Set1", group = "SARANG ORANG UTAN", title = "Bulan Observasi")+
    tm_shape(pakan_sp) + tm_dots(group = "TUMBUHAN PAKAN ORANGUTAN")+
    tm_mouse_coordinates() + tm_scale_bar()
  
  # plot map mode
  #1
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
    
  #2
  tmap_mode("plot")
  osm_kal <- read_osm(orgutan_sf, ext = 1.5, type = "esri-topo", xlim = c(-0.2, 1.5), ylim = c(-0.6,1.2), relative= T)
  tm_shape(osm_kal) + tm_rgb() +
    tm_shape(kal_shp1, projection = 32749) + tm_polygons(alpha = 0.4, col = "cornsilk",legend.show = F) +
    tm_shape(kal_hcv[c(3,15),]) + tm_fill(col = "HCV", alpha = 0.7, palette = "Dark2", area = "HECTARES") + 
    tm_legend(legend.outside = T) +
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
  

  