library(ggmap)
library(ggspatial)
library(tmap)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(openxlsx)
library(grid)
library(gridExtra)

#read data points all for sarang orang utan
dir <- "./spatial_data_analysis/KAL/sampling_16092020_23092020/"
data_sarang <- read.xlsx(paste0(dir,"Tally Sheet_Survei Sarang OU_KAL_2020 - Copy.xlsx"), sheet = "Gabungan")
  #create sf class
  data_sarang_sf <- st_as_sf(data_sarang, coords = c("X", "Y"), crs = "+proj=utm +zone=49 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  #create sp class
  data_sarang_sp <- SpatialPointsDataFrame(coords = data_sarang[,4:5], data = data_sarang, proj4string = CRS('+init=EPSG:32749'))
#read transect
  transek_sf <- st_read(paste0(dir,"shp/transek.shp"))
  transek_sp <- readOGR(dsn = paste0(dir,"shp"), layer = "transek")
#read KAL boundaries and HCV
  #method 1 using st_read : get sf class object good to use for ggplot
  #HGU
  kal_shp1 <- st_read("./spatial_data_analysis/KAL/HGU_KAL/HGU KAL.shp")
  #HCV
  kal_hcv <- st_read("./spatial_data_analysis/KAL/HCV_KAL/KAL_HCV_AREA.shp")
  # method 2 using readOGR : get sp object for tmap
  library(rgdal)
  #HGU
  kal_shp2 <- readOGR(dsn = "./spatial_data_analysis/KAL/HGU_KAL", layer = "HGU KAL")  
  #HCV
  kal_hcv1 <- readOGR(dsn = "./spatial_data_analysis/KAL/HCV_KAL", layer = "KAL_HCV_AREA")

#plot using ggplot2
  #Keseluruhan
  kalcoor <- c(lon = 110.200, lat = -1.42)
  kal_base <- get_map(location = kalcoor, zoom = 12, scale = 1, maptype = "terrain", source = "stamen")
  ggmap(kal_base, base_layer = ggplot(data = kal_shp1)) +
    geom_sf(color = "black",inherit.aes = F, alpha = 0.2, lwd = 0.3, show.legend = F, fill = "cornsilk") +
    xlab("Longitude") + ylab("Latitude") + 
    ggtitle("PT Kayung Agro Lestari", subtitle = "Sampling Orang utan 16/09/2020 - 23/09/2020") + annotation_scale(location = "br") + 
    annotation_north_arrow(height = unit(1, "cm"), width = unit(0.8, "cm"),
                           pad_y = unit(0.8, "cm"), pad_x = unit(2.5, "cm"), 
                           style = north_arrow_fancy_orienteering, location = "br") +
    geom_sf(data = kal_hcv, alpha = 0.9, aes(fill = ENG_NAME), inherit.aes = F) + 
    labs(fill = "Lokasi")+
    geom_sf(data = transek_sf, color = "orange", lwd = 0.1)+
    geom_sf(data = data_sarang_sf, aes(color = Kelas.Sarang, size = `K.(cm)`), inherit.aes = F, shape = 4)+ labs(size = "Keliling Sarang (cm)")+
    scale_fill_manual(values = c(brewer.pal(n=9, "Set1"), "#4D004B", "#810F7C", "#BFD3E6", "#9EBCDA", "#8C96C6", "#88419D"))+
    scale_color_manual(values = c("red","purple","darkgreen", "blue"))+
    scale_size(range = c(0.2,2.5))+
    theme(panel.grid = element_line(colour = gray(0.5), linetype = "dashed", size = 0.1), 
          panel.ontop = T, panel.background = element_blank(), legend.spacing = unit(0.1, "cm"), 
          legend.background = element_blank()) +
    labs(caption = "Created: 28-09-2020") +
    annotation_custom(rasterGrob(anjpng, width = 0.5, height = 0.5), xmin = 110.27, xmax = 110.32, ymin = -1.33, ymax = -1.31)+
    coord_sf(crs = st_crs(4326))
  #HCV 657
  data_sarang_sf$Kelas.Sarang <- factor(data_sarang_sf$Kelas.Sarang, levels = c("A","B","C","D"))
  tmap_mode("plot")
  osm_kal <- read_osm(orgutan_sf, ext = 1.5, type = "esri-topo", xlim = c(-0.2, 1.6), ylim = c(-0.6,1.2), relative= T)
  tm_shape(osm_kal) + tm_rgb() +
    tm_shape(kal_shp1, projection = 32749) + tm_polygons(alpha = 0.4, col = "cornsilk",legend.show = F) +
    tm_shape(kal_hcv[c(3,15),]) + tm_polygons(col = "ENG_NAME", alpha = 0.4, palette = "Dark2", area = "HECTARES", title = "Lokasi HCV",
                                              border.col = "black") + 
    tm_legend(legend.outside = T) +
    tm_grid(projection = 4326, alpha = 0.7, lwd = 0.2, labels.size = 0.3) +
    tm_compass(position = c(0.88, 0.11), just = 2, size = 0.9) + 
    tm_layout(main.title = "Sampling Orang utan HCV 657 PT KAL", main.title.position = c(0.23,0), main.title.size = 0.7, 
              legend.title.fontface = "bold", legend.text.fontface = "bold",
              main.title.fontface = "bold", legend.text.size = 0.4, legend.title.size = 0.6, legend.width = 1,
              outer.margins = c(0.0001,0.001,0,0), legend.hist.size = 0.5) +
    tm_scale_bar(breaks = c(0,1,2), position = c("right", "bottom"), text.size = 0.6) +
    tm_shape(transek_sp[1:2,]) + tm_lines(col = "Tanggal", palette = "Set1", title.col = "Tanggal Transek")+
    tm_shape(data_sarang_sf[1:68,]) + tm_symbols(size  = "K.(cm)", shape = 4, col = "Kelas.Sarang", scale = 0.5, 
                                              legend.size.is.portrait = T, title.size = "Keliling Sarang (cm)",
                                              palette = "-YlOrBr", border.lwd = 0.4, legend.hist = T, 
                                              legend.hist.title = "Histogram Kelas Sarang")+
    tm_xlab("Longitude", size = 0.5) + tm_ylab("Latitude", size = 0.5) +
    tm_credits("Created: 28-09-2020", position = c(0.75,0), size = 0.4) + 
    tm_logo("./spatial_data_analysis/anj.png", height = 1.5, position = c(-0.01,0.89))
  # HCV 2330
  tmap_mode("plot")
  osm_kal <- read_osm(orgutan_sf, ext = 1.5, type = "esri-topo", xlim = c(0.6, 2.9), ylim = c(-3.7,-1.5), relative= T)
  tm_shape(osm_kal) + tm_rgb() +
    tm_shape(kal_shp1, projection = 32749) + tm_polygons(alpha = 0.4, col = "cornsilk",legend.show = F) +
    tm_shape(kal_hcv[12,]) + tm_polygons(col = "ENG_NAME", alpha = 0.4, palette = "Dark2", area = "HECTARES", title = "Lokasi HCV",
                                              border.col = "black") + 
    tm_legend(legend.outside = T) +
    tm_grid(projection = 4326, alpha = 0.7, lwd = 0.2, labels.size = 0.3) +
    tm_compass(position = c(0.9, 0.11), just = 2, size = 0.9) + 
    tm_layout(main.title = "Sampling Orang utan HCV 2330 PT KAL", main.title.position = c(0.23,0), main.title.size = 0.7, 
              legend.title.fontface = "bold", legend.text.fontface = "bold",
              main.title.fontface = "bold", legend.text.size = 0.4, legend.title.size = 0.6, legend.width = 1,
              outer.margins = c(0.001,0.001,0,0), legend.hist.size = 0.5) +
    tm_scale_bar(breaks = c(0,1,2), position = c("right", "bottom"), text.size = 0.6) +
    tm_shape(transek_sp[3:7,]) + tm_lines(col = "Tanggal", palette = "Set1", title.col = "Tanggal Transek")+
    tm_shape(data_sarang_sf[69:156,]) + tm_symbols(size  = "K.(cm)", shape = 4, col = "Kelas.Sarang", scale = 0.5, 
                                                 legend.size.is.portrait = T, title.size = "Keliling Sarang (cm)",
                                                 palette = "-YlOrBr", border.lwd = 0.4, legend.hist = T, 
                                                 legend.hist.title = "Histogram Kelas Sarang")+
    tm_xlab("Longitude", size = 0.5) + tm_ylab("Latitude", size = 0.5) +
    tm_credits("Created: 28-09-2020", position = c(0.75,0), size = 0.4) + 
    tm_logo("./spatial_data_analysis/anj.png", height = 1.5, position = c(-0.01,0.89))
  #interactive map with tmap
  tmap_mode("view")
  tm_basemap(server = providers$Esri.WorldImagery)+
    tm_shape(kal_shp1) + tm_polygons(border.col = "grey", alpha = 0, group = "KAL BORDER")+
    tm_shape(kal_hcv1) + tm_fill(col = "ENG_NAME", alpha = 0.6, title = "Lokasi", group = "KAL HCV AREA",
                                 popup.vars = c("Lokasi: " = "ENG_NAME", "HCV Categories: " = "HCV",
                                                "Hectares:" = "HECTARES"), border.col = "red")+
    tm_shape(kal_hcv1[c(12,15),])+ tm_text(text = "HCV", size = 0.7, size.lowerbound = F, ymod = -5, xmod = 2,
                                           group = "HCV Text 2330 dan 657") +
    tm_shape(kal_hcv1) + tm_borders(col = "black", group = "KAL HCV BORDERS")+
    tm_shape(transek_sp) + tm_lines(col = "Tanggal", palette = "Set1", title.col = "Tanggal Transek", popup.vars = T,
                                    group = "TRANSEK")+
    tm_shape(data_sarang_sp) + tm_dots(size  = "K.(cm)", col = "Kelas.Sarang", scale = 0.3,
                                                     palette = "-YlOrBr", popup.vars = T, group = "SARANG")+
    tm_mouse_coordinates() + tm_scale_bar()
  