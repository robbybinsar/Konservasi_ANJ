library(leaflet)
library(sf)
library(raster)
library(sp)
library(htmltools)
library(leafpop)
library(leafem)
library(htmlwidgets)

#read images
img_JKE <- paste0(c(rep("./spatial_data_analysis/SMM/GRTT_POINT_2020/GROUND_CHECK_GRTT_JKE/images/JKE-")), seq(18), ".jpg")
img_LJE <- paste0(c(rep("./spatial_data_analysis/SMM/GRTT_POINT_2020/GROUND_CHECK_GRTT_LJE/images/LJE-")), seq(19), ".jpg")


#read HGU SMM
SMM_HGU <- st_read(dsn = "./spatial_data_analysis/SMM/shp_HGU_SMM", layer = "HGU_SMM")
#read SMM HCV shapefiles
SMM_hcv <- st_read(dsn = "./spatial_data_analysis/SMM/20200428 - Peta Potensi Area HCV", layer = "20200428_HA_AREA_HCV_POTENSIAL")
#read GRTT
GRTT_JKE <- st_read(dsn = "./spatial_data_analysis/SMM/GRTT_POINT_2020/GROUND_CHECK_GRTT_JKE", layer = "GROUND_CHECK_GRTT_JKE")
GRTT_LJE <- st_read(dsn = "./spatial_data_analysis/SMM/GRTT_POINT_2020/GROUND_CHECK_GRTT_LJE", layer = "GROUND_CHECK_LJE_2020_1")
ARE_perlin <- read.csv("./spatial_data_analysis/SMM/GRTT_POINT_2020/GROUND_CHECK_GRTT_ARE.csv")
ARE_perlin_sf <- st_as_sf(ARE_perlin, coords = c("X", "Y"), crs = 32748)
ARE_perlin_sf <- st_transform(ARE_perlin_sf, crs = crs(GRTT_JKE, asText = TRUE))
#Preparing labels
  #JKE Labels
  GRTT_JKE_NG <- st_set_geometry(GRTT_JKE, NULL)
  labs_JKE <- lapply(seq(nrow(GRTT_JKE_NG)), function (i) {
    paste0("<p>", GRTT_JKE_NG[i, "name"], "<p></p>",
           GRTT_JKE_NG[i, "desc"], "</p>")
  })
  #LJE Labels
  GRTT_LJE_NG <- st_set_geometry(GRTT_LJE, NULL)
  labs_LJE <- lapply(seq(nrow(GRTT_JKE_NG)), function (i) {
    paste0("<p>", GRTT_LJE_NG[i, "name"], "<P></P>",
           GRTT_LJE_NG[i, "desc"], "</p>")
  })
  

SMM_HGU <- st_transform(SMM_HGU, crs = crs(GRTT_JKE, asText = TRUE))
SMM_hcv <- st_transform(SMM_hcv, crs = crs(GRTT_JKE, asText = TRUE))
SMM_hcv <- st_zm(SMM_hcv)

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = SMM_HGU, weight = 2, popup = popupTable(SMM_HGU), fillOpacity = 0.1)%>%
  addPolygons(data = SMM_hcv, weight = 1, color = "yellow", popup = popupTable(SMM_hcv), fillOpacity = 0.1) %>%
  addAwesomeMarkers(data = GRTT_JKE, popup = popupImage(img_JKE, src = "local", embed = T), label = lapply(labs_JKE, HTML), 
                    icon = awesomeIcons(markerColor = "red", icon = "ios-close", iconColor = "black", library = "ion")) %>%
  addAwesomeMarkers(data = GRTT_LJE, popup = popupImage(img_LJE, src = "local", embed = T), label = lapply(labs_LJE, HTML), 
                    icon = awesomeIcons(markerColor = "blue", icon = "ios-close", iconColor = "black", library = "ion")) %>%
  addMouseCoordinates() %>% addScaleBar()

# Save widget
saveWidget(m, "test.html", selfcontained = T)
