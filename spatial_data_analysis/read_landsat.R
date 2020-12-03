#https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/landsat-data-in-r-geotiff/
  # load spatial packages
  library(raster)
  library(rgdal)
  library(rgeos)
  # turn off factors
  options(stringsAsFactors = FALSE)
  # get list of all tifs
  list.files("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1")
  # but really you just want the tif files
  all_landsat_bands <- list.files("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1",
                                  pattern = ".TIF$",
                                  full.names = TRUE) # make sure you have the full path to the file
  all_landsat_bands <- all_landsat_bands[-c(12, 2,3,11,10)]
  
  # stack the data
  landsat_stack_csf <- stack(all_landsat_bands)
  # then turn it into a brick
  landsat_csf_br <- brick(landsat_stack_csf)
  # view stack attributes
  landsat_csf_br
  plotRGB(landsat_csf_br,
          r = 4, g = 3, b = 2,
          stretch = "lin",
          axes = TRUE,
          main = "RGB composite image\n Landsat Bands 4, 3, 2")
  box(col = "white")

#https://rpubs.com/ials2un/getlandsat
  ## Load packages
  library(getSpatialData)
  library(raster)
  library(sf)
  library(sp)
  library(tidyverse)
  library(rgdal)
  ## Use st_read function from the  sf library
  SMM_HGU <- st_read(dsn = "./spatial_data_analysis/SMM/shp_HGU_SMM", layer = "HGU_SMM")
  # but really you just want the tif files
  all_landsat_bands <- list.files("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1",
                                  pattern = ".TIF$",
                                  full.names = TRUE) # make sure you have the full path to the file
  all_landsat_bands <- all_landsat_bands[c(4, 5,6)]
  
  # stack the data
  landsat_stack_csf <- stack(all_landsat_bands)
  # then turn it into a brick
  landsat_csf_br <- brick(landsat_stack_csf)
  ## if necessary, reproject AOI to file CRS
  aoi <- st_transform(SMM_HGU, crs(landsat_csf_br))
  ## crop landsat raster to a new file
  ## uncomment for running
  r_crop <- crop(landsat_csf_br, aoi, filename = "testaja.TIF")
  plotRGB(r_crop,
          r = 3, g = 2, b = 1,
          stretch = "lin",
          axes = TRUE,
          main = "RGB composite image\n Landsat Bands 4, 3, 2")
  box(col = "white")
  ## Read the cropped NDVI
  r <- raster("./testaja.TIF")
  library(leaflet)
  library(RColorBrewer)
  pal <- colorNumeric(c("green", "red", "blue"), values(r),
    na.color = "transparent")
  leaflet() %>% addTiles() %>%
    addRasterImage(r,colors = pal, opacity = 0.8) %>%
    addPolygons(data = SMM_HGU, fill = F)
  
B2 <- raster("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1/LC08_L1TP_122062_20190404_20190421_01_T1_B2.tif")
B3 <- raster("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1/LC08_L1TP_122062_20190404_20190421_01_T1_B3.tif")
B4 <- raster("./spatial_data_analysis/SMM/LC08_L1TP_122062_20190404_20190421_01_T1/LC08_L1TP_122062_20190404_20190421_01_T1_B4.tif")
  
coba <- stack(B2, B3, B4)

#https://rspatial.org/raster/rs/2-exploration.html
  
  