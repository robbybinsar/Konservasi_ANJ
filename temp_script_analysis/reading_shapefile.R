r_ter_mammals <- rasterizeIUCN(dsn= "./rawdata/MAMMALS/MAMMALS.shp", resolution=0.5, 
                               seasonal=c(1,2), origin=1, presence=c(1,2),
                               save=TRUE, path= "./rawdata/MAMMALS")


sr_amphibians <- calcSR(species_names= amphibians$binomial, path= "./rawdata")
raster::plot(sr_amphibians)

library(raster)
grey <- raster("./rawdata/MAMMALS/raster/MAMMALSAbditomys_latidens_0.5.tif")
rgb <- brick("./rawdata/MAMMALS/raster/MAMMALSAbditomys_latidens_0.5.tif")
plot(imported_raster)
plotRGB(imported_raster)

library(tiff)
readfile <- readTIFF("./rawdata/MAMMALS/raster/MAMMALSAbditomys_latidens_0.5.tif")

library(raster)
str_name<-"./rawdata/MAMMALS/raster/MAMMALSAbditomys_latidens_0.5.tif" 
imported_raster <- raster(str_name)
summary(imported_raster)

anura <- readShapePoly("./rawdata/MAMMALS/raster/MAMMALSAbditomys_latidens_0.5.tif")



# METHOD 2
library(sp)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(rgdal)
#1 read
testing1 <- st_read("MAMMALS.shp")
coba <- test$binomial["Macaca fascicularis"]
#2r read
test <- readOGR(dsn = "./rawdata/MAMMALS_TERRESTRIAL_ONLY", layer = "MAMMALS_TERRESTRIAL_ONLY")
obj <- test$binomial == "Macaca fascicularis"
obj1 <- test[obj,]

tmap_mode("view")
tm_shape(obj1, projection = 4326) +
  tm_polygons(c("island", "subspecies"),alpha = 0.7) +
  tm_facets(sync = T, ncol = 1, nrow = 2)

#method with ggplot
gm <- get_map(coba)
