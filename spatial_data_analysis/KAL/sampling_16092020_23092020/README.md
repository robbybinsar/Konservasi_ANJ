PT KAL Orang Utan Survey
================
Robby Butarbutar
11/10/2020

Survey conducted on **16 - 23 September 2020**

#### R script: [sampling\_sarang\_orangutan.R](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/spatial_data_analysis/KAL/mapping_kal.R)

**1. Loading all required packages**

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
    library(png)

**2. Read company’s logo with `readPNG`**

**3. Read coordinates for Orang utan nests across the transects**

    * The geometries are read in the xlsx file format
    * Use `st_as_sf` function to transform data frame containing geometries into simple feature object
    * Use `SpatialPointsDataframe` function to read it in sp object

**4. Import transects line (shapefile format) into R**

    There are two methods used:

  - `library(sf)`
      - `st_read` function: Read simple features from file or database,
        or retrieve layer names and their geometry type(s)
  - `library(rgdal)`
      - `readOGR` function:
          - The function reads an OGR data source and layer into a
            suitable Spatial vector object. It can only handle layers
            with conformable geometry features (not mixtures of points,
            lines, or polygons in a single layer). It will set the
            spatial reference system if the layer has such metadata.
          - If reading a shapefile, the data source name (dsn= argument)
            is the folder (directory) where the shapefile is, and the
            layer is the name of the shapefile (without the .shp
            extension). For example to read bounds.shp from C:/Maps, do
            map \<- readOGR(dsn=“C:/Maps”, layer=“bounds”). The logic
            behind this is that typically one keeps all the shapefiles
            for a project in one folder (directory).

**5. Read shapefiles for concession and high conservation value area
boundaries of PT KAL** \* `library(sf)` + `st_read` function: Read
simple features from file or database, or retrieve layer names and their
geometry type(s)

  - `library(rgdal)`
      - `readOGR` function:
          - The function reads an OGR data source and layer into a
            suitable Spatial vector object. It can only handle layers
            with conformable geometry features (not mixtures of points,
            lines, or polygons in a single layer). It will set the
            spatial reference system if the layer has such metadata.
          - If reading a shapefile, the data source name (dsn= argument)
            is the folder (directory) where the shapefile is, and the
            layer is the name of the shapefile (without the .shp
            extension). For example to read bounds.shp from C:/Maps, do
            map \<- readOGR(dsn=“C:/Maps”, layer=“bounds”). The logic
            behind this is that typically one keeps all the shapefiles
            for a project in one folder (directory).

**5. Plot the spatial data**

  - Plot using **ggmap** + **ggplot** (method 1)
  - Plot using **tmap** + **tmaptools** (method 2)
      - Method 2 will generate both static plot map and interactive map
