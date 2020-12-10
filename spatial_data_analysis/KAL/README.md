PT KAL Spatial Data Analysis
================
Robby Butarbutar
5/4/2020

#### R script: [mapping\_kal.R](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/spatial_data_analysis/KAL/mapping_kal.R)

The `mapping_kal.R` is created as a base R codes for spatial analysis
specifically in PT KAL Ketapang.

There are several steps in the script, they are as follows:

1.  Retrieving basemap from GGMAP (ggmap package)
    
      - `getmap` function : get\_map is a smart wrapper that queries the
        Google Maps, OpenStreetMap, Stamen Maps or Naver Map servers for
        a map.

2.  Read shapefiles for concession and high conservation value area
    boundaries

<!-- end list -->

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

<!-- end list -->

3.  Read points geometry from Orang utan survey
    
      - The geometries are read in the csv file format
      - Use `st_as_sf` function to transform data frame containing
        geometries into simple feature object
      - Use `SpatialPointsDataframe` function to read it in sp object

4.  Read points geometry for Orang utan forage trees and Create Buffer
    
      - In this phase, `readOGR` is used to read the file with shapefile
        format.
      - Then, transform the crs based on crs for PT KAL Concession
      - Use `st_buffer` to create buffer with 500 m in diameter.
          - `st_buffer` function: computes a buffer around this
            geometry/each geometry

5.  Plot the spatial data

<!-- end list -->

  - Plot using **ggmap** + **ggplot** (method 1)
  - Plot using **tmap** + **tmaptools** + **leaflet** (method 2)
      - Method 2 will generate both static plot map and interactive map

### R script visualizing Orang utan survey with transects in PT KAL conservation area on September 16th to 23rd, 2020 can be checked in folder [`sampling_16092020_23092020`](https://github.com/robbybinsar/Konservasi_ANJ/tree/master/spatial_data_analysis/KAL/sampling_16092020_23092020)
