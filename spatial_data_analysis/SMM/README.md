PT SMM Spatial Data Analysis
================
Robby Butarbutar
12/4/2020

## R script: [`mapping_priority_species.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/spatial_data_analysis/SMM/mapping_priority_species.R)

This script contains code to visualize survey result of priority species
such as *Tarsius bancanus spp. saltator* in PT SMM Conservation area.

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

**1. Read shapefiles for concession and high conservation value area
boundaries**

  - `library(sf)`
      - `st_read` function: Read simple features from file or database,
        or retrieve layer names and their geometry type(s)

**2. Read coordinates of priority species encounters**

  - Load csv file named
    [`priority_species.csv`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/spatial_data_analysis/SMM/priority_species.csv)
    that contains information of species and their coordinates.
  - Convert the data frame into sf object with `st_as_sf` function
  - Transform the coordinate reference system (crs) according to the crs
    of PT SMM conservation area (sf file) with `st_transform`

**3. Create buffer 100 m in diameter**

`st_buffer` function: computes a buffer around this geometry/each
geometry

**4. Map spatial data in interactive map with tmap**

`tmap package`: Thematic maps are geographical maps in which spatial
data distributions are visualized. This package offers a flexible,
layer-based, and easy to use approach to create thematic maps, such as
choropleths and bubble maps. It is based on the grammar of graphics, and
resembles the syntax of ggplot2.

## R script: [`interactive_leaflet_GRTT.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/spatial_data_analysis/SMM/interactive_leaflet_GRTT.R)

This R script contains code that viualize result of ground check for
GRTT (Ganti Rugi Tanam Tumbuh) in PT SMM Concession and conservation
area.

    library(leaflet)
    library(sf)
    library(raster)
    library(sp)
    library(htmltools)
    library(leafpop)
    library(leafem)
    library(htmlwidgets)

**1. Load ground check images as variables**

**2. Read shapefiles of concession and high conservation value area
boundaries**

  - `library(sf)`
      - `st_read` function: Read simple features from file or database,
        or retrieve layer names and their geometry type(s)

**3. Read shapefiles of ground check point coordinates**

**4. Preparing data attribute**

  - Remove geometries data from data frame with `st_set_geometry`
    function
  - Using `lapply` to retrieve list of information from the data frame
    that will be labelled into the data attribute.

**5. Interactive map with leaflet package**

In this step the images that’s been loaded will be embedded into the
interactive map as a pop-up image.

**6. Save widget as HTML file**

`saveWidget` function: Save a rendered widget to an HTML file (e.g. for
sharing with others).
