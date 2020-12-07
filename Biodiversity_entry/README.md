---
title: "README"
author: "Robby"
date: "8/25/2020"
output: 
  html_document:
    keep_md: TRUE
    
---



## Update Baseline Kehati

[`update_baseline_kehati.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/Biodiversity_entry/update_baseline_kehati.R) is an R script that have the function as readline prompt to interactively insert biodiversity data with R console. The output of the data inserted will be exported to a designated xlsx file.


1. `readData_fauna <- function(UM, bulan)` and `readData_flora <- function(UM, bulan)`these function require 2 arguments:  

    - UM : unit manajemen which is the location of the in regards with the discoevery of species
    - bulan : Month in English, is the discovery month
2.  The function runs both local database and an API-based function that collect data from the API-related server. Below is the list of the API server used:

    - GBIF Database
    - Open Tree of Life (OTL)
    - The Integrated Taxonomic Information System (ITIS)
    - Encyclopedia of Life (EOL)
    - World Register of Marine Species (WORMS)
    - National Center for Biotechnology Information (NCBI)
    - Species+
    - IUCN Redlist
    - Convention on Migratory Species
3. The output from the function is `added_fauna` for fauna species and `added_flora` for flora species that contains all inserted data. The output dataframe contains variables as follows:

    - NCBI ID (Fauna, Flora)
    - Class (Fauna, Flora)
    - Family (Fauna, Flora)
    - Latin name (Fauna, Flora)
    - English Name (Fauna, Flora)
    - Indonesian Name (Fauna, Flora)
    - Primary Diet (Fauna)
    - CITES Appendices (Fauna, Flora)
    - IUCN Category (Fauna, Flora)
    - Local conservation status (Government law) (Fauna, Flora)
    - Endemism status (Fauna, Flora)
    - CMS Appendices (Fauna)
    - Species Existence (Presence, Origin, Seasonality) (Fauna)
    - Habitat (Fauna)
  
4. The designated file for exporting is located in [`"./pattern_match/pattern_match.xlsx"`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match.xlsx)

## Entry Kehati Site Version

[`entry_kehati_site_version.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/Biodiversity_entry/entry_kehati_site_version.R) same with `update_baseline_kehati.R` only excluding

    - Indonesian Name
    - Primary Diet
    - Endemism Status
    - Species Existence (Presence, Origin, Seasonality)


