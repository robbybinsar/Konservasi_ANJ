---
title: "README"
author: "Robby"
date: "8/25/2020"
output: 
  html_document:
    keep_md: TRUE
    
---



## Update Baseline Kehati

`update_baseline_kehati.R` is an R script that have the function as readline prompt to interactively insert biodiversity data with R console. The output of the data inserted will be exported to a designated xlsx file.


1. `readData_fauna <- function(UM, bulan)` and `readData_flora <- function(UM, bulan)`these function require 2 arguments:  

    - UM : unit manajemen which is the location of the in regards with the discoevery of species
    - bulan : Month in English, is the discovery month

2. The output from the function is `added_fauna` for fauna species and `added_flora` for flora species that contains all inserted data.
  
3. The designated file for exporting is located in `"./pattern_match/pattern_match.xlsx"`


