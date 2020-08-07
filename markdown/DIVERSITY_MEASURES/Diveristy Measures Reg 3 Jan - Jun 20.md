---
title: |
    | Diversity Measures PT ANJ Reg 3 
    | January - June 2020
author: |
    | Robby B
    | Conservation Data Staff
date: "8/7/2020"
output: 
    pdf_document: 
      keep_md: TRUE
      toc: TRUE
      latex_engine: xelatex
geometry: margin=1in
---



**Load packages**


```r
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(openxlsx)
library(knitr)
```

## A. Species Richness Index 


```r
subjek <- "Region 3"
range_x <- "Jan - Jun 2020"
filedata <- "data_jml_spesies_ANJ.xlsx"
```



Table: Species Richness index Jan-June 2020

|   |date       |sites | richness_index|
|:--|:----------|:-----|--------------:|
|5  |2020-01-01 |PMP   |       2.911639|
|6  |2020-01-01 |PPM   |       7.371780|
|7  |2020-01-01 |ANJAP |       8.782675|
|12 |2020-02-01 |PMP   |       2.569492|
|13 |2020-02-01 |PPM   |       6.483391|
|14 |2020-02-01 |ANJAP |       8.875195|
|19 |2020-03-01 |PMP   |       1.063490|
|20 |2020-03-01 |PPM   |       7.914943|
|21 |2020-03-01 |ANJAP |       9.189937|
|26 |2020-04-01 |PMP   |       3.146580|
|27 |2020-04-01 |PPM   |       8.287632|
|32 |2020-05-01 |PMP   |       4.398521|
|33 |2020-05-01 |PPM   |       7.089498|
|34 |2020-05-01 |ANJAP |       8.940376|
|39 |2020-06-01 |PMP   |       1.857897|
|40 |2020-06-01 |PPM   |       8.317466|
|41 |2020-06-01 |ANJAP |       9.280048|


![](Diveristy Measures Reg 3 Jan - Jun 20_files/figure-latex/d-1.pdf)<!-- --> 

## B. Diversity Index


Table: Diversity index Jan-June 2020

|   |date       |sites | diversity_index|
|:--|:----------|:-----|---------------:|
|5  |2020-01-01 |PMP   |       2.0237753|
|6  |2020-01-01 |PPM   |       3.2566683|
|7  |2020-01-01 |ANJAP |       2.9766238|
|12 |2020-02-01 |PMP   |       1.7478681|
|13 |2020-02-01 |PPM   |       2.9830337|
|14 |2020-02-01 |ANJAP |       3.1154126|
|19 |2020-03-01 |PMP   |       0.8961481|
|20 |2020-03-01 |PPM   |       3.4368273|
|21 |2020-03-01 |ANJAP |       3.0523042|
|26 |2020-04-01 |PMP   |       2.1625568|
|27 |2020-04-01 |PPM   |       3.3388135|
|32 |2020-05-01 |PMP   |       2.5673845|
|33 |2020-05-01 |PPM   |       3.1123722|
|34 |2020-05-01 |ANJAP |       2.7807200|
|39 |2020-06-01 |PMP   |       1.7007947|
|40 |2020-06-01 |PPM   |       3.2881334|
|41 |2020-06-01 |ANJAP |       3.0594664|

![](Diveristy Measures Reg 3 Jan - Jun 20_files/figure-latex/f-1.pdf)<!-- --> 

## C. Evenness Index


Table: Evenness index Jan-June 2020

|   |date       |sites | evenness_index|
|:--|:----------|:-----|--------------:|
|5  |2020-01-01 |PMP   |      0.8789144|
|6  |2020-01-01 |PPM   |      0.8555180|
|7  |2020-01-01 |ANJAP |      0.7427942|
|12 |2020-02-01 |PMP   |      0.9755038|
|13 |2020-02-01 |PPM   |      0.8032788|
|14 |2020-02-01 |ANJAP |      0.7578468|
|19 |2020-03-01 |PMP   |      0.5568081|
|20 |2020-03-01 |PPM   |      0.8741047|
|21 |2020-03-01 |ANJAP |      0.7208851|
|26 |2020-04-01 |PMP   |      0.9018562|
|27 |2020-04-01 |PPM   |      0.8491764|
|32 |2020-05-01 |PMP   |      0.9061741|
|33 |2020-05-01 |PPM   |      0.8274944|
|34 |2020-05-01 |ANJAP |      0.6545188|
|39 |2020-06-01 |PMP   |      0.7386458|
|40 |2020-06-01 |PPM   |      0.8281843|
|41 |2020-06-01 |ANJAP |      0.7108318|

![](Diveristy Measures Reg 3 Jan - Jun 20_files/figure-latex/h-1.pdf)<!-- --> 
