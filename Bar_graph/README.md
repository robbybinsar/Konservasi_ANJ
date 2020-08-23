---
title: "Bar Graph (ggplot) with observation research data of plant medium"
author: "Robby"
date: "8/23/2020"
output: 
  html_document:
    keep_md: TRUE
    
---




## Basic Bar Graph Using ggplot


### 1. `Multiple_var_bargraph.R` (R script)

The bar graph from the `Multiple_var_bargraph.R` script will create a bar plot using ggplot from observation research of Trembesi plant with 5 testing independent variables of planting medium they are as follows:  
a. 100% tanah  
b. 50% tanah 50% kompos  
c. 100% kompos  
d. 50% tanah 50% jangkos  
e. 100% jangkos  

The 3 dependent variables that are being measured are:  
a. Tinggi tanaman  
b. Jumlah petiole  
c. Jumlah petiqlule  

Several steps through the `Multiple_var_bargraph.R` script are:

**i. Loading and preparing data**

  - `df` is created with `read.delim` function to read data `Trembesi_summary.txt` (50 obs, 4 var)  

**ii. Gather the data to long format so the variables are all in one column**
  
  - `longdata` is created using `pivot_longer` from `tidyr` package resulting a long format dataframe where the 3 dependent variables are put into one column named as variable and the value of those 3 variables are placed in the other column (150 obs, 3 var).
  
**iii. Create summary statistics**

  - `summary` is created using `summarySE` function from `Rmisc` package resulting a summarised dataframe that consists of N (count of obs), value, sd (standard deviation), se (standard error), and ci (confidence interval with 95% as default) (15 obs, 7 var)
  
**iv. Transform into relative frequency**

  - `dat` is created from 3 processes:
    a. with `ddply` function to transform/ add columns rel_freq = relative frequency of the value and se_freq = relative frequency of standard error.
    b. Dropping the value column
    c. Adding another column that contains the notation from the corresponding post hoc test.
  
  The output: 15 obs, 10 var

**v. Sorting x axis variable fill**

  - This step is optional, it sorted the variable using `factor` with levels from left to right as will be displayed in the bar plot.
  
**vi. ggplot graphic**

  - `grafik` is created using `ggplot` function and exported with `png` to current  directory named as `summary_multiplebar_Trembesi.png`
  
### 2. `Summary_bar_graph_Trembesi_Analisis_statistik.md` (R markdown)

In `Summary_bar_graph_Trembesi_Analisis_statistik.md` explain a step by step process executed from the scripts with the Trembesi obervation data as mentioned in the `Multiple_var_bargraph.R`

### 3. `summary_multiplebar_Trembesi.png`

`summary_multiplebar_Trembesi.png` is the result from the `Multiple_var_bargraph.R` and `Summary_bar_graph_Trembesi_Analisis_statistik.md`
  


