Alpha Diversity Measures
================
Robby B
8/26/2020

## Alpha Diversity

Alpha diversity is the diversity within a praticular area or ecosystem.

[`alpha_diversity_measure.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/Diversity_measures/Alpha_diversity_measures/alpha_diversity_measures.R)
is an R script that consists of several ecological analysis for alpha
diversity measures.  
The function in
[`alpha_diversity_measure.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/Diversity_measures/Alpha_diversity_measures/alpha_diversity_measures.R)
made up of 2 arguments `alpha_diversity <- function (dat, nama_sheet)`:

1.  `dat` : this argument is the file directory to the species count
    data (.csv)  
2.  `nama_sheet` : the name of the desired sheet to be analyzed usually
    a sheet of a particular date (Month, year, or a duration on a
    certain time)

The output are calculations of :  
1\. Species Number  
2\. Richness  
3\. Rarefaction  
4\. Heterogeneity Index  
5\. Evenness Index

The output of the calculation will be exported to `dat`

### Data Sample

To get more understanding on how the function works please refer to one
of the example for alpha diversity measures in markdown file
[`diversity_measures_SMM_2020.md`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/Diversity_measures/Alpha_diversity_measures/diversity_measures_SMM_2020.md)
on section A.
