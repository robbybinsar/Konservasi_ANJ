---
title: "Diversity Measures (alpha and beta)"
subtitle: "PT SMM 2020"
author: "Robby B"
date: "8/4/2020"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
---



**Load packages**


```r
library(tabula)
library(magrittr)
library(khroma)
library(ggplot2)
library(openxlsx)
library(RFLPtools)
library(knitr)
```

**Reading and preparing data**


```r
df <- read.xlsx("C:/Users/rbbutar/Documents/R/Konservasi_ANJ/indeks_kehati/SMM/Indeks_kehati_SMM_2015 - 2020.xlsx", 
                sheet = "2015", cols = 1:5)
df <- df[,-2]
rownames(df) <- df[,1]
df <- df[,-1]
df <- t(df)

#Dropping columns sum 0
df <- as.data.frame(df)
df <- df[,!sapply(df, function(x) sum(x))==0]
print(df)
```

```
##                 Cervus unicolor Manis javanica Muntianus muntjak Ratufa affinis
## Sempadan_sungai               0              0                 2              4
## Hutan_balok                   0              2                 0             15
## Konservasi_KBKT               2              1                 2              0
##                 Trachypitechus auratus Alcedo coerulescens Ceyx ruridorsa
## Sempadan_sungai                      0                   9             15
## Hutan_balok                          0                   0              7
## Konservasi_KBKT                      2                  12             19
##                 Egretta alba Egrretta intermedia Halcyon coromanda
## Sempadan_sungai            6                   3                 2
## Hutan_balok                0                   0                 0
## Konservasi_KBKT            8                   0                 0
##                 Halcyon smyrnenis Haliaeetus leucogaster Ictinaetus malayensis
## Sempadan_sungai                51                      2                     0
## Hutan_balok                    32                      0                     2
## Konservasi_KBKT                87                      0                     1
##                 Nectarinia jugularis Pitta sordida Spilornis cheela
## Sempadan_sungai                   33             0                4
## Hutan_balok                        0             5                0
## Konservasi_KBKT                   46            13                8
##                 Acicipiter gularis Anthracoceros malayanus
## Sempadan_sungai                  8                       0
## Hutan_balok                      6                       4
## Konservasi_KBKT                  0                       0
##                 Arachnotera longilostra Arachnotera robusta Elanus caeruleus
## Sempadan_sungai                       6                   0               12
## Hutan_balok                           0                   4                7
## Konservasi_KBKT                      13                   0               17
##                 Pitta moluccensis Pelargopsis capensis Loriculus galgulus
## Sempadan_sungai                 4                    0                  0
## Hutan_balok                     0                    5                  0
## Konservasi_KBKT                 6                   11                  2
##                 Psipologon raflesii Acridotheres javanicus Harpactes duvaucelii
## Sempadan_sungai                   0                      6                    2
## Hutan_balok                       0                      0                    4
## Konservasi_KBKT                   3                      0                    0
##                 Meiglyptes tukki Psittacula longicauda Rollulus rouloul
## Sempadan_sungai                8                     3                0
## Hutan_balok                    0                     0                2
## Konservasi_KBKT               17                     5                1
##                 Stachyris maculata Treron capellei Crocodilus porosus
## Sempadan_sungai                  3               0                  8
## Hutan_balok                      6               7                  0
## Konservasi_KBKT                  8               0                  0
```

**Creating sorted plot template**


```r
make.sorted.plot <- function(x){
    ordered <- sort(x, T)
    plot(
        ordered,
        col = terrain.colors(10),
        xaxt = "n", pch = 16, cex = 2,
        ylim = c(min(ordered)*0.5, max(ordered)*1.2),
        xlim = c(0, length(x)+1),
        ylab = "Index", xlab = "Sites",
        main = substitute(x))
    text(ordered,
         names(ordered),
         srt = -75,
         pos = 4)
}
```


## A. Alpha - Diversity

Alpha diversity is the diversity within a praticular area or ecosystem

### 1. Richness and Rarefaction

The number of different taxa, provides an instantly comprehensible expression of diversity. While the number of taxa within a sample is easy to ascertain, as a term, it makes little sense: some taxa may not have been seen, or there may not be a fixed number of taxa (e.g. in an open system; Peet 1974). As an alternative, richness (R) can be used for the concept of taxa number (McIntosh 1967). Richness refers to the variety of taxa/species/types present in an assemblage or community (Bobrowsky and Ball 1989) as “the number of species present in a collection containing a specified number of individuals” (Hurlbert 1971).

It is not always possible to ensure that all sample sizes are equal and the number of different taxa increases with sample size and sampling effort (Magurran 1988). Then, rarefaction (S^) is the number of taxa expected if all samples were of a standard size n (i.e. taxa per fixed number of individuals). Rarefaction assumes that imbalances between taxa are due to sampling and not to differences in actual abundances.

### a. Species Number


```r
spec.numb <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(spec.numb) <- c("site","species.number")
    for(i in seq_len(nrow(df))) {
        template <- data.frame(site = rownames(df)[i], 
                                species.number = sum(df[i,]!=0))
        spec.numb <- rbind(spec.numb,template)
    }
kable(spec.numb, caption = "Species Number")
```



Table: Species Number

|site            | species.number|
|:---------------|--------------:|
|Sempadan_sungai |             21|
|Hutan_balok     |             15|
|Konservasi_KBKT |             22|

### b. Richness Index

index_richness returns sample richness


```r
richness_index <- df %>% as_count() %>% index_richness(method = "margalef")
kable(richness_index@index, caption = "Richness Index Margalef", 
      col.names = "Richness_index")
```



Table: Richness Index Margalef

|                | Richness_index|
|:---------------|--------------:|
|Sempadan_sungai |       3.807875|
|Hutan_balok     |       2.990091|
|Konservasi_KBKT |       3.717489|

```r
make.sorted.plot(richness_index@index)
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### c. Composition Index

index_composition returns asymptotic species richness.


```r
composition_ind <- df %>% as_count() %>% index_composition(method = "chao1")
kable(composition_ind@index, 
      caption = "Composition Index with chao1 estimator", col.names = "composition_index")
```



Table: Composition Index with chao1 estimator

|                | composition_index|
|:---------------|-----------------:|
|Sempadan_sungai |          21.00000|
|Hutan_balok     |          15.00000|
|Konservasi_KBKT |          23.12104|

### d. Rarefaction

rarefaction returns Hurlbert's unbiased estimate of Sander's rarefaction.


```r
rarefact <- df %>% as_count() %>% rarefaction(sample = 50, method = "hurlbert", simplify = T)
kable(rarefact, 
      caption = "Rarefaction Hurlbert's unbiased, n = 50",col.names = "rarefaction")
```



Table: Rarefaction Hurlbert's unbiased, n = 50

|                | rarefaction|
|:---------------|-----------:|
|Sempadan_sungai |    15.92802|
|Hutan_balok     |    13.74734|
|Konservasi_KBKT |    14.49910|


### 2. Heterogeneity and Evenness

Diversity measurement assumes that all individuals in a specific taxa are equivalent and that all types are equally different from each other (Peet 1974). A measure of diversity can be achieved by using indices built on the relative abundance of taxa. These indices (sometimes referred to as non-parametric indices) benefit from not making assumptions about the underlying distribution of taxa abundance: they only take relative abundances of the species that are present and species richness into account. Peet (1974) refers to them as indices of heterogeneity (H).

Diversity indices focus on one aspect of the taxa abundance and emphasize either richness (weighting towards uncommon taxa) or dominance (weighting towards abundant taxa; Magurran 1988).

Evenness (E) is a measure of how evenly individuals are distributed across the sample.

### a. Diversity Index


```r
heterogeneity <- df %>% as_count() %>% index_heterogeneity(method = "shannon")
kable(heterogeneity@index, 
      caption = "DIversity Index Shannon-Wiener Method", col.names = "diversity_index")
```



Table: DIversity Index Shannon-Wiener Method

|                | diversity_index|
|:---------------|---------------:|
|Sempadan_sungai |        2.527933|
|Hutan_balok     |        2.360110|
|Konservasi_KBKT |        2.418851|

```r
make.sorted.plot(heterogeneity@index)
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

*The Shannon-Wiener index (Shannon 1948) assumes that individuals are randomly sampled from an infinite population and that all taxa are represented in the sample (it does not reflect the sample size). The main source of error arises from the failure to include all taxa in the sample: this error increases as the proportion of species discovered in the sample declines (Peet 1974; Magurran 1988). The maximum likelihood estimator (MLE) is used for the relative abundance, this is known to be negatively biased by sample size.*

> Note that `berger`, `mcintosh` and `simpson` methods return a dominance index, not the reciprocal form usually adopted, so that an increase in the value of the index accompanies a decrease in diversity.

Corresponding evenness can also be computed :

### b. Evenness Index


```r
evenness_index <- df %>% as_count() %>% index_evenness(method = "shannon")
kable(evenness_index@index,
      caption = "Evenness index Shannon-Wiener Method",
      col.names = "evenness_index")
```



Table: Evenness index Shannon-Wiener Method

|                | evenness_index|
|:---------------|--------------:|
|Sempadan_sungai |      0.8303217|
|Hutan_balok     |      0.8715162|
|Konservasi_KBKT |      0.7825358|

```r
make.sorted.plot(evenness_index@index)
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## B. Beta -diversity

Beta diversity is a comparison of diversity between ecosystems, usually measured as the amount of species change between the ecosystem.

### 1. Similarity

Jaccard, Morisita-Horn and Sorenson indices provide a scale of similarity from 0-1 where 1 is perfect similarity and 0 is no similarity. The Brainerd-Robinson index is scaled between 0 and 200.

### a. Brainerd-Robinson (similarity between assemblages)

- **Quantitative** similarity measures (between samples)
- Brainerd-Robinson quantitative index. This is a city-block metric of similarity between pairs of samples/cases.


```r
brainerd <- df %>% as_count %>% similarity(method = "brainerd")
brainerddata <-sim2dist(brainerd@.Data, maxSim = 200)
plot(
    hclust(brainerddata),
    hang = -1,
    main = "Sites clustered by Brainerd similarity",
    xlab = "Quantitative Index",
    axes = FALSE, ylab = ""
    )
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



### b. Jaccard index (similarity between assemblages)

- **Qualitative** similarity measures (between samples)
-  This analysis includes presence/absence standardization using `decostand`


```r
jaccard <- df %>% as_count %>% similarity(method = "jaccard")
jaccarddata <- sim2dist(jaccard@.Data, maxSim = 1)
plot(
    hclust(jaccarddata),
    hang = -1,
    main = "Sites clustered by Jaccard similarity",
    xlab = "includes presence/absence standardization using `decostand`",
    sub = "Qualitative index",
    axes = FALSE, ylab = ""
    )
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



### c. Binomial co-occurrence (similarity between types)

Binomial co-occurrence assessment. This assesses the degree of co-occurrence between taxa/types within a dataset. The strongest associations are shown by large positive numbers, the strongest segregations by large negative numbers.

- Similarity between pairs of taxa/types can be measured by assessing the degree of co-occurrence (binomial co-occurrence).
- The Binomial co-occurrence assessment approximates a Z-score.


```r
sim.types <- df %>% as_count %>% similarity(method = "binomial") %>%
            plot_spot() + khroma::scale_colour_PRGn() +
            theme(axis.text = element_text(size = 5, face = "bold"))
print(sim.types)
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## C. Abundance Model

Ranks vs abundance plot can be used for abundance models


```r
abund.model <- df %>% as_count() %>% 
            plot_rank(log = "xy", facet = FALSE) +
            ggplot2::theme_bw() + khroma::scale_color_discreterainbow()
print(abund.model)
```

![](Diversity_measures_SMM_2015_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


