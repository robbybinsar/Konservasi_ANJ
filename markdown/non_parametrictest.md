---
title: "Non-Parametric Statistical Test"
author: "Robby"
date: "7/31/2020"
output: 
    html_document:
        keep_md: TRUE
        toc: TRUE
---



This test is used when normality assumption of a dataset is not achieved.


```r
library(rcompanion)
library(multcompView)
library(DescTools)
library(FSA)
```

### **Reading and preparing data**

```r
df <- read.delim("Sengon jml petiqlule.txt")
kable(df, format = "pipe")
```



|Media                 | Jumlah.petiqlule|
|:---------------------|----------------:|
|100% Tanah            |               94|
|100% Tanah            |               78|
|100% Tanah            |               82|
|100% Tanah            |               88|
|100% Tanah            |               58|
|100% Tanah            |               74|
|100% Tanah            |               84|
|100% Tanah            |               80|
|100% Tanah            |               76|
|100% Tanah            |               96|
|50% Tanah 50% Kompos  |               90|
|50% Tanah 50% Kompos  |               66|
|50% Tanah 50% Kompos  |               86|
|50% Tanah 50% Kompos  |               90|
|50% Tanah 50% Kompos  |               80|
|50% Tanah 50% Kompos  |              126|
|50% Tanah 50% Kompos  |               92|
|50% Tanah 50% Kompos  |              102|
|50% Tanah 50% Kompos  |               78|
|50% Tanah 50% Kompos  |               90|
|100% Kompos           |               20|
|100% Kompos           |               22|
|100% Kompos           |               18|
|100% Kompos           |               24|
|100% Kompos           |               18|
|100% Kompos           |               16|
|100% Kompos           |               88|
|100% Kompos           |               78|
|100% Kompos           |               72|
|100% Kompos           |               78|
|50% Tanah 50% Jangkos |               78|
|50% Tanah 50% Jangkos |               84|
|50% Tanah 50% Jangkos |               76|
|50% Tanah 50% Jangkos |               68|
|50% Tanah 50% Jangkos |               82|
|50% Tanah 50% Jangkos |               78|
|50% Tanah 50% Jangkos |               72|
|50% Tanah 50% Jangkos |              102|
|50% Tanah 50% Jangkos |               92|
|50% Tanah 50% Jangkos |               74|
|100% Jangkos          |               64|
|100% Jangkos          |               74|
|100% Jangkos          |               80|
|100% Jangkos          |               86|
|100% Jangkos          |               74|
|100% Jangkos          |               86|
|100% Jangkos          |               70|
|100% Jangkos          |                0|
|100% Jangkos          |               76|
|100% Jangkos          |               74|


```r
attach(df)
```

### **Kruskal-Wallis Rank Sum Test**

```r
kruskalw <- kruskal.test(Jumlah.petiqlule ~ Media)
print(kruskalw)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  Jumlah.petiqlule by Media
## Kruskal-Wallis chi-squared = 16.37, df = 4, p-value = 0.002561
```

*Note: If Kruskal-Wallis test result is significant (p < 0.05) conitnue with post hoc test.

### **Post Hoc Tests**
*Test for several post hoc methods for comparison*

#### Pairwise Wilcoxon Rank Sum Tests

```r
wilcox <- pairwise.wilcox.test(Jumlah.petiqlule,Media,
                               p.adjust.method = "BH", exact = FALSE)
#p.adjust.method: Returns p-values adjusted using Benjamini & Hochberg (1995) method ("BH")
wilcox1 <- wilcox$p.value
wilcox2 <- fullPTable(wilcox1)
wilcox3 <- multcompLetters(wilcox2,compare = "<", threshold = 0.05, 
                           Letters = letters,reversed = FALSE)
print(list(wilcox_pairwise = wilcox, wilcox3))
```

```
## $wilcox_pairwise
## 
## 	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
## 
## data:  Jumlah.petiqlule and Media 
## 
##                       100% Jangkos 100% Kompos 100% Tanah 50% Tanah 50% Jangkos
## 100% Kompos           0.264        -           -          -                    
## 100% Tanah            0.189        0.033       -          -                    
## 50% Tanah 50% Jangkos 0.266        0.057       0.622      -                    
## 50% Tanah 50% Kompos  0.033        0.016       0.264      0.200                
## 
## P value adjustment method: BH 
## 
## [[2]]
##          100% Jangkos           100% Kompos            100% Tanah 
##                  "ab"                   "a"                  "bc" 
## 50% Tanah 50% Jangkos  50% Tanah 50% Kompos 
##                 "abc"                   "c"
```


#### Dunn's Kruskal-Wallis Multiple Comparisons.

```r
Dunn <- dunnTest(Jumlah.petiqlule ~ Media, method = "bh")
```

```
## Warning: Media was coerced to a factor.
```

```r
Dunn1 <- Dunn$res
Dunn2 <- cldList(comparison = Dunn1$Comparison, p.value = Dunn1$P.adj, threshold = 0.05)
print(list(Dunn = Dunn, Dunn2))
```

```
## $Dunn
```

```
## Dunn (1964) Kruskal-Wallis multiple comparison
```

```
##   p-values adjusted with the Benjamini-Hochberg method.
```

```
##                                      Comparison         Z      P.unadj
## 1                    100% Jangkos - 100% Kompos  1.029663 0.3031681386
## 2                     100% Jangkos - 100% Tanah -1.582915 0.1134408255
## 3                      100% Kompos - 100% Tanah -2.612578 0.0089862117
## 4          100% Jangkos - 50% Tanah 50% Jangkos -1.160292 0.2459298967
## 5           100% Kompos - 50% Tanah 50% Jangkos -2.189955 0.0285274786
## 6            100% Tanah - 50% Tanah 50% Jangkos  0.422623 0.6725703772
## 7           100% Jangkos - 50% Tanah 50% Kompos -2.666367 0.0076676023
## 8            100% Kompos - 50% Tanah 50% Kompos -3.696030 0.0002189973
## 9             100% Tanah - 50% Tanah 50% Kompos -1.083452 0.2786080358
## 10 50% Tanah 50% Jangkos - 50% Tanah 50% Kompos -1.506075 0.1320480437
##          P.adj
## 1  0.336853487
## 2  0.226881651
## 3  0.029954039
## 4  0.351328424
## 5  0.071318697
## 6  0.672570377
## 7  0.038338011
## 8  0.002189973
## 9  0.348260045
## 10 0.220080073
## 
## [[2]]
##              Group Letter MonoLetter
## 1        1%Jangkos     ab        ab 
## 2         1%Kompos      a        a  
## 3          1%Tanah     bc         bc
## 4 5%Tanah5%Jangkos    abc        abc
## 5  5%Tanah5%Kompos      c          c
```

#### Nemenyi Test

```r
Media <- as.factor(Media)
nemenyi <- NemenyiTest(x = Jumlah.petiqlule, g = Media, dist = "tukey")
print(nemenyi)
```

```
## 
##  Nemenyi's test of multiple comparisons for independent samples (tukey)  
## 
##                                            mean.rank.diff   pval    
## 100% Kompos-100% Jangkos                            -6.70 0.8426    
## 100% Tanah-100% Jangkos                             10.30 0.5103    
## 50% Tanah 50% Jangkos-100% Jangkos                   7.55 0.7752    
## 50% Tanah 50% Kompos-100% Jangkos                   17.35 0.0598 .  
## 100% Tanah-100% Kompos                              17.00 0.0689 .  
## 50% Tanah 50% Jangkos-100% Kompos                   14.25 0.1850    
## 50% Tanah 50% Kompos-100% Kompos                    24.05 0.0021 ** 
## 50% Tanah 50% Jangkos-100% Tanah                    -2.75 0.9934    
## 50% Tanah 50% Kompos-100% Tanah                      7.05 0.8162    
## 50% Tanah 50% Kompos-50% Tanah 50% Jangkos           9.80 0.5603    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

**An error from cldList() indicate that there's no significant differences**

```r
try(cldList(comparison = nemenyi$Comparison, p.value = nemenyi$P.adj, threshold = 0.05))
```

```
## Error : No significant differences.
```

