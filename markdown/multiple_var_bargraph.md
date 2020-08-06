---
title: "Multiple Variable Bar Graph"
author: "Robby"
date: "8/1/2020"
output: 
    html_document:
        keep_md: TRUE
---




```r
library(tidyr)
library(Rmisc)
library(plyr)
library(ggplot2)
```


### **Reading and Preparing data**

```r
df <- read.delim("Trembesi_summary.txt")
head(df)
```

```
##        Media Tinggi.tanaman Jumlah.petiole Jumlah.petiqlule
## 1 100% Tanah           22.3              8               68
## 2 100% Tanah            0.0              0                0
## 3 100% Tanah            0.0              0                0
## 4 100% Tanah            0.0              0                0
## 5 100% Tanah           28.0             12              100
## 6 100% Tanah           23.0              9               90
```

```r
attach(df)
```

**Gather the data to 'long' format so the variables are all in one column**

```r
longdata <- pivot_longer(df,c(Tinggi.tanaman, Jumlah.petiole, Jumlah.petiqlule), names_to = "variable")
head(longdata)
```

```
## # A tibble: 6 x 3
##   Media      variable         value
##   <chr>      <chr>            <dbl>
## 1 100% Tanah Tinggi.tanaman    22.3
## 2 100% Tanah Jumlah.petiole     8  
## 3 100% Tanah Jumlah.petiqlule  68  
## 4 100% Tanah Tinggi.tanaman     0  
## 5 100% Tanah Jumlah.petiole     0  
## 6 100% Tanah Jumlah.petiqlule   0
```

### **Create summary statistics**

```r
summary <- longdata %>% summarySE(measurevar = "value", groupvars = c("Media","variable"))
print(summary)
```

```
##                    Media         variable  N  value        sd         se        ci
## 1           100% Jangkos   Jumlah.petiole 10   5.70  3.973523  1.2565385  2.842487
## 2           100% Jangkos Jumlah.petiqlule 10  61.60 43.244011 13.6749568 30.934902
## 3           100% Jangkos   Tinggi.tanaman 10  14.53 10.110286  3.1971532  7.232463
## 4            100% Kompos   Jumlah.petiole 10   2.60  2.366432  0.7483315  1.692843
## 5            100% Kompos Jumlah.petiqlule 10  21.20 21.399896  6.7672414 15.308564
## 6            100% Kompos   Tinggi.tanaman 10   8.23  7.251827  2.2932292  5.187645
## 7             100% Tanah   Jumlah.petiole 10   6.60  4.695151  1.4847372  3.358709
## 8             100% Tanah Jumlah.petiqlule 10  61.20 43.330256 13.7022302 30.996598
## 9             100% Tanah   Tinggi.tanaman 10  17.10 12.174290  3.8498485  8.708962
## 10 50% Tanah 50% Jangkos   Jumlah.petiole 10  11.90  9.480389  2.9979623  6.781862
## 11 50% Tanah 50% Jangkos Jumlah.petiqlule 10 112.70 72.366429 22.8842741 51.767825
## 12 50% Tanah 50% Jangkos   Tinggi.tanaman 10  23.13  9.448933  2.9880149  6.759359
## 13  50% Tanah 50% Kompos   Jumlah.petiole 10   7.70  5.578729  1.7641491  3.990783
## 14  50% Tanah 50% Kompos Jumlah.petiqlule 10  69.80 52.615587 16.6385095 37.638924
## 15  50% Tanah 50% Kompos   Tinggi.tanaman 10  21.63 15.295319  4.8368045 10.941612
```

### **Create relative frequency**
*note: .(variable) for quoting*

```r
dat <- ddply(summary,.(variable),transform, rel_freq = round(value/sum(value),2),
             se_freq = se/value*(value/sum(value)))
dat <- dat[order(dat$variable,-dat$value),]
dat["notasi"] <- c("a","ab","ab","ab","b","a","ab","ab","ab","b","a","ab","ab","ab","b") #notation from post hoc test
rownames(dat) <- seq_len(nrow(dat))
print(dat)
```

```
##                    Media         variable  N  value        sd         se        ci rel_freq
## 1  50% Tanah 50% Jangkos   Jumlah.petiole 10  11.90  9.480389  2.9979623  6.781862     0.34
## 2   50% Tanah 50% Kompos   Jumlah.petiole 10   7.70  5.578729  1.7641491  3.990783     0.22
## 3             100% Tanah   Jumlah.petiole 10   6.60  4.695151  1.4847372  3.358709     0.19
## 4           100% Jangkos   Jumlah.petiole 10   5.70  3.973523  1.2565385  2.842487     0.17
## 5            100% Kompos   Jumlah.petiole 10   2.60  2.366432  0.7483315  1.692843     0.08
## 6  50% Tanah 50% Jangkos Jumlah.petiqlule 10 112.70 72.366429 22.8842741 51.767825     0.35
## 7   50% Tanah 50% Kompos Jumlah.petiqlule 10  69.80 52.615587 16.6385095 37.638924     0.21
## 8           100% Jangkos Jumlah.petiqlule 10  61.60 43.244011 13.6749568 30.934902     0.19
## 9             100% Tanah Jumlah.petiqlule 10  61.20 43.330256 13.7022302 30.996598     0.19
## 10           100% Kompos Jumlah.petiqlule 10  21.20 21.399896  6.7672414 15.308564     0.06
## 11 50% Tanah 50% Jangkos   Tinggi.tanaman 10  23.13  9.448933  2.9880149  6.759359     0.27
## 12  50% Tanah 50% Kompos   Tinggi.tanaman 10  21.63 15.295319  4.8368045 10.941612     0.26
## 13            100% Tanah   Tinggi.tanaman 10  17.10 12.174290  3.8498485  8.708962     0.20
## 14          100% Jangkos   Tinggi.tanaman 10  14.53 10.110286  3.1971532  7.232463     0.17
## 15           100% Kompos   Tinggi.tanaman 10   8.23  7.251827  2.2932292  5.187645     0.10
##       se_freq notasi
## 1  0.08689746      a
## 2  0.05113476     ab
## 3  0.04303586     ab
## 4  0.03642140     ab
## 5  0.02169077      b
## 6  0.07008966      a
## 7  0.05096021     ab
## 8  0.04188348     ab
## 9  0.04196701     ab
## 10 0.02072662      b
## 11 0.03531098      a
## 12 0.05715912     ab
## 13 0.04549573     ab
## 14 0.03778248     ab
## 15 0.02710032      b
```

### **Sorting x axis variables fill**

```r
dat$variable <- factor(dat$variable, levels = c("Tinggi.tanaman", "Jumlah.petiole", "Jumlah.petiqlule"))
```

### **Grafik ggplot**

```r
kolom1 <- colnames(df)[1]
grafik <- ggplot(dat, aes(x = reorder(Media,-rel_freq), y = rel_freq, fill = variable)) + 
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.9) +
    geom_errorbar(aes(ymin = rel_freq - se_freq, ymax = rel_freq + se_freq), 
                  width = 0.2,position = position_dodge(0.9))+
    scale_fill_manual("Variable", values = c("dark blue","orange","yellow"))+ 
    geom_text(aes(label =rel_freq), size = 2.5, position = position_dodge(width = 0.9), 
              vjust = -0.5, hjust = -0.1)+
    geom_text(aes(label= notasi),hjust=1.4, vjust=3,size=3, 
              position = position_dodge(1), color = "red")+
    labs(x = kolom1, y = "Frekuensi Relatif Rata-Rata", title = paste("Pengaruh",kolom1, "terhadap", "Tanaman Trembesi"))+
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = rel(1),face = "bold",hjust=0),
          axis.title = element_text(face="bold"),
          axis.title.y = element_text(vjust = 3,size = 8),
          axis.title.x = element_text(vjust=-1, size=8),
          axis.line = element_line(colour="black"),
          axis.text = element_text(size = 6,color = "black", face = "bold"),
          legend.text = element_text(size=6),
          legend.title = element_text(size = 8))
print(grafik)
```

![](multiple_var_bargraph_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
