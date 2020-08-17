library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(openxlsx)

# Load dataset
subjek <- "SMM" # use when desired a one variable line only
range_x <- "Jan - Jun 2020"
data <- read.xlsx("data_jml_spesies_ANJ.xlsx", sheet = "grafik_kekayaan", detectDates = TRUE)
data <- 
    #data[!(data$sites!=subjek),]
    data[!(data$sites == "ANJAP"| data$sites=="PMP"|data$sites=="PPM"),]
    #data[!(data$sites != "ANJAP"& data$sites!= "PMP"& data$sites!= "PPM"),]
attach(data)

# plot

#  Index
ggplot(data, aes(x=date, y=richness_index, group = sites, color = sites )) +
    #geom_line(stat = "smooth", size =1) + #change stat to "identity" for straight line
    
    #polynomial regression, adjust the poly degree to get a desired line of best fit.
    stat_smooth(method = "lm", formula = y~poly(x, 2), size = 1.2, se = F)+ 
    geom_point()+
    geom_hline(yintercept=3.5, color="red", size=.5) +
    geom_hline(yintercept = 5, color = "orange", size = .5)+
    geom_text(aes(label = round(richness_index,2)), size = 3, 
              position = position_dodge(width = 0), vjust = -0.5, hjust = 0) +
    theme_bw() + 
    labs(title = paste("Grafik Indeks Kekayaan", subjek, range_x), 
         subtitle = "Method: Margalef")+
    annotate(geom="text", x= as.Date("2020-01-31", "%Y-%m-%d"), y=3.4, 
             label="3.5 = Batas indeks kekayaan kategori rendah",size = 3)+
    annotate(geom="text", x = as.Date("2020-01-31", "%Y-%m-%d"), y=5.1, 
             label="5 = Batas indeks kekayaan kategori tinggi", size = 3) +
    theme(axis.text = element_text(face = "bold"))


    