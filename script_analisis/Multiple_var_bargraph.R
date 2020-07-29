library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(scales)

# Preparing data
df <- read.delim("Trembesi_summary.txt")

## Gather the data to 'long' format so the bodytemp, length and mass are all in one column
longdata <- gather(test4, variable, value, -Sex)
longdata <- pivot_longer(df,c(Tinggi.tanaman, Jumlah.petiole, Jumlah.petiqlule), names_to = "variable") ## alternatif kedua selain pakai gather

# Create the summary statistics seperately for grouping variables (i.e. bodytemp, length and mass)
summary <- longdata %>% summarySE(measurevar = "value", groupvars = c("Media","variable"))

# buat jadi relative frequency
#.(variable) unutk quoting
dat <- ddply(summary,.(variable),transform, rel_freq = round(value/sum(value),2),se_freq = se/value*(value/sum(value)))
dat <- dat[order(dat$variable,-dat$value),]
dat["notasi"] <- c("a","ab","ab","ab","b","a","ab","ab","ab","b","a","ab","ab","ab","b")

# sort x axis fill
dat$variable <- factor(dat$variable, levels = c("Tinggi.tanaman", "Jumlah.petiole", "Jumlah.petiqlule"))

#plot
kolom1 <- colnames(df)[1]
grafik <- ggplot(dat, aes(x = reorder(Media,-rel_freq), y = rel_freq, fill = variable)) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_errorbar(aes(ymin = rel_freq - se_freq, ymax = rel_freq + se_freq),                            
                  width = 0.2,position = position_dodge(0.9))+
    scale_fill_manual("Variable", values = c("dark blue","orange","yellow"))+ 
    geom_text(aes(label =rel_freq), size = 7, position = position_dodge(width = 0.9), vjust = -0.5, hjust = -0.1)+
    geom_text(aes(label= notasi),hjust=1.5, vjust=2,size=7, 
              position = position_dodge(0.9), color = "red")+
    labs(x = kolom1, y = "Frekuensi Relatif Rata-Rata", title = paste("Pengaruh",kolom1, "terhadap", "Tanaman Trembesi"))+
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = rel(3),face = "bold",hjust=0),
          axis.title = element_text(face="bold"),
          axis.title.y = element_text(vjust = 1.8,size = 25),
          axis.title.x = element_text(vjust=-0.5, size=25),
          axis.line = element_line(colour="black"),
          axis.text = element_text(size = 17,color = "black"),
          legend.text = element_text(size=19),
          legend.title = element_text(size = 21),
          plot.margin = unit(c(1,2,1,2), "cm"))

png("summary_multiplebar.png",width = 1500, height = 800)
print(grafik)
dev.off()
print(grafik)
