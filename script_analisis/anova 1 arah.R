# Package yang diperlukan: ggplot2, car,FSA, agricolae, dan Rmisc

df_tinggi.anggrek <- read.delim("Anggrek Tanah (Tinggi).txt")

## Statistika deskriptif
df_summary.tinggi.anggrek <- data.frame(Summarize(Tinggi.Tanaman ~ Media,data=df_tinggi.anggrek,digits = 1))
write.csv(df_summary.tinggi.anggrek,"Descriptive Tinggi Anggrek.csv")

## Uji Levene's pemeriksaan homogenitas ragam
## jika p > 0.05, asumsi kehomogenan terpenuhi
levene <- leveneTest(Tinggi.Tanaman ~ Media, data = df_tinggi.anggrek)
write.csv(levene,"Levene Test.csv")

## Pemeriksaan normalitas (Uji shapiro dan pemeriksaan grafis)
## jika p > 0.05, asumsi normalitas data terpenuhi
normalitas <- data.frame(unlist(shapiro.test(df_tinggi.anggrek$Tinggi.Tanaman)))
write.csv(normalitas,"Shapiro-Wilk normality test.csv")
## qqPlot: utk menyusun Q-Q plot untuk mengetahui apakah data memenuhi asumsi atau tidak
## Jika titik data berada di antara garis putus-putus, data memenuhi asumsi
png("qqPlot.png",width = 800, height = 500)
qqPlot(df_tinggi.anggrek$Tinggi.Tanaman,id = F)
dev.off()

#ANOVA

## lm: penyusunan linear model dan ANOVA
model1 <- lm(Tinggi.Tanaman ~ Media, data = df_tinggi.anggrek)
hasil_anova <- anova(model1)
write.csv(hasil_anova,"Hasil Anova.csv")

## Summary test t value
summary_t <- summary(model1)
sink("Summary t.txt")
print(summary_t)
sink()

# Post Hoc : Tukey
## Kalo berbeda/berpengaruh nyata maka dilakukan uji lanjut (kali ini Post Hoc tukey)
Test_BNT <- HSD.test(model1,"Media",unbalanced = F)
sink("Hasil Uji Lanjut Tukey.txt")
print(Test_BNT)
sink()
notasi <- Test_BNT$groups[order(Test_BNT$groups[,1]),]
notasi <- notasi[,2]

#Grafik

## summarySE: kalkulasi nilai mean dan standard error (SE)
data1 <- summarySE(data=df_tinggi.anggrek,"Tinggi.Tanaman",groupvars = "Media",conf.interval = 0.95)
write.csv(data1,"summarySE Mean dan Std Error.csv")
##offset.v:kode instruksi untuk pengaturan posisi vertikal huruf hasil uji lanjut di atas nilai SE
offset.v = -1
##offset.h:kode instruksi untuk pengaturan posisi horizontal huruf hasil uji lanjut di atas nilai SE
offset.h = 0.5

#ggplot: kode instruksi untuk penyusunan grafik
#geom_bar: kode untuk pengaturan diagram batang)
#geom_errorbar: kode untuk pembubuhan SE di diagram batang)
#geom_text: kode untuk pembubuhan huruf, kata, frase atau kalimat di grafik)
#labs: kode untuk pengaturan nama judul sumbu x dan y)
#them_bw: kode pengaturan grafik hitam putih
#theme: kode pengaturan latar grafik, ukuran huruf, tipe huruf, dll)
png("ggplot.png",width = 800, height = 500)
ggplot(data1,
    aes(x=reorder(Media,Tinggi.Tanaman), y= Tinggi.Tanaman),ymax=8,ymin=0.0)+
    geom_bar(stat="identity", fill="white",colour="black",width = 0.6)+
    geom_errorbar(aes(ymin=Tinggi.Tanaman-se, ymax=Tinggi.Tanaman+se),width=0.1,size=0.6,colour="black") +
    geom_text(x=1, y=4,label="ANOVA, p < 0.01",size = 7) +
    geom_text(aes(label= notasi,hjust=offset.h, vjust=offset.v),nudge_x = 0.12,nudge_y = -0.1,size=6) +
    geom_text(aes(label=Tinggi.Tanaman),position = position_dodge(width = 0.4),vjust = -0.25, hjust = 1.1,size = 6)+
    labs(x = "Media", y = "Pertambahan Tinggi (cm)") +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = rel(1.5),face = "bold",vjust=1.5),
          axis.title = element_text(face="bold",size = 18),
          axis.title.y = element_text(vjust = 1.8,size = 18),
          axis.title.x = element_text(vjust=-0.5, size=18),
          axis.line = element_line(colour="black"),
          axis.text = element_text(size = 14,color = "black"))
dev.off()
    

