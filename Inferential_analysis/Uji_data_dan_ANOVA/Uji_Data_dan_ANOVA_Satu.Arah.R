#1 Package: ggplot2, car,FSA, agricolae, dan Rmisc
library(ggplot2)
library(agricolae)
library(car)
library(FSA)
library(Rmisc)
library(nortest)
#2 setting up WD
direktori_uji.data <- function(nama_pengamatan) {
    dir.create(file.path("Uji keabsahan data",nama_pengamatan),recursive = T)
    setwd(paste0("Uji keabsahan data","/",nama_pengamatan))
}
#3 Pindahkan data ke WD

#4 Uji data
uji_data <- function(dat,satuan) {
    df <- read.delim(dat)
    dependent.var <- df[,2]
    independent.var <- df[,1]
    kol1 <- colnames(df)[1]
    kol2 <- colnames(df)[2]
    #Statistika deskriptif
    df_summary <- data.frame(Summarize(dependent.var ~ independent.var, data = df, digits = 1))
    colnames(df_summary)[1] <- colnames(df)[1]
    write.csv(df_summary,"Descriptive Analysis.csv")
    ## Uji Levene's pemeriksaan homogenitas ragam
    ## jika p > 0.05, asumsi kehomogenan terpenuhi
    levene <- suppressWarnings(leveneTest(dependent.var ~ independent.var, data = df))
    sink("Levene Test.txt")
    print(levene)
    sink()
    ## Normalitas :Uji shapiro (n < 50)
    ## jika p > 0.05, asumsi normalitas data terpenuhi
    normalitas <- shapiro.test(dependent.var)
    sink("Shapiro-Wilk normality test.txt")
    print(normalitas)
    sink()
    ## Normalitas: Kolmogorov-Smirnov test (n >= 50)
    ## jika p > 0.05, asumsi normalitas data terpenuhi
    Uji_ks <- suppressWarnings(ks.test(dependent.var,"pnorm",mean = mean(dependent.var), sd = sd(dependent.var)))
    sink("Kolmogorov-Smirnov test.txt")
    print(Uji_ks)
    sink()
    ## Normalitas: Lillie Test (n >=50)
    lillie <- lillie.test(dependent.var)
    sink("Lilliefor test.txt")
    print(lillie)
    sink()
    ## qqPlot: (normalitas) utk menyusun Q-Q plot untuk mengetahui apakah data memenuhi asumsi atau tidak
    ## Jika titik data berada di antara garis putus-putus, data memenuhi asumsi
    png("qqPlot.png",width = 800, height = 500)
    qqPlot(dependent.var, id = F, ylab = kol2, main = "Quantile - Quantile Plot")
    dev.off()
    ## Box and Whisker plot
    png("Box and Whisker.png", width = 1150, height = 650)
    boxplot(dependent.var ~ independent.var, data = df, 
            xlab = paste(kol2,satuan), ylab = "",cex.axis = 1.5, cex.lab = 2, 
            par(mar = c(5,16,5,3)+0.1), col = "orange", border = "brown", horizontal = TRUE, las = 1)
    dev.off()
    print(list(independent.var = kol1,dependent.var = kol2, 
        levene_test = levene, Shapiro_test = normalitas, Uji_ks = Uji_ks, Lilliefors_test = lillie ))
    print("Analisis berhasil")
}
#5 Set back the WD
balik <- setwd("C:/Users/rbbutar/Documents/R/Konservasi_ANJ")

#6 setting up WD for ANOVA
direktori_anova <- function(nama_pengamatan) {
    dir.create(file.path("Anova Satu Arah",nama_pengamatan),recursive = T)
    setwd(paste0("Anova Satu Arah","/",nama_pengamatan))
}
#7 pindah data ke WD utk anova

#8 Uji ANOVA
anova_one.way <- function(dat,satuan) {
    #ANOVA
    df1 <- read.delim(dat)
    df2 <- read.delim(dat)
    nama_kolom <- colnames(df1)
    kolom1 <- nama_kolom[1]
    kolom2 <- nama_kolom[2]
    colnames(df2) <- c("independent","dependent")
    ## lm: penyusunan linear model dan ANOVA
    model1 <- lm(dependent ~ independent, data = df2)
    hasil_anova <- anova(model1)
    rownames(hasil_anova)[1] <- kolom1
    sink("Hasil Anova Satu Arah.txt")
    print(hasil_anova)
    sink()
    ## Summary test t value
    summary_t <- summary(model1)
    sink("Summary t.txt")
    print(summary_t)
    sink()
    # Post Hoc : Tukey
    ## Kalo berbeda/berpengaruh nyata maka dilakukan uji lanjut (kali ini Post Hoc tukey)
    Test_Tukey <- HSD.test(model1,"independent", unbalanced = F)
    colnames(Test_Tukey$groups)[1] <- kolom2
    colnames(Test_Tukey$means)[1] <- kolom2
    sink("Hasil Uji Lanjut Tukey.txt")
    print(Test_Tukey)
    sink()
    notasi <- Test_Tukey$groups[order(Test_Tukey$groups[,1]),]
    notasi <- notasi[,2]
    # Grafik
    ## summarySE: kalkulasi nilai mean dan standard error (SE)
    data1 <- summarySE(data = df2,"dependent",groupvars = "independent",conf.interval = 0.95)
    data2 <- summarySE(data = df2,"dependent",groupvars = "independent",conf.interval = 0.95)
    data1 <- data1[order(data1[,3]),]
    data2 <- data2[order(data1[,3]),]
    colnames(data2)[c(1,3)] <- c(kolom1,kolom2)
    write.csv(data2,"summarySE Mean dan std error.csv")
    ##offset.v:kode instruksi untuk pengaturan posisi vertikal huruf hasil uji lanjut di atas nilai SE
    offset.v = -0.8
    ##offset.h:kode instruksi untuk pengaturan posisi horizontal huruf hasil uji lanjut di atas nilai SE
    offset.h = -0.8
    #ggplot: kode instruksi untuk penyusunan grafik
    #geom_bar: kode untuk pengaturan diagram batang)
    #geom_errorbar: kode untuk pembubuhan SE di diagram batang)
    #geom_text: kode untuk pembubuhan huruf, kata, frase atau kalimat di grafik)
    #labs: kode untuk pengaturan nama judul sumbu x dan y)
    #them_bw: kode pengaturan grafik hitam putih
    #theme: kode pengaturan latar grafik, ukuran huruf, tipe huruf, dll)
    grafik <- ggplot(data1,
            aes(x= reorder(independent,dependent), y= dependent),ymax=8,ymin=0.0)+
            geom_bar(stat="identity", fill="white",colour="black",width = 0.6)+
            geom_errorbar(aes(ymin=dependent-se, ymax=dependent+se),width=0.1,size=0.6,colour="black") +
            geom_text(aes(label= notasi,hjust=offset.h, vjust=offset.v),size=6) +
            geom_text(aes(label=dependent),position = position_dodge(width = 0.4),vjust = -0.25, hjust = 1.1,size = 6)+
            labs(x = kolom1, y = paste(kolom2,satuan), title = paste("Pengaruh",kolom1, "terhadap", kolom2), subtitle = "Analysis of Variance \nPost Hoc Analysis: Tukey's Test") +
            theme_bw() +
            theme(panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                plot.title = element_text(size = rel(2),face = "bold",hjust=0),
                plot.subtitle = element_text(size = rel(1.5), hjust = 0),
                axis.title = element_text(face="bold",size = 18),
                axis.title.y = element_text(vjust = 1.8,size = 18),
                axis.title.x = element_text(vjust=-0.5, size=18),
                axis.line = element_line(colour="black"),
                axis.text = element_text(size = 14,color = "black"))
    png("ggplot anova one way.png",width = 970, height = 580)
    print(grafik)
    dev.off()
    print(list(independent.var = kolom1, dependent.var = kolom2, ANOVA_satu_arah = hasil_anova, summary_t = summary_t, post_hoc_Tukey = Test_Tukey ,summary_MEAN_SE=data2 ))
    print("Analisis Berhasil")
}

#9 set back the WD
balik <- setwd("C:/Users/rbbutar/Documents/R/Konservasi_ANJ")
