library(rcompanion)
library(multcompView)
library(DescTools)

direktori_nonpara <- function(nama_pengamatan) {
    dir.create(file.path("Uji data non parametrik",nama_pengamatan),recursive = T)
    setwd(paste0("Uji data non parametrik","/",nama_pengamatan))
}

# uji non parametrik tidak memerlukan data yanng berdistribusi normal, oleh karena itu hasil dr non parametrik biasanya kurang powerful dibanding parametrik
non_parametrik <- function(dat) {
    df1 <- read.delim(dat)
    df2 <- read.delim(dat)
    nama_kolom <- colnames(df1)
    kolom1 <- nama_kolom[1]
    kolom2 <- nama_kolom[2]
    colnames(df2) <- c("independent","dependent")
    ## Kruskall wallis test subtitute for ANOVA in non parametric data (because normality is not achievable)
    kruskalw <- kruskal.test(dependent ~ independent, data = df2)
    sink("Kruskal-wallis test.txt")
    print(kruskalw)
    sink()
    print(list(Kruskalw = kruskalw))
    ## post hoc: Pairwise Wilcoxon Rank Sum Tests
    wilcox <- pairwise.wilcox.test(df2$dependent,df2$independent,p.adjust.method = "BH", exact = FALSE)
    wilcox1 <- wilcox$p.value
    wilcox2 <- fullPTable(wilcox1)
    wilcox3 <- multcompLetters(wilcox2,compare = "<", threshold = 0.05, Letters = letters,reversed = FALSE)
    print(list(wilcox_pairwise = wilcox, wilcox3))
    sink("wilcox pairwise.txt")
    print(wilcox)
    print(wilcox3)
    sink()
    ## post hoc : Dunn Test for multiple comparisons
    Dunn <- dunnTest(dependent ~ independent, data = df2, method = "bh")
    Dunn1 <- Dunn$res
    Dunn2 <- cldList(comparison = Dunn1$Comparison, p.value = Dunn1$P.adj, threshold = 0.05)
    print(list(Dunn = Dunn, Dunn2))
    sink("Dunn test.txt")
    print(Dunn)
    print(Dunn2)
    sink()
    ## post hoc: Nemeyi test for multiple comparisons
    df2$independent <- as.factor(df2$independent)
    nemenyi <- NemenyiTest(x = df2$dependent, g = df2$independent, dist = "tukey")
    print(list(nemenyi = nemenyi))
    sink("Nemenyi test.txt")
    print(nemenyi)
    sink()
    nemenyi1 <- cldList(comparison = nemenyi$Comparison, p.value = nemenyi$P.adj, threshold = 0.05)
    print(list(nemenyi1 = nemenyi1))
}
