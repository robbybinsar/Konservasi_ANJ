library(vegan)

# reading data
df <- read.csv("bird_contoh.csv")
rownames(df) <- df[,1]
df <- df[,-1]
df <- t(df)

# species richness
spec.rich <- specnumber(df, MARGIN = 1)

# diversity indices
div.ind <- diversity(df, index = "shannon", MARGIN = 1)

#plot
make.sorted.plot <- function(x){
    ordered <- sort(x, T)
    plot(
        ordered,
        col = terrain.colors(10),
        xaxt = "n", pch = 16, cex = 2,
        ylim = c(min(ordered)*0.5, max(ordered)),
        xlim = c(0, length(x)+1),
        ylab = "Diversity measure", xlab = "Sites",
        main = substitute(x))
    text(ordered,
         names(ordered),
         srt = -75,
         pos = 4)
}

make.sorted.plot(div.ind)
make.sorted.plot(spec.rich)

# Dissimilarity
dissimilarity <- df %>% as_count() %>% vegdist(method = "jaccard", binary = T)

# Sites dendogram
plot(
    hclust(dissimilarity),
    hang = -1,
    main = "Sites clustered by Jaccard similarity",
    axes = FALSE, ylab = ""
    )
