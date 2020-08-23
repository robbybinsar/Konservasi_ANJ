# Load packages
library(ggplot2)
library(khroma)
library(tabula)
library(magrittr)
library(RFLPtools)
library(openxlsx)

# Reading and preparing data
dat <- "data_jml_spesies_ANJ.xlsx"
nama_sheet <- "January"
df <- read.xlsx(dat, sheet = nama_sheet, cols = 1:8)
rownames(df) <- df[,1]
df <- df[,-1]
df <- t(df)

#Dropping columns sum 0
df <- as.data.frame(df)
df <- df[,!sapply(df, function(x) sum(x))==0]

# Beta diversity

# Similarity
# Brainerd-Robinson quantitative index (similarity between assemblages)
brainerd <- df %>% as_count %>% similarity(method = "brainerd")
brainerd %>% plot_spot() + khroma::scale_colour_YlOrBr() + 
    theme(axis.text = element_text(size = 8, face = "bold"))
brainerddata <-sim2dist(brainerd@.Data, maxSim = 200)
plot(
    hclust(brainerddata),
    hang = -1,
    main = paste("Sites clustered by Brainerd similarity","PT ANJ", nama_sheet),
    xlab = "Quantitative Index",
    axes = FALSE, ylab = ""
)
# Jaccard qualitative index (similarity between assemblages)
jaccard <- df %>% as_count %>% similarity(method = "jaccard")
jaccard %>% plot_spot() + khroma::scale_colour_BuRd() + theme(axis.text = element_text(size = 12, face = "bold"))
jaccarddata <- sim2dist(jaccard@.Data, maxSim = 1)
plot(
    hclust(jaccarddata),
    hang = -1,
    main = paste("Sites clustered by Jaccard similarity","PT ANJ", nama_sheet),
    xlab = "includes presence/absence standardization using `decostand`",
    sub = "Qualitative index",
    axes = FALSE, ylab = ""
)
# Binomial co-occurrence (similarity between types)
sim.types <- df %>% as_count %>% similarity(method = "binomial") %>%
    plot_spot() + khroma::scale_colour_PRGn() + 
    labs(title = paste("Similarity between types", "PT ANJ", nama_sheet))+
    theme(axis.text = element_text(size = 1, face = "bold"))
print(sim.types)

# Abundance models
# rank vs relative abundance
abund.model <- df %>% as_count() %>% 
    plot_rank(log = "xy", facet = FALSE) +
    labs(title = paste("Abundance Model","PT ANJ",nama_sheet))+
    ggplot2::theme_bw() + khroma::scale_color_discreterainbow()
print(abund.model)
