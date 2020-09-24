# Load packages
library(tabula)
library(magrittr)
library(openxlsx)
library(dplyr)

# dat = file directory contains of individuals count data_jml_spesies_ANJ.xlsx
# nama_sheet = bulan
alpha_diversity <- function(dat, nama_sheet) {
# Reading and preparing data
df <- read.xlsx(dat, sheet = nama_sheet, cols = 1:8)
rownames(df) <- df[,1]
df <- df[,-1]
df <- t(df)

#Dropping columns sum 0
df <- as.data.frame(df)
df <- df[,!sapply(df, function(x) sum(x))==0]

# alfa diversity
    # species number
        spec.numb <- data.frame(matrix(nrow = 0, ncol = 2))
        colnames(spec.numb) <- c("site","species.number")
        for(i in seq_len(nrow(df))) {
            template <- data.frame(site = rownames(df)[i], species.number = sum(df[i,]!=0))
            spec.numb <- rbind(spec.numb,template)
        }
    # Richness and Rarefaction
        # Richness index
        richness_index <- df %>% as_count() %>% index_richness(method = "margalef")
        indexr <- as.data.frame(richness_index@index)
        rich_ind_df <- data.frame(row.names = row.names(indexr), size = richness_index@size, richnessIndex = indexr[,1])
        # commposition index
        composition_ind <- df %>% as_count() %>% index_composition(method = "chao1")
        indexc <- as.data.frame(composition_ind@index)
        comp_ind_df <- data.frame(row.names = row.names(indexc), size = composition_ind@size, richnessIndex = indexc[,1])
        colnames(comp_ind_df)[2] <- "composition_index"
        # rarefaction
        rarefact <- df %>% as_count() %>% rarefaction(sample = 10, method = "hurlbert", simplify = T) %>%
            as.data.frame()
        colnames(rarefact)[1] <- "hulbert_rarefaction"
    # Heterogeneity and Evenness
        #Diversity index
        heterogeneity <- df %>% as_count() %>% index_heterogeneity(method = "shannon")
        indexH <- as.data.frame(heterogeneity@index)
        H_index_df <- data.frame(row.names = row.names(indexH), 
                                 size = heterogeneity@size, diversity_index = indexH[,1])
        # Evenness index
        even_index <- df %>% as_count() %>% index_evenness(method = "shannon")
        IndexE <- as.data.frame(even_index@index)
        E_index_df <- data.frame(row.names = row.names(IndexE), 
                                 size = even_index@size, Evennes_index = IndexE[,1])
    # Output alfa divevrsity
    output <- bind_cols(spec.numb,rich_ind_df,comp_ind_df,rarefact,H_index_df,E_index_df)
    drop <- c(5,8,10)
    output <- output[,-drop]
    wb <- loadWorkbook(dat)
    writeData(wb,sheet = nama_sheet ,output, startCol = 11)
    saveWorkbook(wb,dat,overwrite = T)
}
    


        
        
        