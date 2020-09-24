library(openxlsx)
library(stringdist)
library(dplyr)


# funtion for stringsim
# month in english
lookupf <- function(month) {
    #my look up data reference
    teladan <- read.xlsx("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/data_jml_spesies_ANJ.xlsx", 
                         sheet = month, cols =  1) #month to date ; not yet updated
    UM <- c("ANJA", "ANJAS", "SMM", "KAL", "PMP", "PPM", "ANJAP")
    for(u in UM) {
        y <- teladan$Nama.Latin
        #my data that's being looked up for
        mydata <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/ANJ PENDAKI detail 2020.xlsx",
                            sheet = u, cols = 19:31, startRow = 3) %>% select(Nama.Latin, all_of(month))
        colnames(mydata)[colnames(mydata)==month] <- u
        mylatin <- mydata[,1]
        for (i in mylatin) {
            hitung <- stringsim(i, y)
            indeks <- match(max(hitung), hitung)
            value <- y[indeks]
            if (max(hitung) != 1) {
                mydf <- data.frame(Nama.Latin = c(i), closestMatch = c(value))
                teladan <- bind_rows(teladan, mydf)
            }
        }
        teladan <- merge(teladan, mydata, by = "Nama.Latin", all.x = T)
        .GlobalEnv$teladan <- teladan
    }
    teladan[is.na(teladan)] <- 0
    .GlobalEnv$teladan <- teladan
}

#export into csv file"
save <- function(NamaFile) {
    write.csv(teladan, paste0("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/",NamaFile))
}
