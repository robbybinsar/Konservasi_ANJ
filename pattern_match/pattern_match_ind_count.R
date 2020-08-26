library(openxlsx)
library(stringdist)

#note: after running the function refresh the function again to do another one

# funtion for stringsim
lookupf <- function(UM, month) {
    # template for output
    template <- data.frame(matrix(ncol = 0, nrow = 0))
    #my look up data reference
    teladan <- read.xlsx("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/data_jml_spesies_ANJ.xlsx", 
                         sheet = month, cols =  1:8)
    y <- teladan$nama_latin
    #my data that's being looked up for
    mydata <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/ANJ PENDAKI detail 2020.xlsx",
                        sheet = UM, cols = 19, startRow = 3)
    mydata <- mydata[,1]
    for (i in mydata) {
        hitung <- stringsim(i, y)
        indeks <- match(max(hitung), hitung)
        value <- y[indeks]
        if (max(hitung) != 1) {
            mydf <- data.frame(namaLatin = c(i), closestMatch = c(value))
            template <- rbind(template, mydf)
        }
    }
    .GlobalEnv$output <- template
}

#export into csv file"
save <- function(NamaFile) {
    write.csv(output, paste0("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/",NamaFile))
}
