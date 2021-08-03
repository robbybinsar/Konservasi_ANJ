library(stringdist)
library(dplyr)
library(openxlsx)

UM <- c("SMM", "ANJA", "ANJAS", "ANJAP", "PMP", "PPM", "KAL")

for (i in UM) {
  myarray <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/2021/CITIZEN SCIENCE 2021.xlsx",
            sheet = i, startRow = 3, cols = 2)
  pertama <- nrow(myarray)
  lkupvalue <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/2020/CITIZEN SCIENCE 2020.xlsx",
                         sheet = i, startRow = 3, cols = 2)
  lkupvalue <- lkupvalue$Nama
  lkupvalue2 <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/2019/CITIZEN SCIENCE 2019.xlsx",
                          sheet = i, startRow = 3, cols = 2)
  lkupvalue2 <- lkupvalue2$Nama
  gabungan <- list(x = lkupvalue, y = lkupvalue2)
  var <- c("x", "y")
  for(z in var) {
    for(y in gabungan[[z]]) {
      hitung <- stringsim(y, myarray$Nama)
      if (max(hitung) < 0.75) {
        myarray <- myarray %>% add_row(Nama = y)
      } 
    }
  }
  
  kedua <- nrow(myarray)
  write.csv(myarray, paste0("./cs_2019-2021/2019 - 2021/", i,".csv"))
  print(paste(i, "bertambah", kedua-pertama))
}

# Total akumulasi
output <- data.frame(matrix(nrow = 0, ncol = 0))
for (i in UM) {
  df <- read.csv(paste0("./cs_2019-2021/2019 - 2021/",i,".csv"))
  output <- bind_rows(output, df)
}

