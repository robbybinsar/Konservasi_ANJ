library(stringdist)
library(dplyr)

UM <- c("SMM", "ANJA", "ANJAS", "ANJAP", "PMP", "PPM", "KAL")

for (i in UM) {
  myarray <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/CITIZEN SCIENCE 2020.xlsx",
            sheet = i, startRow = 3, cols = 2)
  pertama <- nrow(myarray)
  myarray2020 <- myarray$Nama
  lkupvalue <- read.xlsx("C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/2019/CITIZEN SCIENCE 2019.xlsx",
                         sheet = i, startRow = 3, cols = 2)
  lkupvalue <- lkupvalue$Nama
  for(y in lkupvalue) {
    hitung <- stringsim(y, myarray2020)
    if (max(hitung) < 0.75) {
      myarray <- myarray %>% add_row(Nama = y)
    } 
  }
  kedua <- nrow(myarray)
  write.csv(myarray, paste0("./cs_2019-2020Jul/", i,".csv"))
  print(paste(i, "bertambah", kedua-pertama))
}

# Total akumulasi
output <- data.frame(matrix(nrow = 0, ncol = 0))
for (i in UM) {
  df <- read.csv(paste0("./cs_2019-2020Jul/",i,".csv"))
  output <- bind_rows(output, df)
}
