library(openxlsx)
library(sqldf)
#Loading data
dat <- "D:/DATA ONEDRIVE/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
dataFauna <- read.xlsx(dat, sheet = "ENTRY FAUNA")
colnames(dataFauna)[c(12,14,16,17,18,19,20,21,22)] <- c("PPRI","Endemism","Presence.Reg12","Origin.Reg12","Seasonality.Reg12",
                                                        "Presence.Reg3","Origin.Reg3","Seasonality.Reg3","Habitat.lv1")
dataFaunaAll <- read.xlsx(dat, sheet = "DATABASE FAUNA", cols = 1:34)


# Pengecekan penulisan
cekEr <- which(dataFauna == "N/A" | dataFauna == "0", arr.ind = T)
dataFauna[dataFauna== "0"] <- NA

#Kasih number untuk subset dan write data
#dataFauna <- rbind(c(seq_len(ncol(dataFauna))), dataFauna)

#Writing and saving wb entry fauna sheet
wb <- loadWorkbook(dat)
writeData(wb,sheet = "ENTRY FAUNA" ,dataFauna[,1:22], colNames = F, rowNames = F, startRow = 2)
saveWorkbook(wb,dat,overwrite = T)

#Writing and saving wb database fauna sheet
wb <- loadWorkbook(dat)
update_database_all <- function(x = databaseFauna) {
writeData(wb,sheet = "DATABASE FAUNA" ,as.data.frame(x[,2]), colNames = F, rowNames = F, xy = c(5, nrow(dataFaunaAll)+2))
writeData(wb,sheet = "DATABASE FAUNA" ,as.data.frame(x[,1]), colNames = F, rowNames = F, xy = c(2, nrow(dataFaunaAll)+2))
writeData(wb,sheet = "DATABASE FAUNA" ,as.data.frame(x[,3]), colNames = F, rowNames = F, xy = c(14, nrow(dataFaunaAll)+2))
saveWorkbook(wb,dat,overwrite = T)
}
