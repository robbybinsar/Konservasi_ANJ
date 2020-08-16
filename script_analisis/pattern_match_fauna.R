library(openxlsx)
library(stringr)
library(stringdist)
library(dplyr)

matching <- function(unitmanajemen, bulan){
#load data
dats <- "./pattern_match/pattern_match.xlsx"
df <- read.xlsx(dats,sheet = "analisa")
df[is.na(df)] <- 0 # ubah kolom 'nama latin' menjadi Nama.Latin
xt <- df %>% group_by(Nama.Latin) %>% summarize(Jumlah = sum(Jumlah))
y <- unique(df$Nama.Latin)

dats1 <- "D:/DATA ONEDRIVE/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/ANJ PENDAKI detail 2020.xlsx"
teladan <- read.xlsx(dats1, sheet = unitmanajemen, cols = 19, startRow = 3)
teladan <- mutate(teladan, Jumlah = NA)


# ANJ Detail pattern match
dataf <- data.frame(matrix(nrow = 0, ncol = 0))
x <- teladan[,1]
for (i in y) {
   hitung <- stringsim(i, x)
   indeks <- match(max(hitung), hitung)
   value <- x[indeks]
   if (max(hitung) < 0.75) {
       output <- data.frame(Nama.Latin = c(i), closest.pendaki.detail = c(value))
       dataf <- rbind(dataf, output)
       teladan <- bind_rows(teladan, select(output, Nama.Latin))
       teladan$Jumlah[match(i, teladan$Nama.Latin)] <- xt$Jumlah[match(i, xt$Nama.Latin)]
   } else {
       teladan$Jumlah[match(value,x)] <- xt$Jumlah[match(i,xt$Nama.Latin)]
   }
}
stopifnot(nrow(dataf) != 0)
#PATTERN MATCH BASELINE UM
dat <- "D:/DATA ONEDRIVE/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
dataFaunaUM <- read.xlsx(dat, sheet = "DATABASE FAUNA", cols = 1:34)

# Filter UM only
colnames(dataf)[1] <- "baru" ; dataf <- mutate(dataf, statusUM = NA)
dataFaunaUM <- filter(dataFaunaUM, UM == unitmanajemen)
x <- dataFaunaUM$Latin.name
y <- dataf$baru
for (i in y) {
    hitung <- stringsim(i, x)
    if (max(hitung) < 0.75) {
        dataf$statusUM[match(i,y)] <- "Baru"
    } else {
        dataf$statusUM[match(i,y)] <- 0
    }
}

# PATTERN MATCH BASELINE ALL
dataFaunaAll <- read.xlsx(dat, sheet = "ENTRY FAUNA")
colnames(dataFaunaAll)[c(12,14,16,17,18,19,20,21,22)] <- c("PPRI","Endemism","Presence.Reg12","Origin.Reg12","Seasonality.Reg12",
                                                        "Presence.Reg3","Origin.Reg3","Seasonality.Reg3","Habitat.lv1")

x <- dataFaunaAll$Latin.name
dataf <- mutate(dataf, statusAll = NA, Namalatin.fix = NA, closest.All = NA)
for (i in y) {
    hitung <- stringsim(i, x)
    indeks <- match(max(hitung), hitung)
    value <- x[indeks]
    if (max(hitung) < 0.75) {
        dataf$statusAll[match(i,y)] <- "Baru"
        dataf$closest.All[match(i, y)] <- value
    } else {
        dataf$statusAll[match(i,y)] <- 0
        dataf$Namalatin.fix[match(i,y)] <- value
    }
}
# Completing data
hasil <- merge(dataf, dataFaunaAll, by.x = "Namalatin.fix", by.y = "Latin.name",all = F)
hasil <- select(hasil, c(baru, closest.pendaki.detail, Namalatin.fix, statusUM,
                         statusAll,ANJ.ID:Habitat.lv1, -c(No,closest.All)))
baru <- filter(dataf, statusAll == "Baru")
mergeData1 <- merge(baru, df, by.x = "baru", by.y = "Nama.Latin", all = F)
mergeData1 <- mergeData1 %>% distinct(baru, .keep_all = T) %>%
    filter(baru != 0) %>% select(baru, closest.pendaki.detail,closest.All,
                                 statusUM:Jumlah,-Namalatin.fix)

#Writing and saving wb
wb <- loadWorkbook(dats)
writeDataTable(wb,sheet = unitmanajemen,mergeData1, colNames = T, rowNames = F, 
               tableName = paste0("hasil_non_exist_",unitmanajemen,bulan))
writeDataTable(wb,sheet = unitmanajemen,hasil, colNames = T, rowNames = F,
               startRow = nrow(mergeData1)+2,tableName = paste0("hasil_exist_",unitmanajemen,bulan))
writeData(wb, sheet = unitmanajemen, teladan, colNames = T, rowNames = F,
          startRow = nrow(hasil)+nrow(mergeData1)+ 4)
saveWorkbook(wb,dats,overwrite = T)

.GlobalEnv$hasil <- hasil
.GlobalEnv$virgin <- mergeData1
.GlobalEnv$pendakidetail <- teladan
}

citizen_science <- function(UM, bulan) {
    #load data
    dats <- "./pattern_match/pattern_match.xlsx"
    df <- read.xlsx(dats,sheet = "analisa")
    df[is.na(df)] <- 0
    y <- unique(df$Pengamat) #Nama kolom pengamat bisa dinamis
    
    datlook <- "D:/DATA ONEDRIVE/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/CITIZEN SCIENCE 2020.xlsx"
    dflook <- read.xlsx(datlook, sheet = UM, startRow = 3, cols = 1:14)
    
    #matching
    dataf <- data.frame(matrix(nrow = 0, ncol = 0))
    x <- dflook$Nama
    for (i in y) {
        hitung <- stringsim(i, x)
        indeks <- match(max(hitung), hitung)
        value <- x[indeks]
        if (max(hitung) < 0.8) {
            jumlah <- nrow(filter(df, Pengamat == i))
            output <- data.frame(Nama = c(i), closest.match = c(value), Bulan.Mulai = c(bulan), month = c(jumlah))
            colnames(output)[4] <- bulan
            dflook <- bind_rows(dflook, output)
        } else {
            jumlah <- nrow(filter(df, Pengamat == i))
            dflook[,bulan][match(value,x)] <- jumlah
        }
    }
    
    # Write and save wb
    dflook[is.na(dflook)] <- 0
    dflook <- select(dflook, Bulan.Mulai, Nama, closest.match, Jan:Des)
    write.csv(dflook, file = "D:/DATA ONEDRIVE/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/CITIZEN SCIENCE 2020.csv",
              row.names = F)
    .GlobalEnv$dflook <- dflook
    .GlobalEnv$Pengamat <- y
}
