library(openxlsx)
library(stringr)
library(stringdist)
library(dplyr)

# Note:
# Before running the function make sure the dataset is in tidy form.
# rename corresponding variable 'Nama.Latin' , 'Kelas', 'Pengamat', 'Nama.Lokal, 'Jumlah'

# FAUNA

matching_fauna <- function(unitmanajemen, bulan){
#load data
dats <- "./pattern_match/pattern_match.xlsx"
df <- read.xlsx(dats,sheet = "analisa")
df[is.na(df)] <- 0
xt <- df %>% group_by(Kelas, Nama.Latin, Nama.Lokal) %>% summarize(Jumlah = sum(Jumlah))
y <- unique(df$Nama.Latin)

dats1 <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/ANJ PENDAKI detail 2021.xlsx"
teladan <- read.xlsx(dats1, sheet = unitmanajemen, cols = 15:19, startRow = 3)
teladan <- mutate(teladan, Jumlah = NA)


# ANJ Detail pattern match
dataf <- data.frame(matrix(nrow = 0, ncol = 0))
x <- teladan$Nama.Latin
for (i in y) {
   hitung <- stringsim(i, x)
   indeks <- match(max(hitung), hitung)
   value <- x[indeks]
   if (max(hitung) < 0.70) {
       output <- data.frame(Nama.Latin = c(i), closest.pendaki.detail = c(value))
       dataf <- rbind(dataf, output)
       teladan <- bind_rows(teladan, select(output, Nama.Latin))
       teladan$Kelas[match(i, teladan$Nama.Latin)] <- xt$Kelas[match(i, xt$Nama.Latin)]
       teladan$Nama.Lokal[match(i, teladan$Nama.Latin)] <- xt$Nama.Lokal[match(i, xt$Nama.Latin)]
       teladan$Jumlah[match(i, teladan$Nama.Latin)] <- xt$Jumlah[match(i, xt$Nama.Latin)]
   } else {
       teladan$Jumlah[match(value,x)] <- xt$Jumlah[match(i,xt$Nama.Latin)]
   }
}
teladan$Jumlah[is.na(teladan$Jumlah)] <- 0

stopifnot(nrow(dataf) != 0)
#PATTERN MATCH BASELINE UM
dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
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
    if (max(hitung) < 0.70) {
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
                                 statusUM:Nama.Lokal,-Namalatin.fix)

mrg <- bind_rows(hasil, mergeData1)
teladan[is.na(teladan["Bulan"]), "Bulan"] <- bulan
for(j in mrg$baru){
    teladan$Database[match(j, teladan$Nama.Latin)] <- mrg$statusUM[match(j, mrg$baru)]
}

#Writing and saving wb
wb <- loadWorkbook(dats)
writeDataTable(wb,sheet = unitmanajemen,mergeData1, colNames = T, rowNames = F, 
               tableName = paste0("hasil_non_exist_",unitmanajemen,bulan))
writeDataTable(wb,sheet = unitmanajemen,hasil, colNames = T, rowNames = F,
               startRow = nrow(mergeData1)+3,tableName = paste0("hasil_exist_",unitmanajemen,bulan))
writeData(wb, sheet = unitmanajemen, teladan, colNames = T, rowNames = F,
          startRow = nrow(hasil)+nrow(mergeData1)+ 5)
saveWorkbook(wb,dats,overwrite = T)

.GlobalEnv$hasil_fauna <- hasil
.GlobalEnv$virgin_fauna <- mergeData1
.GlobalEnv$pendakidetail_fauna <- teladan
}

# FLORA

matching_flora <- function(unitmanajemen, bulan){
    #load data
    dats <- "./pattern_match/pattern_match_flora.xlsx"
    df <- read.xlsx(dats,sheet = "analisa")
    df[is.na(df)] <- 0 # ubah kolom 'nama latin' menjadi Nama.Latin
    xt <- df %>% group_by(Nama.Latin, Nama.Lokal) %>% summarize(Jumlah = sum(Jumlah))
    y <- unique(df$Nama.Latin)
    
    dats1 <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/ANJ PENDAKI detail 2021.xlsx"
    teladan <- read.xlsx(dats1, sheet = unitmanajemen, cols = 33:37, startRow = 3)
    teladan <- mutate(teladan, Jumlah = NA)
    
    
    # ANJ Detail pattern match
    dataf <- data.frame(matrix(nrow = 0, ncol = 0))
    x <- teladan$Nama.Latin
    for (i in y) {
        hitung <- stringsim(i, x)
        indeks <- match(max(hitung), hitung)
        value <- x[indeks]
        if (max(hitung) < 0.7) {
            output <- data.frame(Nama.Latin = c(i), closest.pendaki.detail = c(value))
            dataf <- rbind(dataf, output)
            teladan <- bind_rows(teladan, select(output, Nama.Latin))
            teladan$Nama.Lokal[match(i, teladan$Nama.Latin)] <- xt$Nama.Lokal[match(i,xt$Nama.Latin)]
            teladan$Jumlah[match(i, teladan$Nama.Latin)] <- xt$Jumlah[match(i, xt$Nama.Latin)]
        } else {
            teladan$Jumlah[match(value,x)] <- xt$Jumlah[match(i,xt$Nama.Latin)]
        }
    }
    teladan$Jumlah[is.na(teladan$Jumlah)] <- 0
    
    stopifnot(nrow(dataf) != 0)
    #PATTERN MATCH BASELINE UM
    dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
    dataFloraUM <- read.xlsx(dat, sheet = "DATABASE FLORA", cols = 1:26)
    
    # Filter UM only
    colnames(dataf)[1] <- "baru" ; dataf <- mutate(dataf, statusUM = NA)
    dataFloraUM <- filter(dataFloraUM, UM == unitmanajemen)
    x <- dataFloraUM$Latin.name
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
    dataFloraAll <- read.xlsx(dat, sheet = "ENTRY FLORA")
    
    x <- dataFloraAll$Latin.name
    dataf <- mutate(dataf, statusAll = NA, Namalatin.fix = NA, closest.All = NA)
    for (i in y) {
        hitung <- stringsim(i, x)
        indeks <- match(max(hitung), hitung)
        value <- x[indeks]
        if (max(hitung) < 0.70) {
            dataf$statusAll[match(i,y)] <- "Baru"
            dataf$closest.All[match(i, y)] <- value
        } else {
            dataf$statusAll[match(i,y)] <- 0
            dataf$Namalatin.fix[match(i,y)] <- value
        }
    }
    # Completing data
    hasil <- merge(dataf, dataFloraAll, by.x = "Namalatin.fix", by.y = "Latin.name",all = F)
    hasil <- select(hasil, c(baru, closest.pendaki.detail, Namalatin.fix, statusUM,
                             statusAll,ID:Endemism, -c(No,closest.All)))
    baru <- filter(dataf, statusAll == "Baru")
    mergeData1 <- merge(baru, df, by.x = "baru", by.y = "Nama.Latin", all = F)
    mergeData1 <- mergeData1 %>% distinct(baru, .keep_all = T) %>%
        filter(baru != 0) %>% select(baru, closest.pendaki.detail,closest.All,
                                     statusUM:Nama.Lokal,-Namalatin.fix)
    
    mrg <- bind_rows(hasil, mergeData1)
    teladan[is.na(teladan["Bulan"]), "Bulan"] <- bulan
    for(j in mrg$baru){
        teladan$Database[match(j, teladan$Nama.Latin)] <- mrg$statusUM[match(j, mrg$baru)]
    }
    #Writing and saving wb
    wb <- loadWorkbook(dats)
    writeDataTable(wb,sheet = unitmanajemen,mergeData1, colNames = T, rowNames = F, 
                   tableName = paste0("hasil_non_exist_",unitmanajemen,bulan))
    writeDataTable(wb,sheet = unitmanajemen,hasil, colNames = T, rowNames = F,
                   startRow = nrow(mergeData1)+3,tableName = paste0("hasil_exist_",unitmanajemen,bulan))
    writeData(wb, sheet = unitmanajemen, teladan, colNames = T, rowNames = F,
              startRow = nrow(hasil)+nrow(mergeData1)+ 5)
    saveWorkbook(wb,dats,overwrite = T)
    
    .GlobalEnv$hasil_flora <- hasil
    .GlobalEnv$virgin_flora <- mergeData1
    .GlobalEnv$pendakidetail_flora <- teladan
}

#CITIZEN SCIENCE
# Join with citizen 'pengamat' from Flora
# if get an error from closest.match means that there's no new entry 
citizen_science <- function(UM, bulan) {
    #load data
    dats <- "./pattern_match/pattern_match.xlsx"
    df <- read.xlsx(dats,sheet = "analisa")
    df[is.na(df)] <- 0
    y <- unique(df$Pengamat) #Nama kolom pengamat bisa dinamis
    
    datlook <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/CITIZEN SCIENCE 2021.xlsx"
    dflook <- read.xlsx(datlook, sheet = UM, startRow = 3, cols = 1:14)
    
    #matching
    dataf <- data.frame(matrix(nrow = 0, ncol = 0))
    x <- dflook$Nama
    for (i in y) {
        hitung <- stringsim(i, x)
        indeks <- match(max(hitung), hitung)
        value <- x[indeks]
        if (max(hitung) < 0.55) {
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
    if (ncol(dflook)==15) {
        dflook <- select(dflook, Bulan.Mulai, Nama, closest.match, Jan:Des)
    } else {
        dflook <- select(dflook, Bulan.Mulai, Nama, Jan:Des)
    }
    write.csv(dflook, file = "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/SUMMARY PENDAKI/CITIZEN SCIENCE 2021.csv",
              row.names = F)
    .GlobalEnv$dflook <- dflook
    .GlobalEnv$Pengamat <- y
}
