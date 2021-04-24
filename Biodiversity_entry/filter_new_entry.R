library(openxlsx)
library(dplyr)

#Fauna
  # data fauna
  dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
  dataFaunaUM <- read.xlsx(dat, sheet = "DATABASE FAUNA", cols = 1:34)
  #Subset all entries in 2020
  NewEntriesFauna <- dataFaunaUM %>% filter(!is.na(dataFaunaUM[,"Pendaki.2020"])|!is.na(dataFaunaUM[,"Pendaki.2019"]))
  #Subset new entries only
  namakolom_fauna <- colnames(NewEntriesFauna[16:34])
  for(i in namakolom_fauna) {
    NewEntriesFauna <- NewEntriesFauna %>% filter(is.na(NewEntriesFauna[,i]))
  }


#Flora
  # data flora
  datFlo <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
  dataFloraUM <- read.xlsx(datFlo, sheet = "DATABASE FLORA")
  #Subset all entries in 2020
  NewEntriesFlora <- dataFloraUM %>% filter(!is.na(dataFloraUM[,"PENDAKI.2020"])|!is.na(dataFloraUM[,"PENDAKI.2019"]))
  #Subset new entries only
  namakolom_flora <- colnames(NewEntriesFlora[17:27])
  for(i in namakolom_flora) {
    NewEntriesFlora <- NewEntriesFlora %>% filter(is.na(NewEntriesFlora[,i]))
  }
  