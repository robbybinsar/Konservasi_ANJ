library(dplyr)

# FAUNA

added_fauna <- data.frame(matrix(nrow = 0,ncol = 0))
readData_fauna <- function(UM, bulan) {
    while(TRUE) {
        #Kelas
        kelas <- readline(prompt = 'Kelas: \n 1. Aves \n 2. Mamalia \n 3. Reptil \n 4. Insekta \n 5. Pisces')
            if(kelas == 1){kelas <- "Aves"}
            else if(kelas == 2){kelas <- "Mamalia"}
            else if(kelas == 3){kelas <- "Reptil"}
            else if(kelas == 4){kelas <- "Insekta"}
            else if(kelas == 5){kelas <- "Pisces"}
            else {class <- kelas}
        
        #ANJ ID
        dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
        dataFauna <- read.xlsx(dat, sheet = "ENTRY FAUNA")
        colnames(dataFauna)[c(12,14,16,17,18,19,20,21,22)] <- c("PPRI","Endemism","Presence.Reg12","Origin.Reg12","Seasonality.Reg12",
                                                                "Presence.Reg3","Origin.Reg3","Seasonality.Reg3","Habitat.lv1")
        ANJ.ID <- paste0(toupper(substr(kelas,1,3)),"-",sum(dataFauna$Group == kelas)+1)
        
        # Family
        familyname <- readline(prompt = "Family: ")
            while(familyname == "") {
                familyname <- readline(prompt = "Family: ")
            }
        
        #nama latin
        NamaLatin <- readline(prompt = "Nama Latin: ")
            while(NamaLatin == "") {
                NamaLatin <- readline(prompt = "Nama Latin: ")
            }
            
        # English name
        EnglishName <- readline(prompt = "English Name: ")
            if (EnglishName == "") {EnglishName <- NA}
        
        #Indonesian name
        IndonesianName <- readline(prompt = "Nama Indonesia: ")
            if (IndonesianName == "") {IndonesianName <- NA}
        
        #Primary diet
        PrimaryDiet <- readline(prompt = "Primary diet: ")
            if (PrimaryDiet == "") {PrimaryDiet <- NA}
        
        #CITES
        pathcites <- "C:/Users/robby/Documents/ANJ/DATABASE/DOKUMEN/CITES/Index_of_CITES_Species_2020-08-19 05_48.csv"
        CITESLookup <- read.csv(pathcites)
        CITESvect <- CITESLookup$FullName
        CITESlist <- CITESLookup$CurrentListing
        splitcites <- strsplit(NamaLatin, " ")[[1]][1]
        hitung <- stringsim(NamaLatin, CITESvect)
        if (max(hitung) >= 0.9) {
            value <- CITESvect[match(max(hitung),hitung)]
            CITES <- CITESlist[match(value, CITESvect)]
        } else if (max(stringsim(splitcites, CITESvect)) >= 0.95) {
            value <- CITESvect[match(max(stringsim(splitcites, CITESvect)),stringsim(splitcites, CITESvect))]
            CITES <- CITESlist[match(value, CITESvect)]
        } else if (max(stringsim(familyname, CITESvect)) >= 0.95) {
            value <- CITESvect[match(max(stringsim(familyname, CITESvect)),stringsim(familyname, CITESvect))]
            CITES <- CITESlist[match(value, CITESvect)]
        } else {
            CITES <- NA
        }
        
        #IUCN
        IUCN <- readline(prompt = "IUCN redlist: ")
            if(sum(c("DD","LC","NT","VU","EN","CR","EX") %in% IUCN)==0) {IUCN <- NA}
        
        #PPRI 1999
        dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
        PPRILookup <- read.xlsx(dat, sheet = "LHK dan PP", cols = 9, startRow = 5)
        split1 <- strsplit(NamaLatin," ")[[1]][1] ; split1 <- paste(split1, "spp.") 
        if (max(stringsim(NamaLatin, PPRILookup$Nama.Latin), na.rm = T) >= 0.9) {
            PPRI <- "Dilindungi"
        } else if(max(stringsim(split1, PPRILookup$Nama.Latin), na.rm = T) >= 0.9) {
            PPRI <- "Dilindungi"
        } else {
            PPRI <- "Tidak Dilindungi"
        }
        
        #Permenlhk 2018
        permenlhk2018array <- read.xlsx(dat, sheet = "LHK dan PP", cols = 3, startRow = 5)
        split2 <-  strsplit(NamaLatin," ")[[1]][1] ; split2 <- paste(split2, "spp.")
        if (max(stringsim(NamaLatin, permenlhk2018array$Nama.Latin), na.rm = T) >= 0.9) {
            permenlhk2018 <- "Dilindungi"
        } else if(max(stringsim(split1, permenlhk2018array$Nama.Latin), na.rm = T) >= 0.9) {
            permenlhk2018 <- "Dilindungi"
        } else {
            permenlhk2018 <- "Tidak Dilindungi"
        }
        
        #Endemism
        endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            while(endemism != 1 & endemism != 2 & endemism != 3){
                endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            }
            if (endemism == 1){endemism <- "Endemik"}
            else if(endemism == 2){endemism <- "Tidak Endemik"}
            else if(endemism == 3){endemism <- "Data Deficient"}
        
        #CMS
        cms <- readline(prompt = 'CMS Status: \n 1. Migran (I) \n 2. Migran (II) \n 3. Migran (III)')
            while(cms != 1 & cms != 2 & cms != 3 & cms != "") {
                cms <- readline(prompt = 'CMS Status: \n 1. Migran (I) \n 2. Migran (II) \n 3. Migran (III)')
            }
            if (cms == 1) {cms <- "Migran (I)"}
            else if(cms == 2) {cms <- "Migran (II)"}
            else if(cms == 3) {cms <- "Migran (III)"}
            else {cms <- NA}
        
        #Presence status reg 1 dan 2
        Presence.Reg12 <- readline(prompt = 'Presence status Regional 1 & 2: \n 1. Extant \n 2. Possibly Extant \n 3. Presence Uncertain')
            if(Presence.Reg12 == 1) {Presence.Reg12 <- "Extant"}
            else if(Presence.Reg12 == 2) {Presence.Reg12 <- "Possibly Extant"}
            else if(Presence.Reg12 == 3) {Presence.Reg12 <- "Presence Uncertain"}
            else {Presence.Reg12 <- NA}
        
        #Origin status reg 1 dan 2
        Origin.Reg12 <- readline(prompt = 'Origin status Regional 1 & 2: \n 1. Native \n 2. Introduced \n 3. Origin Uncertain')
            if(Origin.Reg12 == 1) {Origin.Reg12 <- "Native"}
            else if(Origin.Reg12 == 2) {Origin.Reg12 <- "Introduced"}
            else if(Origin.Reg12 == 3) {Origin.Reg12 <- "Origin Uncertain"}
            else {Origin.Reg12 <- NA}
        
        #Seasonality reg 1 dan 2
        Seasonality.Reg12 <- readline(prompt = 'Seasonality status Regional 1 & 2: \n Res (Resident) \n Br (Breeding) \n NB (Non breeding) \n Unknown')
            if(Seasonality.Reg12 == "") {Seasonality.Reg12 <- NA}
        
        #Presence status reg 3
        Presence.Reg3 <- readline(prompt = 'Presence status Regional 3: \n 1. Extant \n 2. Possibly Extant \n 3. Presence Uncertain')
            if(Presence.Reg3 == 1) {Presence.Reg3 <- "Extant"}
            else if(Presence.Reg3 == 2) {Presence.Reg3 <- "Possibly Extant"}
            else if(Presence.Reg3 == 3) {Presence.Reg3 <- "Presence Uncertain"}
            else {Presence.Reg3 <- NA}
        
        #Origin status reg 3
        Origin.Reg3 <- readline(prompt = 'Origin status Regional 3: \n 1. Native \n 2. Introduced \n 3. Origin Uncertain')
            if(Origin.Reg3 == 1) {Origin.Reg3 <- "Native"}
            else if(Origin.Reg3 == 2) {Origin.Reg3 <- "Introduced"}
            else if(Origin.Reg3 == 3) {Origin.Reg3 <- "Origin Uncertain"}
            else {Origin.Reg3 <- NA}
        
        #Seasonality reg 3
        Seasonality.Reg3 <- readline(prompt = 'Seasonality status Regional 3: \n Res (Resident) \n Br (Breeding) \n NB (Non breeding) \n Unknown')
            if(Seasonality.Reg3 == "") {Seasonality.Reg3 <- NA}
        
        #Habitat
        Habitat.lv1 <- readline(prompt = 'Habitat: ')
            if(Habitat.lv1 == "") {Habitat.lv1 <- NA}
        
        output <- data.frame(ANJ.ID = c(ANJ.ID), Group = c(kelas), Family = c(familyname), Latin.name = c(NamaLatin), 
                             English.name = c(EnglishName), Indonesian.name = c(IndonesianName), 
                             Primary.Diet = c(PrimaryDiet), CITES=c(CITES), IUCN = c(IUCN), 
                             PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018), Endemism = c(endemism),
                             CMS = c(cms), Presence.Reg12 = c(Presence.Reg12), Origin.Reg12=c(Origin.Reg12),
                             Seasonality.Reg12 = c(Seasonality.Reg12), Presence.Reg3 = c(Presence.Reg3),
                             Origin.Reg3 = c(Origin.Reg3), Seasonality.Reg3 = c(Seasonality.Reg3), 
                             Habitat.lv1 = c(Habitat.lv1))
        added_fauna <- bind_rows(added_fauna, output)
        
        #Addmore entry Q
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
            if (addmore == 2) break
    }
    dats <- "./pattern_match/pattern_match.xlsx"
    wb <- loadWorkbook(dats)
    writeDataTable(wb,sheet = UM ,added_fauna , colNames = T, rowNames = F, startCol = 13, tableName = paste0("insert_to_entry_database_",UM,bulan))
    saveWorkbook(wb,dats,overwrite = T)
    .GlobalEnv$added_fauna <- added_fauna
}


#FLORA
added_flora <- data.frame(matrix(nrow = 0,ncol = 0))
readData_flora <- function(UM, bulan) {
    while(TRUE) {
        #Kelompok
        kelompok <- readline(prompt = 'Kelompok: \n Bambu \n Berkayu \n Epifit \n Herba \n Paku 
                             \n Jamur \n Liana \n Palem \n Pandan \n Perdu \n Pohon 
                             \n Semak \n Tidak berkayu \n Un')
        while (sum(c("Bambu", "Berkayu","Epifit", "Herba", "Paku",
                     "Jamur", "Liana", "Palem", "Pandan", "Perdu" , "Pohon",
                     "Semak", "Tidak berkayu", "Un") %in% kelompok) == 0) {
            kelompok <- readline(prompt = 'Kelompok: \n Bambu \n Berkayu \n Epifit \n Herba \n Paku \n
                             Jamur \n Liana \n Palem \n Pandan \n Perdu \n Pohon \n
                             Semak \n Tidak berkayu \n Un')
        }
        
        #ANJ ID
        dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
        dataFlora <- read.xlsx(dat, sheet = "ENTRY FLORA")
        ANJ.ID <- paste0(toupper(substr(kelompok,1,3)),"-",sum(dataFlora$Group == kelompok)+1)
        
        # Family
        familyname <- readline(prompt = "Family: ")
        while(familyname == "") {
            familyname <- readline(prompt = "Family: ")
        }
        
        #nama latin
        NamaLatin <- readline(prompt = "Nama Latin: ")
        while(NamaLatin == "") {
            NamaLatin <- readline(prompt = "Nama Latin: ")
        }
        
        # English name
        EnglishName <- readline(prompt = "English Name: ")
        if (EnglishName == "") {EnglishName <- NA}
        
        #Indonesian name
        IndonesianName <- readline(prompt = "Nama Indonesia: ")
        if (IndonesianName == "") {IndonesianName <- NA}
        
        #CITES
        pathcites <- "C:/Users/robby/Documents/ANJ/DATABASE/DOKUMEN/CITES/Index_of_CITES_Species_2020-08-19 05_48.csv"
        CITESLookup <- read.csv(pathcites)
        CITESvect <- CITESLookup$FullName
        CITESlist <- CITESLookup$CurrentListing
        splitcites <- strsplit(NamaLatin, " ")[[1]][1]
        hitung <- stringsim(NamaLatin, CITESvect)
        if (max(hitung) >= 0.9) {
            value <- CITESvect[match(max(hitung),hitung)]
            CITES <- CITESlist[match(value, CITESvect)]
        } else if (max(stringsim(splitcites, CITESvect)) >= 0.95) {
            value <- CITESvect[match(max(stringsim(splitcites, CITESvect)),stringsim(splitcites, CITESvect))]
            CITES <- CITESlist[match(value, CITESvect)]
        } else if (max(stringsim(familyname, CITESvect)) >= 0.95) {
            value <- CITESvect[match(max(stringsim(familyname, CITESvect)),stringsim(familyname, CITESvect))]
            CITES <- CITESlist[match(value, CITESvect)]
        } else {
            CITES <- NA
        }
        
        #IUCN
        IUCN <- readline(prompt = "IUCN redlist: ")
        if(sum(c("DD","LC","NT","VU","EN","CR","EX") %in% IUCN)==0) {IUCN <- NA}
        
        #PPRI 1999
        dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
        PPRILookup <- read.xlsx(dat, sheet = "LHK dan PP", cols = 9, startRow = 5)
        split1 <- strsplit(NamaLatin," ")[[1]][1] ; split1 <- paste(split1, "spp.") 
        if (max(stringsim(NamaLatin, PPRILookup$Nama.Latin), na.rm = T) >= 0.9) {
            PPRI <- "Dilindungi"
        } else if(max(stringsim(split1, PPRILookup$Nama.Latin), na.rm = T) >= 0.9) {
            PPRI <- "Dilindungi"
        } else {
            PPRI <- "Tidak Dilindungi"
        }
        
        #Permenlhk 2018
        permenlhk2018array <- read.xlsx(dat, sheet = "LHK dan PP", cols = 3, startRow = 5)
        split2 <-  strsplit(NamaLatin," ")[[1]][1] ; split2 <- paste(split2, "spp.")
        if (max(stringsim(NamaLatin, permenlhk2018array$Nama.Latin), na.rm = T) >= 0.9) {
            permenlhk2018 <- "Dilindungi"
        } else if(max(stringsim(split1, permenlhk2018array$Nama.Latin), na.rm = T) >= 0.9) {
            permenlhk2018 <- "Dilindungi"
        } else {
            permenlhk2018 <- "Tidak Dilindungi"
        }
        
        #Endemism
        endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
        while(endemism != 1 & endemism != 2 & endemism != 3){
            endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
        }
        if (endemism == 1){endemism <- "Endemik"}
        else if(endemism == 2){endemism <- "Tidak Endemik"}
        else if(endemism == 3){endemism <- "Data Deficient"}
        
        output <- data.frame(ID = c(ANJ.ID), Group = c(kelompok), Family = c(familyname), Latin.name = c(NamaLatin), 
                             English.name = c(EnglishName), Indonesian.name = c(IndonesianName), 
                             CITES=c(CITES), IUCN = c(IUCN), PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018), Endemism = c(endemism)
                             )
        added_flora <- bind_rows(added_flora, output)
        
        #Addmore entry Q
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
        if (addmore == 2) break
    }
    dats <- "./pattern_match/pattern_match_flora.xlsx"
    wb <- loadWorkbook(dats)
    writeDataTable(wb,sheet = UM ,added_flora , colNames = T, rowNames = F, startCol = 13, tableName = paste0("insert_to_entry_database_",UM,bulan))
    saveWorkbook(wb,dats,overwrite = T)
    .GlobalEnv$added_flora <- added_flora
}
