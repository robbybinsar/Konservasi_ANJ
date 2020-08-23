library(dplyr)

# UPDATE SHEET ENTRY FLORA
added <- data.frame(matrix(nrow = 0,ncol = 0))
readData <- function() {
    while(TRUE) {
        #habitus
        habitus <- readline(prompt = 'Habitus: ')
        hab <- unique(dataFlora$Group)    
            while(sum(c(hab) %in% habitus)== 0) {
                habitus <- readline(prompt = 'Habitus: ')
            }
        
        #ANJ ID
        ID <- paste0(toupper(substr(habitus,1,3)),"-",sum(dataFlora$Group == habitus)+1)
        
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
        CITES <- readline(prompt = "CITES Appendix: ")
            if(sum(c("I","II","III") %in% CITES)== 0) {CITES <- NA}
        
        #IUCN
        IUCN <- readline(prompt = "IUCN redlist: ")
            if(sum(c("DD","LC","NT","VU","EN","CR","EX") %in% IUCN)==0) {IUCN <- NA}
        
        #PPRI 1999
        PPRI <- readline(prompt = 'PP/RI: \n 1. Dilindungi \n 2. Tidak Dilindungi')
            while(PPRI != 1 & PPRI != 2){
                PPRI <- readline(prompt = 'PP/RI: \n 1. Dilindungi \n 2. Tidak Dilindungi')
            }    
            if (PPRI == 1){PPRI <- "Dilindungi"}
            else if(PPRI == 2){PPRI <- "Tidak Dilindungi"}
            
        #Permenlhk 2018
        permenlhk2018 <- readline(prompt = 'PERMENLHK 2018: \n 1. Dilindungi \n 2. Tidak Dilindungi')
            while(permenlhk2018 != 1 & permenlhk2018 != 2){
                permenlhk2018 <- readline(prompt = 'PERMENLHK 2018: \n 1. Dilindungi \n 2. Tidak Dilindungi')
            }
            if (permenlhk2018 == 1){permenlhk2018 <- "Dilindungi"}
            else if(permenlhk2018 == 2){permenlhk2018 <- "Tidak Dilindungi"}
        
        #Endemism
        endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            while(endemism != 1 & endemism != 2 & endemism != 3){
                endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            }
            if (endemism == 1){endemism <- "Endemik"}
            else if(endemism == 2){endemism <- "Tidak Endemik"}
            else if(endemism == 3){endemism <- "Data Deficient"}
        
        output <- data.frame(Group = c(habitus), Family = c(familyname), Latin.name = c(NamaLatin), 
                             English.name = c(EnglishName), Indonesian.name = c(IndonesianName), 
                             CITES=c(CITES), IUCN = c(IUCN), PPRI = c(PPRI), 
                             Permenlhk.106= c(permenlhk2018), Endemism = c(endemism),ID = c(ID))
        dataFlora <- bind_rows(dataFlora, output)
        added <- bind_rows(added, output)
        
        #Addmore entry Q
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
        if (addmore == 2) break
    }
    .GlobalEnv$dataFlora <- dataFlora
    .GlobalEnv$added <- added
}

# now call the function, it will allow inputs until an empty year is typed
readData()

# UPDATE SHEET DATABASE FLORA
df <- data.frame(matrix(nrow = 0, ncol = 0))
readdata2 <- function() {
    while(TRUE) {
        nama.latin <- readline(prompt = 'Insert: \n Type 1 to insert from Added dataframe \n If not just type the name right away')
            if(nama.latin == 1) {nama.latin <- added$Latin.name}
        unitmanajemen <- readline(prompt = 'UM: ')
            while(sum(c("ANJA","ANJAS","SMM","KAL","PMP","PPM","ANJAP") %in% unitmanajemen)==0) {
                unitmanajemen <- readline(prompt = 'UM: ')
            }
        bulan <- readline(prompt = "Bulan ditemukan: ")
            while(sum(c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep","Okt","Nov","Des") %in% bulan)==0) {
                bulan <- readline(prompt = 'Bulan ditemukan: ')
            }
        output <- data.frame(UM = c(rep(unitmanajemen, length(nama.latin))), nama.latin = c(nama.latin), bulan = c(rep(bulan,length(nama.latin))))
        df <- bind_rows(df, output)
        
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
        if (addmore == 2) break
    }
    .GlobalEnv$databaseFlora <- df
}




