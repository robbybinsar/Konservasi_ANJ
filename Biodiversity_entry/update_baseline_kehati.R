library(dplyr)
library(rgbif)
library(openxlsx)
library(stringdist)
library(inborutils)
library(tidyverse)
library(taxize)
library(rcites)
library(rredlist)

# FAUNA

added_fauna <- data.frame(matrix(nrow = 0,ncol = 0))
readData_fauna <- function(UM, bulan) {
    #data fauna foyr ID
    dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
    dataFauna <- read.xlsx(dat, sheet = "ENTRY FAUNA")
    colnames(dataFauna)[c(12,14,16,17,18,19,20,21,22)] <- c("PPRI","Endemism","Presence.Reg12","Origin.Reg12","Seasonality.Reg12",
                                                            "Presence.Reg3","Origin.Reg3","Seasonality.Reg3","Habitat.lv1")
    ID_all <- select(dataFauna, ANJ.ID, Group)
    
    while(TRUE) {
        #nama latin
        NamaLatin <- readline(prompt = "Nama Latin: ")
        while(NamaLatin == "") {
            NamaLatin <- readline(prompt = "Nama Latin: ")
        }
        #using name_suggest() from rgbif
        suggestion_name <- name_suggest(q = NamaLatin)$data
        suggestion_name <- suggestion_name[rowSums(is.na(suggestion_name)) == 0, ]
            if(nrow(suggestion_name) != 0) {
                suggestion_vect <- suggestion_name$canonicalName
                hitung <- stringsim(NamaLatin, suggestion_vect)
                #using name_backbone from rgbif
                if(max(hitung) >= 0.90) {
                    backbone <- name_backbone(name = suggestion_vect[match(max(hitung),hitung)], 
                                              rank = "species")
                    NamaLatin_rev <- backbone$species[1]
                    if(identical(NamaLatin, NamaLatin_rev)) {
                        print(paste("Nama Latin tersedia di database GBIF"))
                    } else {
                        print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                        NamaLatin <- NamaLatin_rev
                    }
                } else {
                    df_suggestion <- mutate(suggestion_name, sim_degree = hitung)
                    print(df_suggestion)
                    r_indeks <- readline(prompt = "Pilih Nama Latin yang sesuai dengan memasukkan row index dari tabel di atas \n tekan Enter untuk menggunakan Nama Latin user")
                    if(sum(seq_len(nrow(df_suggestion)) %in% r_indeks) == 1) {
                        NamaLatin_rev <- df_suggestion$canonicalName[match(r_indeks, seq_len(nrow(df_suggestion)))]
                        backbone <- name_backbone(name = NamaLatin_rev)
                        print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                        NamaLatin <- NamaLatin_rev
                    } else {
                        backbone <- data.frame(matrix(ncol = 1, nrow = 0))
                        NamaLatin <- NamaLatin
                        print("Nama Latin tidak ada di database GBIF \n Menggunakan Nama Latin dari user")
                    }
                }
                
            } else {
                #using name_backbone_verbose
                backbone <- name_backbone_verbose(name = NamaLatin)
                if((ncol(backbone$data) != 3 & length(backbone[["data"]][["canonicalName"]])==0) | nrow(backbone$alternatives) != 0) {
                    if (ncol(backbone$data) > 3) {
                        backbone <- backbone$data
                        NamaLatin <- backbone[["species"]]
                        print(paste("Latin name is revised to", NamaLatin))
                    } else if (backbone$alternatives[[6]] > 0.75){
                        backbone <- backbone$alternatives
                        NamaLatin <- backbone[["species"]]
                        print(paste("Latin name is revised to", NamaLatin))
                    }
                    
                } else {
                    #using gbif_species_name_match() from rgbif
                    df_latin <- data.frame(name = c(NamaLatin))
                    backbone <- gbif_species_name_match(df_latin, name = "name", gbif_terms = c("usageKey", "rank","matchType",
                                                        "kingdom","phylum","class", "order","family","genus", "species", "confidence", "synonym", 
                                                        "status"))
                    if (!is.na(backbone$species)) {
                        NamaLatin_rev <- backbone$species[1]
                        print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                        NamaLatin <- NamaLatin_rev
                    } else {
                        # using tol_resolve() from taxize
                        resolve_name <- tol_resolve(names = c(NamaLatin))
                        backbone <- name_backbone(name = resolve_name$unique_name)
                        NamaLatin_rev <- backbone$species
                        print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to Open Tree of Life Database"))
                        NamaLatin <- NamaLatin_rev
                    }
                    
                }
            }
        
        #Kelas
        if (nrow(backbone) > 0){
            kelas <- backbone$class[1]
            print(paste("Kelas:",kelas))
        } else if(nrow(backbone) == 0){
            kelas <- readline(prompt = 'Kelas: \n 1. Aves \n 2. Mamalia \n 3. Reptil \n 4. Insekta \n 5. Pisces')
            if(kelas == 1){kelas <- "Aves"}
            else if(kelas == 2){kelas <- "Mamalia"}
            else if(kelas == 3){kelas <- "Reptil"}
            else if(kelas == 4){kelas <- "Insekta"}
            else if(kelas == 5){kelas <- "Pisces"}
            else {class <- kelas}
        }
        
        #ANJ ID
        
        lookup <- dataFauna$Latin.name
        hitung <- max(stringsim(NamaLatin, lookup))
        if( hitung > 0.75) {
          ANJ.ID <- paste(dataFauna$ANJ.ID[match(hitung, stringsim(NamaLatin, lookup))], "(exist)")
          print(paste("Spesies ditemukan pada database lokal ID: ", ANJ.ID, 
                      ", Matching score:", round(hitung, 2)))
        } else {
          ANJ.ID <- paste0(toupper(substr(kelas,1,3)),"-",sum(ID_all$Group == kelas)+1)
          ID_all <- ID_all %>% add_row(ANJ.ID = ANJ.ID, Group = kelas)
        }
        
        
        # Family
        if (nrow(backbone) > 0) {
            familyname <- backbone$family[1]
            print(paste("Family:", familyname))
        } else if(nrow(backbone) == 0){
            familyname <- readline(prompt = "Family: ")
            while(familyname == "") {
                familyname <- readline(prompt = "Family: ")
            }
        }
            
        # English name
        toCommon <- sci2comm(NamaLatin)
        if (length(toCommon[[1]]) > 0) {
            EnglishName <- toCommon[[1]]
            print(paste("Common name retrieved from database:", EnglishName))
        } else {
            EnglishName <- readline(prompt = "English Name: ")
                if (EnglishName == "") {EnglishName <- NA}
        }
        
        # Conservation status
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
            print(paste("CITES:",CITES))
            
            #IUCN
            IUCN_dat <- rl_search(NamaLatin)$result
            if (length(IUCN_dat) > 0) {
              IUCN <- IUCN_dat$category[1]
            } else {
              IUCN <- NA
              }
            print (paste("IUCN Redlist Category:", IUCN))
            
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
            print(paste("PPRI:", PPRI))
            
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
            print(paste("PERMENLHK:", permenlhk2018))
            
            #Habitat
            if(length(IUCN_dat)>0) {
                Habitat.lv1 <- rl_narrative(NamaLatin)$result$habitat
                print(paste("Habitat:", Habitat.lv1))
            } else {
                Habitat.lv1 <- readline(prompt = 'Habitat: ')
                if(Habitat.lv1 == "") {Habitat.lv1 <- NA}
            }
            
            #CMS
            taxonconcept <- spp_taxonconcept(query_taxon = NamaLatin, taxonomy = "CMS")
            if (length(taxonconcept) > 0) {
                id_cms <- taxonconcept$all_id[[1]]
                browseURL(paste0("https://speciesplus.net/#/taxon_concepts/",id_cms,"/legal"))
                cms <- readline(prompt = 'CMS Status: \n 1. Migran (I) \n 2. Migran (II) \n 3. Migran (III)')
                while(cms != 1 & cms != 2 & cms != 3 & cms != "") {
                    cms <- readline(prompt = 'CMS Status: \n 1. Migran (I) \n 2. Migran (II) \n 3. Migran (III)')
                }
                if (cms == 1) {cms <- "Migran (I)"}
                else if(cms == 2) {cms <- "Migran (II)"}
                else if(cms == 3) {cms <- "Migran (III)"}
                else {cms <- NA}
                
            } else {
                cms <- NA
                print("CMS: Spesies tidak ada dalam appendix CMS")
            }
            
        
        #Indonesian name
        IndonesianName <- readline(prompt = "Nama Indonesia: ")
            if (IndonesianName == "") {IndonesianName <- NA}
        
        #Primary diet
        PrimaryDiet <- readline(prompt = "Primary diet: ")
            if (PrimaryDiet == "") {PrimaryDiet <- NA}
        
        #Endemism
        endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            while(endemism != 1 & endemism != 2 & endemism != 3){
                endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
            }
            if (endemism == 1){endemism <- "Endemik"}
            else if(endemism == 2){endemism <- "Tidak Endemik"}
            else if(endemism == 3){endemism <- "Data Deficient"}
        
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
        
        output <- data.frame(ANJ.ID = c(ANJ.ID), Group = c(kelas), Family = c(familyname), Latin.name = c(NamaLatin), 
                             English.name = c(EnglishName), Indonesian.name = c(IndonesianName), 
                             Primary.Diet = c(PrimaryDiet), CITES=c(CITES), IUCN = c(IUCN), 
                             PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018), Endemism = c(endemism),
                             CMS = c(cms), Presence.Reg12 = c(Presence.Reg12), Origin.Reg12=c(Origin.Reg12),
                             Seasonality.Reg12 = c(Seasonality.Reg12), Presence.Reg3 = c(Presence.Reg3),
                             Origin.Reg3 = c(Origin.Reg3), Seasonality.Reg3 = c(Seasonality.Reg3), 
                             Habitat.lv1 = c(Habitat.lv1))
        added_fauna <- bind_rows(added_fauna, output)
        .GlobalEnv$added_fauna <- added_fauna
        
        #Addmore entry Q
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
          if (addmore == 2) break
    }
    dats <- "./pattern_match/pattern_match.xlsx"
    wb <- loadWorkbook(dats)
    writeDataTable(wb,sheet = UM ,added_fauna , colNames = T, rowNames = F, startCol = 10, tableName = paste0("insert_to_entry_database_",UM,bulan))
    saveWorkbook(wb,dats,overwrite = T)
}


#FLORA
added_flora <- data.frame(matrix(nrow = 0,ncol = 0))
readData_flora <- function(UM, bulan) {
    
    # Data flora for creating ID
    dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
    dataFlora <- read.xlsx(dat, sheet = "ENTRY FLORA")
    ID_all <- select(dataFlora, ID, Class)
    
    while(TRUE) {
      
        #nama latin
        NamaLatin <- readline(prompt = "Nama Latin: ")
        while(NamaLatin == "") {
            NamaLatin <- readline(prompt = "Nama Latin: ")
        }
        #using name_suggest() from rgbif
        suggestion_name <- name_suggest(q = NamaLatin)$data
        suggestion_name <- suggestion_name[rowSums(is.na(suggestion_name)) == 0, ]
        if(nrow(suggestion_name) != 0) {
            suggestion_vect <- suggestion_name$canonicalName
            hitung <- stringsim(NamaLatin, suggestion_vect)
            #using name_backbone from rgbif
            if(max(hitung) >= 0.90) {
                backbone <- name_backbone(name = suggestion_vect[match(max(hitung),hitung)], 
                                          rank = "species")
                NamaLatin_rev <- backbone$species[1]
                if(identical(NamaLatin, NamaLatin_rev)) {
                    print(paste("Nama Latin tersedia di database GBIF"))
                } else {
                    print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                    NamaLatin <- NamaLatin_rev
                }
            } else {
                df_suggestion <- mutate(suggestion_name, sim_degree = hitung)
                print(df_suggestion)
                r_indeks <- readline(prompt = "Pilih Nama Latin yang sesuai dengan memasukkan row index dari tabel di atas \n tekan Enter untuk menggunakan Nama Latin user")
                if(sum(seq_len(nrow(df_suggestion)) %in% r_indeks) == 1) {
                    NamaLatin_rev <- df_suggestion$canonicalName[match(r_indeks, seq_len(nrow(df_suggestion)))]
                    backbone <- name_backbone(name = NamaLatin_rev)
                    print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                    NamaLatin <- NamaLatin_rev
                } else {
                    backbone <- data.frame(matrix(ncol = 1, nrow = 0))
                    NamaLatin <- NamaLatin
                    print("Nama Latin tidak ada di database GBIF \n Menggunakan Nama Latin dari user")
                }
            }
            
        } else {
            #using name_backbone_verbose
            backbone <- name_backbone_verbose(name = NamaLatin)
            if((ncol(backbone$data) != 3 & length(backbone[["data"]][["canonicalName"]])==0) | nrow(backbone$alternatives) != 0) {
                if (ncol(backbone$data) > 3) {
                    backbone <- backbone$data
                    NamaLatin <- backbone[["species"]]
                    print(paste("Latin name is revised to", NamaLatin))
                } else if (backbone$alternatives[[6]] > 0.75){
                    backbone <- backbone$alternatives
                    NamaLatin <- backbone[["species"]]
                    print(paste("Latin name is revised to", NamaLatin))
                }
                
            } else {
                #using gbif_species_name_match() from rgbif
                df_latin <- data.frame(name = c(NamaLatin))
                backbone <- gbif_species_name_match(df_latin, name = "name", gbif_terms = c("usageKey", "rank","matchType",
                                                                                            "kingdom","phylum","class", "order","family","genus", "species", "confidence", "synonym", 
                                                                                            "status"))
                if (!is.na(backbone$species)) {
                    NamaLatin_rev <- backbone$species[1]
                    print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to GBIF database"))
                    NamaLatin <- NamaLatin_rev
                } else {
                    # using tol_resolve() from taxize
                    resolve_name <- tol_resolve(names = c(NamaLatin))
                    backbone <- name_backbone(name = resolve_name$unique_name)
                    NamaLatin_rev <- backbone$species
                    print(paste(NamaLatin, "is revised to", NamaLatin_rev, "according to Open Tree of Life Database"))
                    NamaLatin <- NamaLatin_rev
                }
                
            }
        }
        
        # kelas
        if (nrow(backbone) > 0) {
            kelas <- backbone$class[1]
            print(paste("Kelas:", kelas))
        } else {
            kelas <- readline(prompt = "kelas:")
            while(kelas == "") {
                kelas <- readline(prompt = "Kelas:")
            }
        }
        
        #ANJ ID
        lookup <- dataFlora$Latin.name
        hitung <- max(stringsim(NamaLatin, lookup))
        if( hitung > 0.75) {
            ANJ.ID <- paste(dataFlora$ID[match(hitung, stringsim(NamaLatin, lookup))], "(exist)")
            print(paste("Spesies ditemukan pada database lokal ID: ", ANJ.ID, 
                  ", Matching score:", round(hitung, 2)))
        } else {
            ANJ.ID <- paste0(toupper(substr(kelas,1,3)),"-",sum(ID_all$Class == kelas)+1)
            ID_all <- ID_all %>% add_row(ID = ANJ.ID, Class = kelas)
        }
        
        #Kelompok
        kelompok <- readline(prompt = 'Kelompok:')
        
        # Family
        if (nrow(backbone) > 0) {
            familyname <- backbone$family[1]
            print(paste("Family:", familyname))
        } else if(nrow(backbone) == 0){
            familyname <- readline(prompt = "Family: ")
            while(familyname == "") {
                familyname <- readline(prompt = "Family: ")
            }
        }
        
        
        # English name
        toCommon <- sci2comm(NamaLatin)
        if (length(toCommon[[1]]) > 0) {
            EnglishName <- toCommon[[1]]
            print(paste("Common name retrieved from database:", EnglishName))
        } else {
            EnglishName <- readline(prompt = "English Name: ")
            if (EnglishName == "") {EnglishName <- NA}
        }
        
        # Conservation status
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
            print(paste("CITES:",CITES))
            
            #IUCN
            IUCN_dat <- rl_search(NamaLatin)$result
            if (length(IUCN_dat) > 0) {
              IUCN <- IUCN_dat$category[1]
            } else {
              IUCN <- NA
              }
            print (paste("IUCN Redlist Category:", IUCN))
            
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
            print(paste("PPRI:", PPRI))
        
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
            print(paste("PERMENLHK:", permenlhk2018))
            
        #Indonesian name
        IndonesianName <- readline(prompt = "Nama Indonesia: ")
        if (IndonesianName == "") {IndonesianName <- NA}
        
        
        #Endemism
        endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
        while(endemism != 1 & endemism != 2 & endemism != 3){
            endemism <- readline(prompt = 'Status Endemisme: \n 1. Endemik \n 2. Tidak Endemik \n 3. Data Deficient')
        }
        if (endemism == 1){endemism <- "Endemik"}
        else if(endemism == 2){endemism <- "Tidak Endemik"}
        else if(endemism == 3){endemism <- "Data Deficient"}
        
        output <- data.frame(ID = c(ANJ.ID), Group = c(kelompok), Class = c(kelas),Family = c(familyname), Latin.name = c(NamaLatin), 
                             English.name = c(EnglishName), Indonesian.name = c(IndonesianName), 
                             CITES=c(CITES), IUCN = c(IUCN), PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018), Endemism = c(endemism)
                             )
        added_flora <- bind_rows(added_flora, output)
        .GlobalEnv$added_flora <- added_flora
        
        #Addmore entry Q
        addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
        if (addmore == 2) break
    }
    dats <- "./pattern_match/pattern_match_flora.xlsx"
    wb <- loadWorkbook(dats)
    writeDataTable(wb,sheet = UM ,added_flora , colNames = T, rowNames = F, startCol = 10, tableName = paste0("insert_to_entry_database_",UM,bulan))
    saveWorkbook(wb,dats,overwrite = T)
}
