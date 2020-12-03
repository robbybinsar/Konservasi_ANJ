library(dplyr)
library(rgbif)
library(openxlsx)
library(stringdist)
library(inborutils)
library(tidyverse)
library(taxize)
library(rcites)
library(rredlist)

set_token("Fafh0Cn4SPZ5rI86BeH5fwtt")
Sys.setenv(ENTREZ_KEY = "f7041649502ffe57baeb36b4be7acd695808")
Sys.setenv(IUCN_REDLIST_KEY = "5abaac6464f1172201641c99a3357982c62423e69a08920925bc430d1753812b")

# FAUNA
hasil_fauna <- data.frame(matrix(nrow = 0,ncol = 0))
read_fauna <- function() {
  
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
        NamaLatin_rev <- backbone$canonicalName[1]
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
      if((ncol(backbone$data) != 3 & length(backbone[["data"]][["canonicalName"]])!=0) | nrow(backbone$alternatives) != 0) {
        if (ncol(backbone$data) > 3) {
          backbone <- backbone$data
          NamaLatin <- backbone[["canonicalName"]]
          print(paste("Latin name is revised to", NamaLatin))
        } else if (backbone$alternatives[[6]] > 0.75){
          backbone <- backbone$alternatives
          NamaLatin <- backbone[["canonicalName"]]
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
    
    #NCBI ID
    NCBI_id <- get_ids(NamaLatin, db = c("ncbi"))
    NCBI_id <- NCBI_id[["ncbi"]][[1]]
    print(paste("NCBI ID:", NCBI_id))
    
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
    IUCN_dat <- iucn_summary(NamaLatin)
    IUCN <- iucn_status(IUCN_dat)[[1]]
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
    if(!is.na(IUCN)) {
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
    
    #IUCN Browse
    if (!is.na(IUCN)) {
      iucn_url <- as.data.frame(get_iucn(NamaLatin))$uri
      browseURL(iucn_url)
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
    
    output <- data.frame( NCBI.ID = c(NCBI_id),Group = c(kelas), Family = c(familyname), Latin.name = c(NamaLatin), 
                         English.name = c(EnglishName), CITES=c(CITES), IUCN = c(IUCN), 
                         PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018),
                         CMS = c(cms), Habitat.lv1 = c(Habitat.lv1))
    hasil_fauna <- bind_rows(hasil_fauna, output)
    .GlobalEnv$hasil_fauna <- hasil_fauna
    
    #Addmore entry Q
    addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
    if (addmore == 2) break
  }
  write.csv(hasil_fauna, "hasil_fauna.csv")
}

#FLORA
hasil_flora <- data.frame(matrix(nrow = 0,ncol = 0))
read_flora <- function() {
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
      if((ncol(backbone$data) != 3 & length(backbone[["data"]][["canonicalName"]])!= 0 ) | nrow(backbone$alternatives) != 0) {
        if (ncol(backbone$data) > 3) {
          backbone <- backbone$data
          NamaLatin <- backbone[["canonicalName"]]
          print(paste("Latin name is revised to", NamaLatin))
        } else if (backbone$alternatives[[6]] > 0.75){
          backbone <- backbone$alternatives
          uni <- unique(backbone[["canonicalName"]])
          hitung <- stringsim(NamaLatin, uni)
          value <- uni[match(max(hitung), hitung)]
          NamaLatin <- value
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
    IUCN_dat <- iucn_summary(NamaLatin)
    IUCN <- iucn_status(IUCN_dat)[[1]]
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
    
    #IUCN Browse
    if (!is.na(IUCN)) {
      iucn_url <- as.data.frame(get_iucn(NamaLatin))$uri
      browseURL(iucn_url)
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
    
    output <- data.frame(Class = c(kelas),Family = c(familyname), Latin.name = c(NamaLatin), 
                         English.name = c(EnglishName), 
                         CITES=c(CITES), IUCN = c(IUCN), PPRI = c(PPRI), Permenlhk.106= c(permenlhk2018)
    )
    hasil_flora <- bind_rows(hasil_flora, output)
    .GlobalEnv$hasil_flora <- hasil_flora
    
    #Addmore entry Q
    addmore <- readline(prompt = 'Add more entry? \n 1.Press Enter to add more entry \n 2.Type 2 to finish')
    if (addmore == 2) break
  }
  write.csv(hasil_flora, "hasil_flora.csv")
}
