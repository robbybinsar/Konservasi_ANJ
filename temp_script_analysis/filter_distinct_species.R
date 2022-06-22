#Fauna

dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Fauna ANJ.xlsx"
dataFaunaUM <- read.xlsx(dat, sheet = "DATABASE FAUNA", cols = 1:35)

subset1 <- filter_at(dataFaunaUM, vars(Pendaki.2020:PPM.Remark), any_vars(!is.na(.)))
  vek1 <- unique(subset1$ID)
subset2 <- filter_at(dataFaunaUM, vars(Pendaki.2020:GSB.Lap.KBKT.Sem.1.2020), all_vars(is.na(.)))
  vek2 <- unique(subset2$ID)

keluar <- c(0)
for (i in vek2) {
  v <- !any(vek1 == i, na.rm = T)
  keluar <- sum(keluar, v)
}
