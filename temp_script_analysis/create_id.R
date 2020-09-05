dat <- "C:/Users/robby/OneDrive - PT. Austindo Nusantara Jaya Tbk/BIODIVERSITY/01. Database Flora ANJ.xlsx"
dataFlora <- read.xlsx(dat, sheet = "ENTRY FLORA")

kelasu <- unique(dataFlora$Class)
template <- data.frame(matrix(ncol = 0, nrow = 0))
for (i in kelasu) {
  mydata <- dataFlora %>% filter(Class == i) %>% select(Class, Latin.name)
  id <- sapply(seq_len(nrow(mydata)), 
               function(x) {paste0(toupper(substr(i,1,3)),"-",x)})
  mydata <- mutate(mydata, new_id = c(id))
  template <- bind_rows(template, mydata)
  }
  
