library(openxlsx)
library(dplyr)

output <- read.xlsx("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/data_jml_spesies_ANJ.xlsx", 
                  sheet = "January", cols = c(1,3))

sheets <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
for(i in sheets) {
  data <- read.xlsx("C:/Users/robby/Documents/My R/Konservasi_ANJ/Diversity_measures/indeks_kehati/data_jml_spesies_ANJ.xlsx", 
            sheet = i, cols = c(1,3))
  output <- merge(output, data, all = T)
}
output[is.na(output)] <- 0
result <- output %>% filter_at(vars(Jan:Dec), any_vars(.>0))
