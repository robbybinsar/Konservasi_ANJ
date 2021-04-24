library(openxlsx)
library(ggplot2)

cs20192020 <- read.xlsx("./cs_2019-2020Jul/cs_20192020.xlsx")
cs20192020$month <- factor(cs20192020$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
cs20192020$year <- factor(cs20192020$year)
cs20192020$citizen_science <- as.numeric(cs20192020$citizen_science)

tiff("cs_20192020.png", width = 1200, height = 900, res = 165)
ggplot(cs20192020, aes(x=month, y=citizen_science, group = year, color = year )) +
  geom_line(stat = "identity", size =1) + #change stat to "identity/smooth" for straight line or smooth
  geom_point()+
  scale_y_continuous(breaks = seq(0,1000,100), limits = c(0,800))+
  geom_text(aes(label = citizen_science), size= 3,position = position_dodge(width = 0), 
            vjust = -1) +
  theme_bw() + ylab("Observer (Citizen Science)")+
  labs(title = "Grafik Citizen Science PT ANJ 2019 - 2020", 
       subtitle = "Cumulative count of Citizen Science participated in 2019 - 2020: 1053 observers")+
  theme(axis.text = element_text(face = "bold"))
dev.off()
