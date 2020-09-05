cs20192020 <- read.xlsx("./cs_2019-2020Jul/cs_20192020.xlsx")
cs20192020$month <- factor(cs20192020$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
cs20192020$year <- factor(cs20192020$year)
cs20192020$citizen_science <- as.numeric(cs20192020$citizen_science)

tiff("cs_20192020.png", width = 1200, height = 800, res = 165)
ggplot(cs20192020, aes(x=month, y=citizen_science, group = year, color = year )) +
  geom_line(stat = "identity", size =1) + #change stat to "identity/smooth" for straight line or smooth
  geom_point()+
  scale_y_continuous(breaks = seq(0,800,100))+
  geom_text(aes(label = citizen_science), size= 3,position = position_dodge(width = 0), 
            vjust = -1) +
  ylim(0,650) +
  theme_bw() + 
  labs(title = "Grafik Citizen Science PT ANJ 2019 - 2020", 
       subtitle = "Total akumulasi citizen science 2019 - 2020: 899 orang")+
  theme(axis.text = element_text(face = "bold"))
dev.off()
