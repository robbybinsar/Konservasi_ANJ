library(openxlsx)
library(ggplot2)

f20192020 <- read.xlsx("./cs_2019-2020Jul/form_pendaki_grafik.xlsx")
f20192020$month <- factor(f20192020$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
f20192020$year <- factor(f20192020$year)
f20192020$form <- as.numeric(f20192020$form)

tiff("form_20192020.png", width = 1200, height = 950, res = 165)
ggplot(f20192020, aes(x=month, y=form, group = year, color = year )) +
  geom_line(stat = "identity", size =1) + #change stat to "identity/smooth" for straight line or smooth
  geom_point()+ stat_smooth(method = "lm", se = F, lty = "dashed", lwd = 0.5) +
  scale_y_continuous(breaks = seq(0,2300,100), limits = c(0,2300))+
  geom_text(aes(label = form), size= 3,position = position_dodge(width = 0), 
            vjust = -1) +
  theme_bw() + ylab("Form Pendaki")+
  labs(title = "Grafik Engagement Form PENDAKI PT ANJ 2019 - 2020", 
       subtitle = "Total of form submitted each month from 2019-2020")+
  theme(axis.text = element_text(face = "bold"))
dev.off()
