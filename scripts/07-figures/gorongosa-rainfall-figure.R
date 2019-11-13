library(ggplot2)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

rainfall <- read.csv("Data/Chitengo_rainfall_forR.csv")

rainfall$MonthText <- reorder(rainfall$MonthText, rainfall$MonthSeq)

pdf('Figures/rainfall.pdf', width = 6, height = 4)
ggplot(data = rainfall, aes(x = MonthText, y = Rainfall, fill = Season)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  xlab('Month') +
  ylab("Rainfall (mm)")
dev.off()
