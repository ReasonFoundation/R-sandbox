rm(list = ls())
library(ggplot2)
library(ggthemes)
library(xlsx)
library(extrafont)
library(plyr)
library(scales)
graph3 <- read.xlsx("/Users/anilniraula/Downloads/Graph_3.xls",1, header=FALSE)
#View(graph3)
colnames(graph3) <- c('years','percentage1','percentage2')
graph3$years <- as.numeric(as.character(graph3$years))
graph3$percentage1 <- as.numeric((graph3$percentage1))
graph3$percentage2 <- as.numeric((graph3$percentage2))
png(file="graph3.png",width=30,height=20, units = "cm", res=450)
  ggplot(graph3,aes(x=graph3$years)) +
    geom_line(aes( y = graph3$percentage1*100,color='#FF6633'),size=1.5) +
    ylab('Text graph 3 here') +
    theme(axis.line.y = element_line(color="black"),
          axis.line.x = element_line(color="black", size=0.5),
          axis.text.x = element_text(size=11,angle=90,hjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face='bold',size=14),
          axis.text.y = element_text(size=10),
          panel.background = element_blank(),
          legend.key=element_blank())+
    scale_y_continuous(breaks = round(seq(-30,70, by = 10),1),limits = c(-10,70), expand = c(0,0), labels = function(b) { paste0(round(b, 0), "%")})+
    scale_x_continuous(breaks = round(seq(min(graph3$years), max(graph3$years), by = 10),1))+ 
    geom_hline(yintercept = -10, color = "black")+
    geom_line(aes(y=graph3$percentage2*100,color='#3300FF'),size=1.5)+
    scale_color_discrete( labels = c("Orange line means this", "Blue line means this"))+
    theme(legend.justification = c(1, 1), legend.position = c(0.5, 1),legend.title=element_blank())
dev.off()
#colors()
#help(png)
