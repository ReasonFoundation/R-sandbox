rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
graph5 <- read.csv('graph5.csv',header=F)
colnames(graph5) <- c('labels','percentage1','percentage2','percentage3','percentage4')
graph5$labels <- as.numeric(as.character(graph5$labels))
graph5$percentage1 <- as.numeric((graph5$percentage1))
graph5$percentage2 <- as.numeric((graph5$percentage2))
graph5$percentage3 <- as.numeric((graph5$percentage3))
graph5$percentage4 <- as.numeric((graph5$percentage4))
xbreaks <- round(seq(min(graph5$labels), max(graph5$labels), by = 2),1)
xlabels <- as.character(xbreaks)
xlabels[!((xbreaks-1)%%2==0)] <- ''
png(file="graph5.png",width=4000,height=3500,res=450)
ggplot(graph5,aes(x=graph5$labels)) +
  geom_line(aes( y = graph5$percentage1*100,color='TEXT 1'),size=2) +
  geom_label(aes(13, 0,size=10), label="Text goes here\nA\nB\nC\nD",fontface="bold",family='Calibri',show.legend = FALSE,label.size = 0)+
  theme(axis.line.y = element_line(color="black"),
        axis.text.x = element_text(face="bold",size=14,family='Calibri'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(face="bold",size=14,family='Calibri'),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(-3,20, by = 3),1),labels = function(b) { paste0(round(b, 0), "%")})+
  scale_x_continuous(breaks = xbreaks,labels=xlabels)+ 
  geom_hline(yintercept = -3, color = "black")+
  geom_line(aes(y=graph5$percentage2*100,color='TEXT 2'),size=2)+
  geom_line(aes(y=graph5$percentage3*100,color='TEXT 3'),size=2)+
  geom_line(aes(y=graph5$percentage4*100,color='TEXT TEXT TEXT TEXT TEXT TEXT'),size=2)+
  annotate("label", x=9, y=15, label = "some text box with an\n arrow that I can move\n around when I want")+
  annotate("segment", x=9, xend = graph5$labels[11], y= 14, yend= graph5$percentage4[11]*100+0.25,color='pink', arrow = arrow())+
  scale_color_manual(values=c("TEXT 1" = 'purple',"TEXT 2" = 'pink',"TEXT 3" = 'navyblue',"TEXT TEXT TEXT TEXT TEXT TEXT" = 'lightblue'))+
  theme( legend.justification = c(1, 1),legend.position = c(0.35 ,0.25),legend.title=element_blank(),legend.text=element_text(size=10))
dev.off()