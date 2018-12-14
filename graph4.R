rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
graph4 <- read.csv('graph4.csv',header=F)
colnames(graph4) <- c('labels','values1','values2','values3')
graph4 <- data.frame(graph4)
graph4$values1 <- as.numeric(as.character(graph4$values1))
graph4$values2 <- as.numeric(as.character(graph4$values2))
graph4$values3 <- as.numeric(as.character(graph4$values3))
values = as.vector( rbind(graph4$values1,graph4$values2))
label <- rep(graph4$labels,each=2)
graph <- data.frame(label,values)
legend <- rep(c('Green line means this','Purple line means this'),length(graph4$labels))
#png(file="graph4.png",width=4000,height=3500,res=450)
xbreaks <- round(seq(min(graph4$labels), max(graph4$labels), by = 1),1)
xlabels <- as.character(xbreaks)
xlabels[!((xbreaks-1)%%3==0)] <- ''
ggplot(graph, aes(fill=legend,x=label, y=values*100)) + 
  geom_bar(stat="identity",width=0.75) +
  geom_line(aes(y=rep(graph4$values3*100,each=2),color="Yellow line means this"),size=2)+
  scale_fill_manual(values = c(
    'Green line means this' = 'green',
    'Purple line means this' = 'purple'))+
  scale_color_manual(values=c("Yellow line means this" = 'yellow'))+
  ylab('Text 1 goes here') +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(axis.line.y = element_line(color="black"),
        axis.text.x = element_text(face="bold",size=14,family='Calibri'),
        legend.text=element_text(size=10),
        axis.title.x = element_blank(),
        axis.title.y=element_text(face='bold',size=14,family='Calibri'),
        axis.text.y = element_text(face="bold",size=14,family='Calibri'),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(0,40, by = 4),1),labels = function(b) { paste0(round(b, 0), "%")})+
  scale_x_continuous(breaks = xbreaks,labels=xlabels)+
  geom_hline(yintercept = 0, color = "black")
#dev.off()