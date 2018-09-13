rm(list = ls())
#clear everything
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
#all those libraries are needed for this and other graphs.
graph1 <- read.xlsx("/Users/anilniraula/Downloads/Graph_1.xls",1, header=FALSE)
#View(graph1)
#copying data from file to graph 1
graph1 <- t(graph1)
#makes rows into columns, transpose of the matrix
#if the data is organized properly in the first place, this step is not needed.
colnames(graph1) <- c('labels','values')
#because data is just numbers and we don't know what that means, we need to label that.
graph1 <- data.frame(graph1)
#we need to change it for ggplot2, this particular library works only with this type of data
graph1$values <- as.numeric(as.character(graph1$values))
#converting it into numeric, makes it numbers, it was string before
#sometimes you do it even if data is correctly numeric
graph1$labels <- factor(graph1$labels,levels=graph1$labels)
#order the labels in the correct format, otherwise it makes it alhabetical format
c8 <-  c(rep("#CC0000",5), rep("#669900",2), "#FF6633")
#colors, different colors for different bars, it should be same color as in Excel
#if it's not the same color as excel, we need to see what palet options they have
png(file="graph1.png",width=4000,height=3500,res=450)
#setting the dimensions of the graph
#now all below just the specifics of the graph that I had in the style guide
#geom_bar is used for bar charts
ggplot(graph1, aes(x=graph1$labels, y=graph1$values)) + 
  geom_bar(stat="identity",width=0.75,fill = c8,color="#000000") +
  ylab('Text 1 goes here') +
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(color="black",size=9, angle=0, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face='bold',size=14),
        axis.text.y = element_text(color="black", size=10),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(min(graph1$values), max(graph1$values)+1, by = 2),1),limits = c(min(graph1$values), max(graph1$values)+1), expand = c(0,0), labels = dollar_format(prefix = "$"))+ 
  geom_hline(yintercept = 0, color = "black")
dev.off()
