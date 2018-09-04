#before running the cose you should set the appropriate working directory
#use command "getwd()" to find working directory 
#use command "setwd()" to set a new working directory
#it should look something like "setwd("/Users/jensidorova/R")"
#then you can use command Run to run each line of code one by one
rm(list = ls())
#clear everything
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
#all those libraries are needed for this and other graphs.
graph1 <- read.csv('Graph 1.csv',header=F)
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
c8 <-  c(rep("#669900",2),rep("#CC0000",5), "#FF6633")
#colors, different colors for different bars, it should be same color as in Excel
#if it's not the same color as excel, we need to see what palet options they have
graph1 <- cbind(graph1,c8)
#combining c8 to graph 1.
png(file="graph1.png",width=4000,height=3500,res=450)
#setting the dimensions of the graph
#now all below just the specifics of the graph that I had in the style guide
#geom_bar is used for bar charts
ggplot(graph1, aes(x=graph1$labels, y=graph1$values)) + 
  geom_bar(stat="identity",width=0.75,fill = c8,color="#000000") +
  ylab('Text 1 goes here') +
  theme(axis.ticks.x = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(face="bold",size=14, angle=90,hjust=1),
        axis.title.x = element_blank(),
        axis.title.y=element_text(face='bold',size=14),
        axis.text.y = element_text(face="bold",size=14),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(min(graph1$values), max(graph1$values)+1, by = 2),1),labels = dollar_format(prefix = "$"))+ 
  geom_hline(yintercept = 0, color = "black")
dev.off()

