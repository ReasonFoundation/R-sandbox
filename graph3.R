#before running the cose you should set the appropriate working directory
#use command "getwd()" to find working directory 
#use command "setwd()" to set a new working directory
#it should look something like "setwd("/Users/jensidorova/R")"
#then you can use command Run to run each line of code one by one
rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
graph3 <- read.csv('Graph 3.csv',header=F)
colnames(graph3) <- c('years','percentage1','percentage2')
graph3$years <- as.numeric(as.character(graph3$years))
graph3$percentage1 <- as.numeric((graph3$percentage1))
graph3$percentage2 <- as.numeric((graph3$percentage2))
png(file="graph3.png",width=4000,height=3500,res=450)
ggplot(graph3,aes(x=graph3$years)) +
  geom_line(aes( y = graph3$percentage1*100,color='#FF6633'),size=2) +
  ylab('Text graph 3 here') +
  theme(axis.line.y = element_line(color="black"),
        axis.text.x = element_text(face="bold",size=14,hjust=0,angle=90),
        axis.title.x = element_blank(),
        axis.title.y =element_text(face='bold',size=14),
        axis.text.y = element_text(face="bold",size=14),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(-30,70, by = 10),1),labels = function(b) { paste0(round(b, 0), "%")})+
  scale_x_continuous(breaks = round(seq(min(graph3$years), max(graph3$years), by = 10),1))+ 
  geom_hline(yintercept = 0, color = "black")+
  geom_line(aes(y=graph3$percentage2*100,color='#3300FF'),size=2)+
  scale_color_discrete( labels = c("Orange line means this", "Blue line means this"))+
  theme(legend.justification = c(1, 1), legend.position = c(0.5, 1),legend.title=element_blank())
dev.off()

