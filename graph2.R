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
graph2 <- read.csv('Graph 2.csv',header=F)
colnames(graph2) <- c('years','percentage','dollar1','dollar2')
graph2$years <- as.numeric(as.character(graph2$years))
#select the years column - just selects the column which is named years.
graph2$percentage <- as.numeric((graph2$percentage))
graph2$dollar1 <- as.numeric(as.character(graph2$dollar1))
graph2$dollar2 <- as.numeric(as.character(graph2$dollar2))
cat.rle = rle(graph2$dollar1 <0)
#picks up the negative values
graph2$group = rep.int(1:length(cat.rle$lengths), times=cat.rle$lengths)
#together with the above makes a group column
png(file="graph2.png",width=4000,height=3500,res=450)
#ribbon is for the mountain type of plots; geom area is for green
#geom line is for the black line that separates the green and red
#aes - the location (year, dollar)
#for custom areas you have to write the number
ggplot(graph2,aes(x=graph2$years,y=graph2$dollar1)) +
  geom_ribbon(aes(ymin = graph2$dollar1, ymax = 0), fill = "#CC0000")+
  geom_area(aes(y = ifelse(group == "1", graph2$dollar1, 0)), fill = "#669900")+
  geom_line(data = graph2, aes(x = graph2$years, y = graph2$dollar1)) +
  geom_label(aes(1955, 4,size=14), label="whatever 2",fontface="bold",colour = "#669900",label.size=.275,show.legend = FALSE) +
  geom_label(aes(1840, 13,size=14), label="ABC\nABD",fontface="bold",colour = "#3300FF",show.legend = FALSE,label.size=.275)+
  geom_label(aes(1930, 22,size=14), label="ACB\nACC",fontface="bold",colour = "#CC0000",show.legend = FALSE,label.size=.275)+
  geom_label(aes(1815, 7,size=14), label="whatever 1", fontface="bold",colour = "#3300FF",show.legend = FALSE,label.size=.275)+
  ylab('Text 2 goes here') +
  theme(axis.line.y = element_line(color="black"),
      axis.text.x = element_text(face="bold",size=14,hjust=1,angle=90),
      axis.title.x = element_blank(),
      axis.title.y.left =element_text(face='bold',size=14),
      axis.text.y.left = element_text(face="bold",size=14),
      axis.title.y.right =element_text(face='bold',size=14),
      axis.text.y.right = element_text(face="bold",size=14),
      panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(min(graph2$dollar1), max(graph2$dollar1)+1, by = 3),1),labels = dollar_format(prefix   = "$"), sec.axis = sec_axis(~ . * 5,breaks = seq(-25,135,15), name = "Text 3 goes here",labels = function(b) { paste0    (round(b, 0), "%")}))+
  scale_x_continuous(breaks = round(seq(min(graph2$years), max(graph2$years), by = 10),1))+ 
  geom_hline(yintercept = 0, color = "black")+
  geom_line(aes(y=percentage*20),color='#3300FF',size=4)
dev.off()

