#before running the cose you should set the appropriate working directory
#use command "getwd()" to find working directory 
#use command "setwd()" to set a new working directory
#it should look something like "setwd("/Users/jensidorova/R")"
#then you can use command Run to run each line of code one by one
rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(scales)
graph2 <- read.csv('Graph 2_v3.csv',header=F)
colnames(graph2) <- c('years','percentage','dollar1','dollar2')
graph2$years <- as.numeric(as.character(graph2$years))

#extrapolate data points to "smooth out" the area chart 
extrapo <- approx(graph2$years, graph2$dollar1, n = 10000)
graph21 <- data.frame(years = extrapo$x, dollar1 = extrapo$y)

#create a "negative-positive" column for fill aesthetic
graph21$sign[graph21$dollar1 >= 0] = "positive"
graph21$sign[graph21$dollar1 < 0] = "negative"

#for custom areas you have to write the number
png(file="graph2_v3.png",width=4000,height=3500,res=450)

#geom hline is for the black line that separates the green and red
ggplot(graph21, aes(years, dollar1)) +
  geom_area(aes(fill = sign), show.legend = FALSE) +
  scale_fill_manual(values = c("#669900","#CC0000")) +
  geom_line() +
  annotate("label", label="whatever 2", x=1955, y=4, size=5, label.size=.275, fontface="bold", colour="#669900") +
  annotate("label", label="ABC\nABD", x=1840, y=13, size=5, label.size=.275, fontface="bold", colour="#3300FF") +
  annotate("label", label="ACB\nACC", x=1930, y=22, size=5, label.size=.275, fontface="bold", colour="#CC0000") +
  annotate("label", label="whatever 1", x=1815, y=7, size=5, label.size=.275, fontface="bold", colour="#3300FF") +
  ylab('Text 2 goes here') +
  theme(axis.line.y = element_line(color="black"),
        axis.text.x = element_text(face="bold",size=14,hjust=1,angle=90),
        axis.title.x = element_blank(),
        axis.title.y.left =element_text(face='bold',size=14),
        axis.text.y.left = element_text(face="bold",size=14),
        axis.title.y.right =element_text(face='bold',size=14),
        axis.text.y.right = element_text(face="bold",size=14),
        panel.background = element_blank())+
  scale_y_continuous(breaks = round(seq(min(graph2$dollar1),max(graph2$dollar1)+1, by = 3),1),
                     labels = dollar_format(prefix   = "$"), 
                     sec.axis = sec_axis(~ . * 5,
                                         breaks = seq(-25,135,15), 
                                         name = "Text 3 goes here",
                                         labels = function(b) { paste0    (round(b, 0), "%")})) +
  scale_x_continuous(breaks = round(seq(min(graph2$years), max(graph2$years), by = 10),1)) +
  geom_hline(yintercept = 0, color = "black") + 
  geom_line(data = graph2, aes(x = years, y=percentage*20),color='#3300FF',size=4)
dev.off()
