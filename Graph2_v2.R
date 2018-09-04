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

graph2$slope1 <- c(NA, with(graph2, diff(dollar1)/diff(years)))
graph2$intcpt1 <- with(graph2, dollar1 - slope1 * years)
graph2$years2 <- with(graph2, years - dollar1/slope1)
graph2$dollar3 <- graph2$dollar2
graph2 <- graph2[,-5:-6]
graph2[which(graph2$years2 > graph2$years), "years2"] <- NA
graph2[which(graph2$years2 < (graph2$years-diff(graph2$years))), "years2"] <- NA
graph2$segment <- findInterval(graph2$years, sort(c(graph2$years2[which(!is.na(graph2$years2))])))
graph2$years3 <- c(tail(graph2$years2, -1), NA)


graph21 <- graph2[, c(1, 3:4, 7)]
graph22 <- graph2[!is.na(graph2$years2), c(5, 4, 6, 7)]
graph23 <- graph2[!is.na(graph2$years3), c(8, 4, 6, 7)]
names(graph22) <- names(graph21)
names(graph23) <- names(graph21)
combo <- rbind(graph21, graph22)
combo <- rbind(combo, graph23)
combo <- combo[is.finite(combo$dollar1), ]
combo <- combo[order(combo$years), ]

#ribbon is for the mountain type of plots; geom area is for green
#geom line is for the black line that separates the green and red
#aes - the location (year, dollar)
#for custom areas you have to write the number
png(file="graph4.png",width=4000,height=3500,res=450)

ggplot(combo, aes(years, ymin = dollar1, ymax = dollar2)) +
  geom_ribbon(aes(fill = factor(segment))) +
  scale_fill_manual(values = c("#669900","#CC0000")) +
  geom_line(data = combo, aes(x = years, y = dollar1)) +
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
  scale_y_continuous(breaks = round(seq(min(combo$dollar1),max(combo$dollar1)+1, by = 3),1),
                     labels = dollar_format(prefix   = "$"), 
                     sec.axis = sec_axis(~ . * 5,
                                         breaks = seq(-25,135,15), 
                                         name = "Text 3 goes here",
                                         labels = function(b) { paste0    (round(b, 0), "%")})) +
  scale_x_continuous(breaks = round(seq(min(combo$years), max(combo$years), by = 10),1)) +
  geom_hline(yintercept = 0, color = "black")
  #geom_line(aes(y=percentage*20),color='#3300FF',size=4)
dev.off()
