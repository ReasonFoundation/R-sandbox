library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(tidyverse)
source("pensionFuns.R")

graph5 <- read_csv("graph5.csv", col_names = F) %>%
  `colnames<-`(c("labels", "percentage1", "percentage2", "percentage3", "percentage4")) %>%
  mutate_if(is.character, as.numeric) %>%
  gather(keys, values, -labels) %>%
  drop_na()

p <- ggplot(graph5, aes(x = labels)) +
  geom_line(aes(y = values * 100, color = keys), size = 2) +
  geom_hline(yintercept = 0, color = "black") +

  # geom_label(aes(13, 0, size = 10),
  #   label = "Text goes here\nA\nB\nC\nD",
  #   fontface = "bold",
  #   family = "Calibri",
  #   show.legend = FALSE,
  #   label.size = 0
  # ) +
  reasonTheme +
  scale_y_continuous(
    breaks = pretty_breaks(10),
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  # annotate("label", x = 9, y = 15, label = "some text box with an\n arrow that I can move\n around when I want") +
  # annotate("segment", x = 9, xend = graph5$labels[11], y = 14, yend = graph5$percentage4[11] * 100 + 0.25, color = "pink", arrow = arrow()) +
  # scale_color_manual(values = c("TEXT 1" = "purple", "TEXT 2" = "pink", "TEXT 3" = "navyblue", "TEXT TEXT TEXT TEXT TEXT TEXT" = "lightblue")) +
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.35, 0.25), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 10)
        )
p
