rm(list = ls())
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(tidyverse)
source("pensionFuns.R")

graph4 <- read_csv("graph4.csv", col_names = F) %>%
  `colnames<-`(c("label", "values1", "values2", "values3")) %>%
  mutate_if(is.character, as.numeric) %>%
  gather(key = key, value = value, -label)


legend <- rep(c("Green line means this", "Purple line means this"), length(graph4$label))

p <- ggplot(graph4, aes(fill = key, x = label, y = value * 100)) +
  geom_col(width = 0.75) +
  # geom_line(aes(y = rep(graph4$values3 * 100, each = 2), color = "Yellow line means this"), size = 2) +
  # scale_fill_manual(values = c(
  #   "Green line means this" = "green",
  #   "Purple line means this" = "purple"
  # )) +
 # scale_color_manual(values = c("Yellow line means this" = "yellow")) +
  ylab("Text 1 goes here") +
  reasonTheme +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(
    breaks = pretty_breaks(10),
    labels = function(b) {
      paste0(round(b, 0), "%")
    }
  ) +
  scale_x_continuous(breaks = pretty_breaks()) +
  geom_hline(yintercept = 0, color = "black")
p
