library(ggplot2)
library(tidyverse)
source("pensionFuns.R")

glGraph('data/Graph 1.csv')

ggsave("graph1.2.png", width = 9, height = 5.33)


