library(ggplot2)
library(tidyverse)
library(scales)
source("pensionFuns.R")

# graph <- read_csv('data/Graph 3.csv', col_names = FALSE) %>% 
#   `colnames<-`(c('year','percentage1', 'percentage2')) %>% 
#   mutate_all(as.numeric)

allData <- pullData("Kansas Public Employees' Retirement System")
allWide <- spreadData(allData) 
data <- selected_Data(allWide)

graph <- data %>%
  select(year, 
         `ADEC Contribution Rates`,
         `Actual Contribution Rates (Statutory)`) %>%
  mutate_all(funs(as.numeric)) %>%
  gather(key = contribution, value = amount, -year)

Colors <-  c("ADEC Contribution Rates" = "#FF6633",
             "Actual Contribution Rates (Statutory)" = "#3300FF")

p <- ggplot(graph, aes(x = year)) +
  geom_line(aes(y = amount * 100, color = contribution), size = 2) +
  scale_fill_manual(values = lineColors) +
  geom_hline(yintercept = 0, color = "black") +
  
  scale_y_continuous(breaks = pretty_breaks(10), 
                     labels = function(b) { 
                       paste0(round(b, 0), "%")
                       }) +
  
  scale_x_continuous(breaks = pretty_breaks(10)) + 
  
  ylab('Text graph 3 here') +
  scale_color_discrete(labels = c("Orange line means this", "Blue line means this")) +
  
  reasonTheme +
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.5, 1),
        legend.title = element_blank())
p


