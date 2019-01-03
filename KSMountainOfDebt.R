library(tidyverse)
source("pensionFuns.R")

pl <- planList()
KS <- pl %>% filter(state == 'Kansas')

allData <- pullData("Kansas Public Employees' Retirement System")
allWide <- spreadData(allData) 

data <- modData(allWide) 

modGraph(data)

modTable(data)

# openxlsx::write.xlsx(allWide, file = "KSdata.xlsx")


