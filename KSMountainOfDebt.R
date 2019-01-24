library(tidyverse)
source("pensionFuns.R")

pl <- planList()
KS <- pl %>% filter(state == 'Kansas')

allWide <- pullData(pl, plan_name = "Kansas Public Employees' Retirement System")

data <- modData(allWide) 

modGraph(data)

modTable(data)

# openxlsx::write.xlsx(allWide, file = "KSdata.xlsx")


