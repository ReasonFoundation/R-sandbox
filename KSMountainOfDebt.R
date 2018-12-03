library(tidyverse)
library(DT)
library(readxl)
library(openxlsx)

source("pensionFuns.R")

pl <- planList()
KS <- pl %>% filter(state == 'Kansas')

allData <- pullData("Kansas Public Employees' Retirement System")
allWide <- spreadData(allData,year, attribute_name, attribute_value) 

data <- modData(allWide) 
data2 <- data %>% 
  mutate(actuarialAssets = as.numeric(actuarialAssets)*1000, AAL = as.numeric(AAL)*1000, UAAL = UAAL*1000)

modGraph(data, 1000)

modTable(data2)

openxlsx::write.xlsx(allWide, file = "KSdata.xlsx")
