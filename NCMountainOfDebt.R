library(tidyverse)
library(ggthemes)
library(extrafont)
library(scales)

source("pensionFuns.R")

# allData <- pullData()
# allWide <- spreadData(allData,year, attribute_name, attribute_value) 

allWide <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx') %>% 
  select('Fiscal Year End', 'Actuarial Accrued Liability', 'Actuarial Value of Assets')

modPlot <- modGraph(wideData = allWide,
                    yearCol = 'Fiscal Year End',
                    aalCol = 'Actuarial Accrued Liability',
                    assetCol = 'Actuarial Value of Assets')
modPlot


