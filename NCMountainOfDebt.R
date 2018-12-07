if (!require("tidyverse")) devtools::install_github("rstudio/tidyverse")
library(xts)
library(lubridate)
options(viewer = NULL)
source("pensionFuns.R")


allWide <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx') %>% 
  select('Fiscal Year End', 'Actuarial Accrued Liability', 'Actuarial Value of Assets') %>% 
  drop_na()

data <- modData(allWide,
                yearCol = 'Fiscal Year End',
                aalCol = 'Actuarial Accrued Liability',
                assetCol = 'Actuarial Value of Assets',
                base = 1)

modGraph(data)

modTable(data)

