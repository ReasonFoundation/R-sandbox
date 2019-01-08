require(xts)
require(tidyverse)
require(lubridate)
options(viewer = NULL)
source("pensionFuns.R")

installRequiredPackages()
allWide <- loadData('NorthCarolina_PensionDatabase_TSERS.xlsx') %>% 
  select('fiscal_year_end', 'actuarial_accrued_liability', 'actuarial_value_of_assets') %>% 
  drop_na()

data <- modData(allWide,
                year_col = 'fiscal_year_end',
                aal_col = 'actuarial_accrued_liability',
                asset_col = 'actuarial_value_of_assets',
                base = 1)

modGraph(data)

modTable(data)

