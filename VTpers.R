library(tidyverse)
source("pensionFuns.R")

pl <- planList() %>% 
  filter(state == 'Vermont')
data <- pullData("Vermont State Retirement System") %>% 
  spreadData() %>% 
  selected_Data() %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pctChangeUAAL = UAAL / UAAL[1], pctChangeCont = empCont / empCont[1])

gsp <- readxl::read_xls("VTNGSP.xls") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pctChangeGSP = VTNGSP / VTNGSP[1],
         pctChangeExp = GeneralFund / GeneralFund[1])

data <- data %>% 
  left_join(gsp)

p <- contGraph(data, 
               "pctChangeUAAL", 
               "pctChangeCont", 
               "pctChangeExp", 
               "Percent Change from 2001 Value", 
               "UAAL", 
               "Employer Contributions", 
               "General Fund Expenditures")

p
