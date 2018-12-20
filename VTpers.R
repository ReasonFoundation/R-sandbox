library(tidyverse)
source("pensionFuns.R")

pl <- planList() %>% 
  filter(state == 'Vermont')

data <- pullData("Vermont State Retirement System") %>% 
  spreadData() # %>% 
  # selected_Data() %>% 
  # mutate_if(is.character, as.numeric) %>% 
  # mutate(pctChangeUAAL = UAAL / UAAL[1], pctChangeCont = empCont / empCont[1])

data2 <- pullData("Vermont State Teachers' Retirement System") %>% 
  spreadData() %>% 
  selected_Data() %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pctChangeUAAL = UAAL / UAAL[1], pctChangeCont = empCont / empCont[1])

gsp <- readxl::read_xls("VTNGSP.xls") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pctChangeGSP = VTNGSP / VTNGSP[1],
         pctChangeExp = GeneralFund / GeneralFund[1])

data <- data %>% 
  left_join(gsp) %>% 
  left_join(data2, by = "year", suffix = c("SE", "T")) %>% 
  mutate(totUAAL = UAALSE + UAALT,
         totCont = empContSE + empContT) %>% 
  mutate(pctChangeTotUAAL = totUAAL / totUAAL[1], 
         pctChangeTotCont = totCont / totCont[1])

p <- contGraph(data, 
               "pctChangeTotUAAL", 
               "pctChangeTotCont", 
               "pctChangeExp", 
               "Percent Change from 2001 Value", 
               "UAAL", 
               "Employer Contributions", 
               "General Fund Expenditures")

p
ggsave("VTgraph.png")

glGraph("Graph 1.csv")
