library(tidyverse)
source("pensionFuns.R")

pl <- planList() %>% 
  filter(state == 'Vermont')

data <- pullData("Vermont State Retirement System") %>% 
  selectedData() %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pct_change_uaal = uaal / uaal[1], pct_change_cont = emp_cont / emp_cont[1])

data2 <- pullData("Vermont State Teachers' Retirement System") %>% 
  selectedData() %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pct_change_uaal = uaal / uaal[1], pct_change_cont = emp_cont / emp_cont[1])

gsp <- readxl::read_xls("VTNGSP.xls") %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(pct_change_gsp = VTNGSP / VTNGSP[1],
         pct_change_exp = GeneralFund / GeneralFund[1])

data <- data %>% 
  left_join(gsp) %>% 
  left_join(data2, by = "year", suffix = c("_se", "_t")) %>% 
  mutate(tot_uaal = uaal_se + uaal_t,
         tot_cont = emp_cont_se + emp_cont_t) %>% 
  mutate(pct_change_tot_uaal = tot_uaal / tot_uaal[1], 
         pct_change_tot_cont = tot_cont / tot_cont[1])

p <- contGraph(data, 
               "pct_change_tot_uaal", 
               "pct_change_tot_cont", 
               "pct_change_exp", 
               "Percent Change from 2001 Value", 
               "UAAL", 
               "Employer Contributions", 
               "General Fund Expenditures")

p
ggsave("VTgraph.png")

