data <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx') %>% 
  select('fiscal_year_end', 'actuarial_accrued_liability', 'actuarial_value_of_assets') %>% 
  drop_na() %>% 
  select(year = fiscal_year_end, aal = actuarial_accrued_liability, actuarial_assets = actuarial_value_of_assets) %>% 
  mutate(
    uaal = as.numeric(aal) - as.numeric(actuarial_assets),
    funded_ratio = as.numeric(actuarial_assets) / as.numeric(aal)
    )

NC_debt_plot <- debtPlot(data)
NC_debt_plot
