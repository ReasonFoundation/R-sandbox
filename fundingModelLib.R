####################################################################
# Description: This function selects the data used in the funding model
# Parameters:
#     wideData = a datasource in wide format
#     .date_var = column name for valuation date. Default: 'Actuarial Valuation Date For GASB Assumptions',
#     .aal_var = column name AAL. Default: 'Actuarial Accrued Liabilities Under GASB Standards',
#     .asset_var = column name for Actuarial Assets. Default: 'Actuarial Assets under GASB standards',
#     .adec_var = column name for ADEC. Default: 'Employer Annual Required Contribution',
#     .emp_cont_var = column name for employer contributions. Default: 'Employer Contributions',
#     .payroll_var = column name for payroll. Default: 'Covered Payroll'
# Usage: data <- selected_Data(wideData,
#                   .dateCol = 'Actuarial Valuation Date For GASB Assumptions',
#                   .aalCol = 'Actuarial Accrued Liabilities Under GASB Standards',
#                   .assetCol = 'Actuarial Assets under GASB standards',
#                   .ADECCol = 'Employer Annual Required Contribution',
#                   .empContCol = 'Employer Contributions',
#                   .payrollCol = 'Covered Payroll')
<<<<<<< HEAD

########### add total_pension_liabilty, payroll_growth_rate variables, drop final year if NAs
=======
>>>>>>> 4af61f55a2e9b1cf8634d3ffe510fdaf69913d2f


fundingData <- function(wide_data,
                        .date_var = "actuarial_valuation_date_for_gasb_assumptions",
<<<<<<< HEAD
                        .aal_var = "total_pension_liability",
                        #.aal_var = "actuarial_accrued_liabilities_under_gasb_standards",
=======
                        .aal_var = "actuarial_accrued_liabilities_under_gasb_standards",
>>>>>>> 4af61f55a2e9b1cf8634d3ffe510fdaf69913d2f
                        .asset_var = "actuarial_assets_under_gasb_standards",
                        .adec_var = "employer_annual_required_contribution",
                        .emp_cont_var = "employer_contributions",
                        .payroll_var = "covered_payroll",
<<<<<<< HEAD
                        .pgr_var = "payroll_growth_rate",
=======
>>>>>>> 4af61f55a2e9b1cf8634d3ffe510fdaf69913d2f
                        n = 35,
                        pgr = 2.75) {
  require(tidyverse)
  require(lubridate)
  require(janitor)
  
  date_var <- sym(.date_var)
  aal_var <- sym(.aal_var)
  asset_var <- sym(.asset_var)
  adec_var <- sym(.adec_var)
  emp_cont_var <- sym(.emp_cont_var)
  payroll_var <- sym(.payroll_var)
<<<<<<< HEAD
  pgr_var <- sym(.pgr_var)
=======
>>>>>>> 4af61f55a2e9b1cf8634d3ffe510fdaf69913d2f
  
  initial <- wide_data %>%
    mutate(
      date = !!date_var
    ) %>%
    mutate(
      year = year(excel_numeric_to_date(as.numeric(date))),
      valuation_date = excel_numeric_to_date(as.numeric(date))
      ) %>%
    select(
      year,
<<<<<<< HEAD
      valuation_date,
=======
      valuation_date = !!date_var,
>>>>>>> 4af61f55a2e9b1cf8634d3ffe510fdaf69913d2f
      actuarial_assets = !!asset_var,
      aal = !!aal_var,
      adec = !!adec_var,
      emp_cont = !!emp_cont_var,
      existing_payroll = !!payroll_var
    ) %>%
    mutate(
      uaal = as.numeric(aal) - as.numeric(actuarial_assets),
      funded_ratio = as.numeric(actuarial_assets) / as.numeric(aal),
      adec_contribution_rates = as.numeric(adec) / as.numeric(existing_payroll),
      actual_contribution_rates = as.numeric(emp_cont) / as.numeric(existing_payroll)
    ) %>% 
    top_n(1, year) %>%
    mutate(
      rehi_payroll = 104073,
      new_payroll = 0,
      payroll_total = existing_payroll + rehi_payroll
    ) 
  date_min <- initial$valuation_date[1]
  date_max <- date_min + years(n)
  all_dates <- seq(date_min, date_max, by = "year")
  new_df <- data.frame(list(valuation_date = all_dates))
  left_join(new_df, initial) %>%
    mutate(
      year = lag(year(valuation_date), default = first(year) - 1) + 1, 
      contribution_fy = year + 2
    ) %>%
    mutate(
      payroll_total = payrollGrowth(., pgr = pgr),
      payroll_rehi = payrollGrowth(., y = "rehi_payroll", pgr = pgr),
      payroll_existing = payrollExistingGrowth(.)
    ) %>%
    mutate(
      payroll_new = payroll_total - payroll_rehi - payroll_existing
    ) %>%
    select(-c(rehi_payroll, new_payroll, existing_payroll))
}

####################################################################
# Description: This function projects payroll at the payroll growth rate
# Parameters:
#     x: the dataframe containing a payroll column
#     y: the name of the column to project
#     pgr: the payroll growth rate
# Usage: mutate(payrollTotal = payrollGrowth(., pgr = input$pgr))

payrollGrowth <- function(x, y = "payroll_total", pgr) {
  output <- vector("double", nrow(x))
  output[1] <- as.numeric(x[[y]][1])
  for (i in 2:nrow(x)) {
    output[i] <- output[i - 1] * (1 + pgr / 100)
  }
  output
}

####################################################################
# Description: This function projects existing employee payroll
# Parameters:
#     x: the dataframe containing a payroll column
#     y: the name of the column to project
# Usage: mutate(payrollExisting = payrollExistingGrowth(.))

payrollExistingGrowth <- function(x, y = "existing_payroll") {
  output <- vector("double", nrow(x))
  output[1] <- as.numeric(x[[y]][1])
  for (i in 2:nrow(x)) {
    output[i] <-
      max(output[i - 1] * ((1 - 0.036) - (0.036 * 0.01) * max(0, x$year[i] - 2017)), 0)
  }
  output
}

####################################################################
# Description: This function creates an exportable data table of the funding model data
# Parameters:
#     data: the dataframe containing the funding model projections
# Usage: dataTableFM(data)

dataTableFM <- function(data) {
  require(DT)
  require(tidyverse)
  
  data <- data %>%
    rename(
      "Year" = year,
      "Valuation Date" = valuation_date,
      "Contribution Fiscal Year" = contribution_fy,
      "Total Payroll" = payroll_total,
      "Existing Employee Payroll" = payroll_existing,
      "Rehired Employee Payroll" = payroll_rehi,
      "New Employee Payroll" = payroll_new,
      "Actuarial Assets" = actuarial_assets,
      "Actuarial Accrued Liabilities" = aal,
      "Unfunded Actuarial Accrued Liabilities" = uaal,
      "Funded Ratio" = funded_ratio,
      "Actuarially Determined Employer Contribution" = adec,
      "Employer Contribution" = emp_cont
    )
  datatable(
    data,
    extensions = c("Buttons"),
    rownames = FALSE,
    editable = TRUE,
    options = list(
      bPaginate = FALSE,
      scrollX = T,
      scrollY = "600px",
      dom = "Brt",
      buttons = c("copy", "csv", "excel", "pdf", "print")
    )
  ) %>%
    formatCurrency(c(3:7, 11, 13:15)) %>%
    formatPercentage(8:10)
}
