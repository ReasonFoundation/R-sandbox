####################################################################
# Description: This function selects the data used in the funding model
# Parameters:
#     wideData = a datasource in wide format
#     dateCol = column name for valuation date. Default: 'Actuarial Valuation Date For GASB Assumptions',
#     aalCol = column name AAL. Default: 'Actuarial Accrued Liabilities Under GASB Standards',
#     assetCol = column name for Actuarial Assets. Default: 'Actuarial Assets under GASB standards',
#     ADECCol = column name for ADEC. Default: 'Employer Annual Required Contribution',
#     empContCol = column name for employer contributions. Default: 'Employer Contributions',
#     payrollCol = column name for payroll. Default: 'Covered Payroll'
# Usage: data <- selected_Data(wideData,
#                   dateCol = 'Actuarial Valuation Date For GASB Assumptions',
#                   aalCol = 'Actuarial Accrued Liabilities Under GASB Standards',
#                   assetCol = 'Actuarial Assets under GASB standards',
#                   ADECCol = 'Employer Annual Required Contribution',
#                   empContCol = 'Employer Contributions',
#                   payrollCol = 'Covered Payroll')


fundingData <- function(wide_data,
                        date_col = "actuarial_valuation_date_for_gasb_assumptions",
                        aal_col = "actuarial_accrued_liabilities_under_gasb_standards",
                        asset_col = "actuarial_assets_under_gasb_standards",
                        adec_col = "employer_annual_required_contribution",
                        emp_cont_col = "employer_contributions",
                        payroll_col = "covered_payroll") {
  require(tidyverse)
  require(lubridate)
  require(janitor)
  
  wide_data %>%
    mutate(
      year = year(excel_numeric_to_date(as.numeric(actuarial_valuation_date_for_gasb_assumptions))),
      valuation_date = excel_numeric_to_date(as.numeric(actuarial_valuation_date_for_gasb_assumptions))
    ) %>%
    select(
      year,
      valuation_date,
      actuarial_assets = asset_col,
      aal = aal_col,
      adec = adec_col,
      emp_cont = emp_cont_col,
      payroll = payroll_col
    ) %>%
    mutate(
      uaal = as.numeric(aal) - as.numeric(actuarial_assets),
      funded_ratio = as.numeric(actuarial_assets) / as.numeric(aal),
      adec_contribution_rates = as.numeric(adec) / as.numeric(payroll),
      actual_contribution_rates = as.numeric(emp_cont) / as.numeric(payroll)
    ) %>%
    drop_na()
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
      "Actuaially Determined Employer Contribution" = adec,
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
    formatCurrency(c(3:5, 7:8, 11)) %>%
    formatPercentage(6, 9:10)
}
