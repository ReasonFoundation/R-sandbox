
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
