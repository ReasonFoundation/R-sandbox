# This script contains functions used to load pension plan data either from Reason's database or
# from an excel file.
# Author: Andrew Abbott
# Date: 12/11/2018

# Color Scheme
# All images should use web safe colors — this gives us a range of orange and blue
# colors that fit with Reason’s branding, as well as reds and greens that we can use to
# indicate positive or negative data patterns. In general, it is best to choose from the
# following palette of colors:
#   • Orange (FF6633): Orange that matches with Reason’s logo
#   • Yellow (FFCC33)
#   • Dark Grey/Blue (333333)
#   • Light Blue (3399CC)
#   • Royal Blue (3366CC)
#   • Grey (6699CC)
# Use the orange and yellow colors to emphasize attention to lines or areas of
# interest.
# For graphs that require a clear positive/negative emphasis, you can use the
# following colors:
#   • Green (669933): for positive
#   • Red (990000): for negative

# This function installs the required packages.
# Usage: installRequiredPackages()

installRequiredPackages <- function() {
  packages_needed <- c('tidyverse', 'RPostgres', 'ggplot2', 'httr', 'ggthemes', 'extrafont', 'scales', 'DT', 'lubridate')
  installed <- installed.packages()
  sapply(packages_needed, function(p)
    if(!p %in% installed[,1]){
      install.packages(p)
      }
    )
}

# This function grabs a list of the plans with their state from the Reason database.
# Use this to find the exact plan names that are used in Reason's database.
# Usage: This function has no parameters so calling the function will return the list of plans.
# A where clause can be added in the query to pull specific plans or plans from specific states.
# It would be inserted above the order by line.
# example: where state.name in ('Texas', 'Arkansas')
# example2: where plan.id in (30,31,33,90,91,466,1469,1473,1875,1877,1878,1913,1915)

require(ggplot2)

planList <- function() {
  require(RPostgres)
  require(httr)
  require(tidyverse)

  # The folliwing url is provided by Heroku
  url <-
    "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"
  # To parse the url into usable sections use parse_url
  pg <- parse_url(url)
  # create a connection from the url using the parsed pieces
  con <- dbConnect(Postgres(),
    dbname = trimws(pg$path),
    host = pg$hostname,
    port = pg$port,
    user = pg$username,
    password = pg$password,
    sslmode = "require"
  )
  # define the query to retrieve the plan list
  q1 <- "select plan.id,
  display_name,
  state.name as State
  from plan
  inner join government
  on plan.admin_gov_id = government.id
  inner join state
  on government.state_id = state.id
  order by state.name"

  # sends the query to the connection
  res <- dbSendQuery(con, q1)
  # fetches the results
  plans <- dbFetch(res)
  p_list <- plans %>%
    mutate_if(sapply(plans, is.character), as.factor)
  # clears the results
  dbClearResult(res)
  # closes the connection
  dbDisconnect(con)
  p_list
}

####################################################################
# Description: This function pulls data for a selected plan from the Reason database.
# Parameters: The one parameter is the plan's name as found in the planList() function.
# Usage: example: allData <- pullData("Kansas Public Employees' Retirement System")

pullData <-
  function(displayName = "Texas Employees Retirement System") {
    require(RPostgres)
    require(httr)
    require(tidyverse)

    # The folliwing url is provided by Heroku
    url <-
      "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"
    # To parse the url into usable sections use parse_url
    pg <- parse_url(url)
    # create a connection from the url using the parsed pieces
    con <- dbConnect(
      Postgres(),
      dbname = trimws(pg$path),
      host = pg$hostname,
      port = pg$port,
      user = pg$username,
      password = pg$password,
      sslmode = "require"
    )
    # define the query to retrieve the plan data
    q2 <- "select plan_annual_attribute.year,
  plan.id,
  plan.display_name,
  state.name as state,
  plan_attribute.name as attribute_name,
  plan_annual_attribute.attribute_value,
  data_source_id,
  data_source.name as data_source_name
  from plan_annual_attribute
  inner join plan
  on plan_annual_attribute.plan_id = plan.id
  inner join government
  on plan.admin_gov_id = government.id
  inner join state
  on government.state_id = state.id
  inner join plan_attribute
  on plan_annual_attribute.plan_attribute_id = plan_attribute.id
  inner join data_source
  on plan_attribute.data_source_id = data_source.id
  where cast(plan_annual_attribute.year as integer) >= 1980 and
  data_source_id <> 1 and
  plan_id = "
    pl <- planList()
    # calls planList to get the plan id for the chosen plan
    planId <- pl$id[pl$display_name == displayName]
    # gets the matching plan id. This avoids problems with quotations in plan names.
    q3 <-
      paste0(q2, planId, " order by year, data_source_id, plan_attribute_id")
    # inserts the plan id into the middle of the query.
    res <- dbSendQuery(con, q3)
    all_data <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)

    all_data
  }

####################################################################
# Description: This function changes the data from long to wide.
# Parameters:
#     data = a datafrane in long format
#     name = The name of the attribute name column, defaults to attribute_name which is used in Reason's database
#     value = The name of the attribute value column, defaults to attribute_value
# Usage: allWide <- spreadData(allData)

spreadData <- function(data) {
  data %>%
    group_by_at(vars(-attribute_value)) %>%  # group by everything other than the value column. 
    mutate(row_id = 1:n()) %>% 
    ungroup() %>%  # build group index
    spread(attribute_name, attribute_value, convert = TRUE) %>%    # spread
    select(-row_id)  # drop the index
}

####################################################################
# Description: This function loads plan data from an Excel file
# Parameters: The filename including the path if in a subdirectory
# Usage: allWide <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx')

loadData <- function(filename) {
  require(tidyverse)
  read_excel(filename, col_types = "numeric")
}

####################################################################
# Description: This function selects the data used in the 'mountain of debt' graph
# Parameters:
#     wideData = a datasource in wide format
#     yearCol = the name of the column conatining the year
#     aalcol = the name of the column containing the AAL, default is Reason db column name
#     assetcol = the name of the column containing the Actuarial Assets, default to Reason db name.
#     base: Does the plan report their numbers by the thousand dollar or by the dollar?
#           default is 1000, change to 1 for plans that report by the dollar
# Usage: data <- modData(allWide,
#                   yearCol = 'Fiscal Year End',
#                   aalCol = 'Actuarial Accrued Liability',
#                   assetCol = 'Actuarial Value of Assets',
#                   base = 1)

modData <- function(wide_data,
                    year_col = "year",
                    aal_col = "Actuarial Accrued Liabilities Under GASB Standards",
                    asset_col = "Actuarial Assets under GASB standards",
                    base = 1000) {
  require(tidyverse)
  wide_data %>%
    select(year = year_col, actuarial_assets = asset_col, aal = aal_col) %>%
    mutate(
      uaal = as.numeric(aal) - as.numeric(actuarial_assets),
      # create a UAAL column as AAL-Actuarial Assets
      funded_ratio = as.numeric(actuarial_assets) / as.numeric(aal),
      # create a fundedRatio column as Actuarial Assets divided by AAL
    ) %>%
    mutate(
      actuarial_assets = as.numeric(actuarial_assets) * base,
      aal = as.numeric(aal) * base,
      uaal = uaal * base
    ) %>%
    drop_na()
}

####################################################################
# Description: This saves the theme for reuse in multiple plots
# must have ggplot2 require loaded
# Parameters: none
# Usage:  ggplot(...) + reasonTheme

reasonTheme <- theme(
  # removes legend
  legend.position = "none",

  # details the x-axis text
  axis.text.x = element_text(
    face = "bold",
    size = 14,
    # 0.5 centers the label on the tick mark
    vjust = 0.5,
    angle = 90,
    color = "black"
  ),
  axis.title.x = element_blank(),

  # axis lines set to black
  axis.line.x = element_line(color = "black"),
  axis.line.y = element_line(color = "black"),

  # left and right y-axis title and text fonts set
  axis.title.y.left = element_text(face = "bold", size = 14, color = "black"),
  axis.text.y.left = element_text(face = "bold", size = 14, color = "black"),
  axis.title.y.right = element_text(face = "bold", size = 14, color = "black"),
  axis.text.y.right = element_text(face = "bold", size = 14, color = "black"),

  # sets the background to blank white
  panel.background = element_blank()
)


####################################################################
# Description: This function creates the mountain of debt graph
# Parameters:
#     data: the dataframe created by the modData function
# Usage: modGraph(data)

modGraph <- function(data) {
  require(tidyverse)
  require(ggthemes)
  require(extrafont)
  require(scales)

  # extrapolate between years linearly
  extrapo <- approx(data$year, data$uaal, n = 10000)
  extrapo2 <- approx(data$year, data$funded_ratio, n = 10000)
  graph <-
    data.frame(
      year = extrapo$x,
      uaal = extrapo$y,
      funded_ratio = extrapo2$y
    )
  # create a "negative-positive" column for fill aesthetic
  graph$sign[graph$uaal >= 0] <- "positive"
  graph$sign[graph$uaal < 0] <- "negative"

  ggplot(graph, aes(x = year)) +
    # area graph using pos/neg for fill color
    geom_area(aes(y = uaal, fill = sign)) +
    # line tracing the area graph
    geom_line(aes(y = uaal)) +
    # line with funded ratio
    geom_line(aes(y = funded_ratio * (max(graph$uaal))), color = "#3300FF", size = 1) +
    # axis labels
    labs(y = "Unfunded Accrued Actuarial Liabilities", x = NULL) +

    # colors assigned to pos, neg
    scale_fill_manual(values = c("negative" = "#669900", "positive" = "#CC0000")) +

    # sets the y-axis scale
    scale_y_continuous(
      # creates 10 break points for labels
      breaks = pretty_breaks(n = 10),
      # changes the format to be dollars, without cents, scaled to be in billions
      labels = dollar_format(
        prefix = "$",
        scale = (1e-9),
        largest_with_cents = 1
      ),
      # defines the right side y-axis as a transformation of the left side axis, maximum UAAL = 100%, sets the breaks, labels
      sec.axis = sec_axis(
        ~ . / (max(graph$uaal) / 100),
        breaks = pretty_breaks(n = 10),
        name = "Funded Ratio",
        labels = function(b) {
          paste0(round(b, 0), "%")
        }
      ),
      # removes the extra space so the fill is at the origin
      expand = c(0, 0)
    ) +

    # sets the x-axis scale
    scale_x_continuous( # sets the years breaks to be every 2 years
      breaks = round(seq(min(graph$year), max(graph$year), by = 2), 1),
      expand = c(0, 0)
    ) +

    # adds the Reason theme defined previously
    reasonTheme
}

####################################################################
# Description: This function creates a data table containing the data in the mountain of debt graph.
# Parameters:
#     data: the dataframe created by the modData function
# Usage: modTable(data)

modTable <- function(data) {
  require(DT)
  require(tidyverse)

  data <- data %>%
    # give the columns pretty names
    rename(
      "Year" = year,
      "Actuarial Assets" = actuarial_assets,
      "Actuarial Accrued Liabilities" = aal,
      "Unfunded Actuarial Accrued Liabilities" = uaal,
      "Funded Ratio" = funded_ratio
    )
  # create a datatable
  datatable(
    data,
    # add buttons for export, etc.
    extensions = c("Buttons"),
    # remove row names
    rownames = FALSE,
    # allow editing the table, experimenting with this one
    editable = TRUE,
    options = list(
      bPaginate = FALSE,
      scrollX = T,
      scrollY = "600px",
      dom = "Brt",
      buttons = list(
        "copy",
        list(
          extend = "csv",
          text = "csv",
          title = "MOD"
        ),
        list(
          extend = "excel",
          text = "Excel",
          title = "MOD"
        ),
        list(
          extend = "pdf",
          text = "pdf",
          title = "MOD"
        )
      )
    )
  ) %>%
    formatCurrency(c(2:4)) %>%
    formatPercentage(5, 2)
}

####################################################################
# Description: This function creates a graph in the Gain/Loss format
# Parameters:
#     filename: the name of the file containing the gain/loss data
#     ylab: The y-axis label, default set
# Usage: glGraph(filename = 'data/Graph 1.csv')

glGraph <-
  function(filename, ylab = "Changes in Unfunded Liability (in Billions)") {
    require(ggplot2)
    require(tidyverse)

    graph1 <- read_csv(filename) %>% # load data from csv file
      gather("label", "value") %>% # put in long format with label-value pairs
      mutate(label = str_wrap(label, 8)) %>% # wrap the label names to clean up axis labels
      mutate(label = str_to_title(label)) %>% # properly capitalize the labels
      # assign pos/neg/total to the values for fill color
      mutate(
        sign = case_when(
          value >= 0 ~ "positive",
          value < 0 ~ "negative"
          )
      ) %>%
      mutate(sign = case_when(label == "Total" ~ "total", TRUE ~ sign)) %>% 
      mutate(sign = factor(sign, levels = c("total", "negative", "positive"))) %>% 
      mutate(label = factor(label, levels = label[order(sign, value, label, decreasing = TRUE)], ordered = TRUE))

    # assign colors to go with signs
    fill_colors <- c(
      "negative" = "#669900",
      "positive" = "#CC0000",
      "total" = "#FF6633"
      )

    # create plot
    ggplot(graph1, aes(x = label, y = value)) +
      geom_col(width = 0.75, aes(fill = sign)) +
      geom_hline(yintercept = 0, color = "black") +
      scale_fill_manual(values = fill_colors) +
      scale_y_continuous(breaks = pretty_breaks(), labels = dollar_format(prefix = "$")) +
      ylab(ylab) +
      reasonTheme +
      theme(
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 0)
      )
    # ggsave("graph1.2.png", width = 9, height = 5.33)
  }

####################################################################
# Description: This function selects the data used in several graphs
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


selectedData <- function(wide_data,
                          date_col = "Actuarial Valuation Date For GASB Assumptions",
                          aal_col = "Actuarial Accrued Liabilities Under GASB Standards",
                          asset_col = "Actuarial Assets under GASB standards",
                          adec_col = "Employer Annual Required Contribution",
                          emp_cont_col = "Employer Contributions",
                          payroll_col = "Covered Payroll") {
  require(tidyverse)
  require(lubridate)

  wide_data %>%
    mutate(
      year = year(as_date(as.numeric(`Actuarial Valuation Date For GASB Assumptions`), origin = "1900-01-01")),
      valuation_date = as_date(as.numeric(`Actuarial Valuation Date For GASB Assumptions`), origin = "1900-01-01")
      ) %>%
    rename(
      actuarial_assets = asset_col,
      aal = aal_col,
      adec = adec_col,
      emp_cont = emp_cont_col,
      payroll = payroll_col
    ) %>%
    mutate(
      uaal = as.numeric(aal) - as.numeric(actuarial_assets),
      funded_ratio = as.numeric(actuarial_assets) / as.numeric(aal),
      `ADEC Contribution Rates` = as.numeric(adec) / as.numeric(payroll),
      `Actual Contribution Rates (Statutory)` = as.numeric(emp_cont) / as.numeric(payroll)
    ) %>%
    select(
      year,
      valuation_date,
      actuarial_assets,
      aal,
      uaal,
      funded_ratio,
      adec,
      emp_cont,
      `ADEC Contribution Rates`,
      `Actual Contribution Rates (Statutory)`,
      payroll
    ) %>%
    drop_na()
}

####################################################################
# Description: This function creates a graph comparing 2 percentages
# Parameters:
#     data: the dataframe created by the selected_Data function
# Usage: contGraph(data)

contGraph <- function(data, 
                      y1 = "ADEC Contribution Rates", 
                      y2 = "Actual Contribution Rates (Statutory)", 
                      y3 = NULL,
                      labelY = NULL,
                      label1 = NULL, 
                      label2 = NULL,
                      label3 = NULL) {
  require(ggplot2)
  require(tidyverse)
  require(scales)

  graph <- data %>%
    select(
      year,
      y1,
      y2, 
      y3
    ) %>%
    mutate_all(funs(as.numeric)) %>%
    rename(label1 = y1, label2 = y2, label3 = y3) %>% 
    gather(key = keys, value = amount, -year)

  lineColors <- c(
    y1 = "#FF6633",
    y2 = "#3300FF",
    y3 = "#333333"
  )
  
  labs <- c(
    label1,
    label2,
    label3
  )

  ggplot(graph, aes(x = year)) +
    geom_line(aes(y = amount * 100, color = keys), size = 2) +
    scale_fill_manual(values = lineColors) +
    geom_hline(yintercept = 0, color = "black") +

    scale_y_continuous(
      breaks = pretty_breaks(10),
      labels = function(b) {
        paste0(round(b, 0), "%")
      }
    ) +

    scale_x_continuous(breaks = pretty_breaks(10)) +

    ylab(labelY) +
    scale_color_discrete(labels = labs) +

    reasonTheme +
    theme(
      legend.justification = c(1, 1),
      legend.position = c(0.5, 1),
      legend.title = element_blank()
    )
}
