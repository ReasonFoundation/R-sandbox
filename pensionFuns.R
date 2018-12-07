#This script contains functions used to load pension plan data either from Reason's database or 
#from an excel file.
#Author: Andrew Abbott
#Date: 12/7/2018


# This function grabs a list of the plans with their state from the Reason database. 
# Use this to find the exact plan names that are used in Reason's database.
# Usage: This function has no parameters so calling the function will return the list of plans. 
# A where clause can be added in the query to pull specific plans or plans from specific states. 
# It would be inserted above the order by line.
# example: where state.name in ('Texas', 'Arkansas')
# example2: where plan.id in (30,31,33,90,91,466,1469,1473,1875,1877,1878,1913,1915)

planList <- function(){
  
  library(RPostgres)
  library(httr)
  library(tidyverse)
  
  # The folliwing url is provided by Heroku
  url <- "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"
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
  
  #sends the query to the connection
  res <- dbSendQuery(con, q1)
  # fetches the results
  plans <- dbFetch(res)
  pList <- plans %>% mutate_if(is.character, as.factor)
  # clears the results
  dbClearResult(res)
  # closes the connection
  dbDisconnect(con)
  pList
}

####################################################################
# Description: This function pulls data for a selected plan from the Reason database.
# Parameters: The one parameter is the plan's name as found in the planList() function.
# Usage: example: allData <- pullData("Kansas Public Employees' Retirement System")

pullData <- function(displayName = "Texas Employees Retirement System"){
  
  library(RPostgres)
  library(httr)
  library(tidyverse)
  
  # The folliwing url is provided by Heroku
  url <- "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"
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
  q3 <- paste0(q2, planId, " order by year, data_source_id, plan_attribute_id")   
  # inserts the plan id into the middle of the query.
  res <- dbSendQuery(con,q3)
  allData <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  
  allData
}

####################################################################
# Description: This function changes the data from long to wide.
# Parameters:
#     data = a datafrane in long format
#     name = The name of the attribute name column, defaults to attribute_name which is used in Reason's database
#     value = The name of the attribute value column, defaults to attribute_value
# Usage: allWide <- spreadData(allData) 

spreadData <- function(data, name = "attribute_name", value = "attribute_value"){
  allWide <- data %>% 
    select(year, name, value) %>% 
    spread(name, value)
  allWide
}

####################################################################
# Description: This function loads plan data from an Excel file
# Parameters: The filename including the path if in a subdirectory
# Usage: allWide <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx')

loadData <- function(filename){
  library(readxl)
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


modData <- function(wideData,
                    yearCol = 'year',
                    aalCol = 'Actuarial Accrued Liabilities Under GASB Standards',
                    assetCol = 'Actuarial Assets under GASB standards', 
                    base = 1000) {
  library(tidyverse)
  subsetData <- wideData %>%
    rename(actuarialAssets = assetCol,
           AAL = aalCol,
           year = yearCol) %>%
    select(year, actuarialAssets, AAL) %>%
    mutate(
      UAAL = as.numeric(AAL) - as.numeric(actuarialAssets),
      # create a UAAL column as AAL-Actuarial Assets
      fundedRatio = as.numeric(actuarialAssets) / as.numeric(AAL),
      # create a fundedRatio column as Actuarial Assets divided by AAL
    ) %>%
    mutate(actuarialAssets = as.numeric(actuarialAssets)*base, AAL = as.numeric(AAL)*base, UAAL = UAAL*base) %>% 
    drop_na()
  subsetData
}

####################################################################
# Description: This saves the theme for reuse in multiple plots
# Parameters: none
# Usage:  ggplot(...) + reasonTheme

reasonTheme <-  theme(
  # removes legend
  legend.position = 'none',
  
  # details the x-axis text
  axis.text.x = element_text(
    face = "bold",
    size = 14,
    # 0.5 centers the label on the tick mark
    vjust = 0.5,
    angle = 90,
    color = "black"),
  
  # axis lines set to black
  axis.line.x = element_line(color = "black"),
  axis.line.y = element_line(color = "black"),
  
  # left and right y-axis title and text fonts set
  axis.title.y.left = element_text(face = 'bold', size = 14, color = "black"),
  axis.text.y.left = element_text(face = "bold", size = 14, color = "black"),
  axis.title.y.right = element_text(face = 'bold', size = 14, color = "black"),
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
  library(tidyverse)
  library(ggthemes)
  library(extrafont)
  library(scales)
  
  # extrapolate between years linearly
  extrapo <- approx(data$year, data$UAAL, n = 10000)
  extrapo2 <- approx(data$year, data$fundedRatio, n = 10000)
  graph <-
    data.frame(year = extrapo$x,
               UAAL = extrapo$y,
               fundedRatio = extrapo2$y)
  # create a "negative-positive" column for fill aesthetic
  graph$sign[graph$UAAL >= 0] = "positive"
  graph$sign[graph$UAAL < 0] = "negative"
  
  p <- ggplot(graph, aes(x = year)) +
    # area graph using pos/neg for fill color
    geom_area(aes(y = UAAL, fill = sign)) +
    # line tracing the area graph
    geom_line(aes(y = UAAL)) +
    # line with funded ratio
    geom_line(aes(y = fundedRatio * (max(graph$UAAL))), color = '#3300FF', size = 1) +
    # axis labels
    labs(y = 'Unfunded Accrued Actuarial Liabilities', x = NULL) +
    
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
        ~ . / (max(graph$UAAL) / 100),
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
    scale_x_continuous(# sets the years breaks to be every 2 years
      breaks = round(seq(min(graph$year), max(graph$year), by = 2), 1),
      expand = c(0, 0)) +
    
    # adds the Reason theme defined previously
    reasonTheme 
  p
}

####################################################################
# Description: This function creates a data table containing the data in the mountain of debt graph.
# Parameters:
#     data: the dataframe created by the modData function
# Usage: modTable(data)

modTable <- function(data){
  library(DT)
  library(tidyverse)
  
  data <- data %>%
    # give the columns pretty names
    rename('Year' = year,
           'Actuarial Assets' = actuarialAssets, 
           'Actuarial Accrued Liabilities' = AAL,
           'Unfunded Actuarial Accrued Liabilities' = UAAL,
           'Funded Ratio' = fundedRatio)
  # create a datatable 
  datatable(data,
            # add buttons for export, etc.
            extensions = c('Buttons'),
            # remove row names
            rownames = FALSE, 
            # allow editing the table, experimenting with this one
            editable = TRUE,
            options = list(
              bPaginate = FALSE, 
              scrollX = T,
              scrollY = '600px',
              dom = 'Brt',
              buttons = list(
                'copy', 
                list(extend = 'csv', text = 'csv', title = 'MOD'), 
                list(extend = 'excel', text = 'Excel', title = 'MOD'), 
                list(extend = 'pdf', text = 'pdf', title = 'MOD')
                )
              )
            ) %>%
    formatCurrency(c(2:4)) %>% 
    formatPercentage(5,2)
}




