# create a function to grab a list of the plans with their state

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
  # remove 'where plan.id ...' line to return all plans
  q1 <- "select plan.id, 
  display_name, 
  state.name as State 
  from plan 
  inner join government 
  on plan.admin_gov_id = government.id 
  inner join state 
  on government.state_id = state.id 
  where plan.id in (30,31,33,90,91,466,1469,1473,1875,1877,1878,1913,1915)
  order by state.name"
  
  res <- dbSendQuery(con, q1)
  pList <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  pList
}

# Create a function to pull data for selected plan

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
  planId <- pl$id[pl$display_name == displayName]
  q3 <- paste0(q2, planId, " order by year, data_source_id, plan_attribute_id")
  res <- dbSendQuery(con,q3)
  allData <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  
  allData
}

spreadData <- function(data, yeadCol, attribCol, valCol){
  allWide <- data %>% 
    select(year, attribute_name, attribute_value) %>% 
    spread(attribute_name, attribute_value)
  allWide
}


loadData <- function(filename){
  library(readxl)
  read_excel(filename, col_types = "numeric")
}

d <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx')


# create a function to produce the mountain of debt graph
modGraph <- function(wideData, 
                     yearCol = 'year', 
                     aalCol = 'Actuarial Accrued Liabilities Under GASB Standards',
                     assetCol = 'Actuarial Assets under GASB standards'){
  
  library(tidyverse)
  subsetData <- wideData %>% 
    rename(actuarialAssets = assetCol, 
           AAL = aalCol,
           year = yearCol) %>%
    select(year, actuarialAssets, AAL) %>% 
    mutate(UAAL = as.numeric(AAL) - as.numeric(actuarialAssets), 
           fundedRatio = as.numeric(actuarialAssets) / as.numeric(AAL)) %>% 
    drop_na()
  
  #extrapolate data points to "smooth out" the area chart 
  subsetData$year <- as.numeric(as.character(subsetData$year))
  subsetData$fundedRatio <- subsetData$fundedRatio*100
  extrapo <- approx(subsetData$year, subsetData$UAAL, n = 10000)
  extrapo2 <- approx(subsetData$year, subsetData$fundedRatio, n = 10000)
  graph <- data.frame(year = extrapo$x, UAAL = extrapo$y, fundedRatio = extrapo2$y)
  #create a "negative-positive" column for fill aesthetic
  graph$sign[graph$UAAL >= 0] = "positive"
  graph$sign[graph$UAAL < 0] = "negative"
  
  p <- ggplot(graph, aes(x = year)) +
    geom_area(aes(y = UAAL, fill = sign), show.legend = FALSE) +
    scale_fill_manual(values = c("#669900", "#CC0000")) +
    geom_line(aes(y = UAAL)) +
    geom_line(aes(y = fundedRatio), color = '#3300FF', size = 1) +
    labs(y = 'Unfunded Accrued Actuarial Liabilities', x = NULL) +
    theme(
      axis.line.y = element_line(color = "black"),
      axis.text.x = element_text(
        face = "bold",
        size = 14,
        hjust = 1,
        angle = 90,
        color = "black"
      ),
      axis.title.x = element_blank(),
      axis.title.y.left = element_text(face = 'bold', size = 14, color = "black"),
      axis.text.y.left = element_text(face = "bold", size = 14, color = "black"),
      axis.title.y.right = element_text(face = 'bold', size = 14, color = "black"),
      axis.text.y.right = element_text(face = "bold", size = 14, color = "black"),
      panel.background = element_blank()
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 10),
      labels = dollar_format(prefix = "$"),
      sec.axis = sec_axis(
        ~ . / (max(graph$UAAL) / 100),
        breaks = pretty_breaks(n = 10),
        name = "Funded Ratio",
        labels = function(b) {
          paste0(round(b, 0), "%")
        }
      )
    ) +
    scale_x_continuous(breaks = round(seq(min(graph$year), max(graph$year), by = 2), 1)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(
      y = fundedRatio * (max(graph$UAAL) / 100)),
      color = '#3300FF',
      size = 1
    )
  p
}





