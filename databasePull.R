library(RPostgres)
library(tidyverse)
library(dbplyr)

# The folliwing url is provided by Heroku
url <- "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"

# To parse the url into usable sections use parse_url
pg <- httr::parse_url(url)

# predefine the queries we will need
q1 <- "select plan.id, 
display_name, 
state.name as State 
from plan 
inner join government 
on plan.admin_gov_id = government.id 
inner join state 
on government.state_id = state.id 
order by state.name"
q2 <- "select plan_annual_attribute.year, 
plan.id, 
plan.display_name, 
state.name as state, 
plan_attribute.name as attribute_name, 
plan_annual_attribute.attribute_value 
from plan_annual_attribute 
inner join plan 
on plan_annual_attribute.plan_id = plan.id
inner join government 
on plan.admin_gov_id = government.id
inner join state 
on government.state_id = state.id
inner join plan_attribute 
on plan_annual_attribute.plan_attribute_id = plan_attribute.id 
where plan_id = "
# create a connection from the url using the parsed pieces
con <- dbConnect(RPostgres::Postgres(),
                          dbname = trimws(pg$path),
                          host = pg$hostname,
                          port = pg$port,
                          user = pg$username,
                          password = pg$password,
                          sslmode = "require"
                 )

# grab a list of the plans with their state
res <- dbSendQuery(con, q1)
planList <- dbFetch(res)
dbClearResult(res)

# Run a SQL query 
planId <- select(subset(planList,id == 1915),id)$id
q3 <- paste0(q2, planId, " order by year, data_source_id, plan_attribute_id")
res <- dbSendQuery(con,q3)
allData <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
allWide <- allData %>% 
  select(year, id, display_name, state, attribute_name, attribute_value) %>% 
  spread(attribute_name, attribute_value)


