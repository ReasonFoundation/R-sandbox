library(RPostgres)
library(tidyverse)
library(dbplyr)

# The folliwing url is provided by Heroku
url <- "postgres://viliygpvlizwel:5c26e3ddd0b2682b5c71a4230547677007d7f9fcfe1ed1c29ee45d6375a7475d@ec2-54-235-177-45.compute-1.amazonaws.com:5432/d47an5cjnv5mjb"

# To parse the url into usable sections use parse_url
pg <- httr::parse_url(url)

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
res <- dbSendQuery(con, "select plan.id, display_name, state.name as State from plan 
                inner join government on plan.admin_gov_id = government.id
                        inner join state on government.state_id = state.id")
planList <- dbFetch(res)
dbClearResult(res)

# Run a SQL query 
AustinData <- dbSendQuery(con, "select plan_annual_attribute.year, plan_attribute_id, plan_attribute.name as attribute_name, data_source_id, 
                data_source.name as data_source_name, plan_annual_attribute.attribute_value from plan_annual_attribute 
                inner join plan_attribute on plan_annual_attribute.plan_attribute_id = plan_attribute.id 
                inner join data_source on plan_attribute.data_source_id = data_source.id
                where plan_id = 1915
                order by year, data_source_id, plan_attribute_id") %>% dbFetch()
dbClearResult(res)


