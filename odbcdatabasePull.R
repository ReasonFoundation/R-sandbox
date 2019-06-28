library(RPostgres)
library(tidyverse)
library(dbplyr)
library(janitor)
library(DBI)
library(odbc)

# The folliwing url is provided by Heroku
url <- "postgres://u9a2ef5lju8rrc:p54e096881444359c1c6cf3992281aaeecbe8f2bd5aa5e993f4811c2e7316cfb2@ec2-3-209-200-73.compute-1.amazonaws.com:5432/d629vjn37pbl3l"


# To parse the url into usable sections use parse_url
pg <- httr::parse_url(url)

# predefine the queries we will need

q2 <- "select plan_annual_master_attribute.year,
  plan_annual_master_attribute.plan_id,
  plan_annual_master_attribute.attribute_value,
  plan.display_name,
  plan_master_attribute_names.name
  
  from                plan_annual_master_attribute
  inner join          plan
  on plan_annual_master_attribute.plan_id = plan.id
  inner join          plan_master_attribute_names
  on plan_annual_master_attribute.master_attribute_id = plan_master_attribute_names.id
   order by year, plan_annual_master_attribute.plan_id, plan_master_attribute_names.id"
# create a connection from the url using the parsed pieces
db_con <- dbConnect(odbc::odbc(),
                          driver = "PostgreSQL Driver",
                          database = trimws(pg$path),
                          server = pg$hostname,
                          Port = pg$port,
                          uid = pg$username,
                          pwd = pg$password,
                          sslmode = "require"
                 )


# Run a SQL query 
res <- dbSendQuery(db_con,q2)
allData <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
allWide <- allData %>% 
  filter(State == "Mississippi")
 # filter(display_name == 'Mississippi Public Employees Retirement System') 
allWide <- allWide %>% 
  select(year, plan_id, display_name, name, attribute_value) %>% 
  spread(name, attribute_value) 
clean <- allWide %>% 
  remove_empty('cols')


