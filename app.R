#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
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
                   inner join state on government.state_id = state.id
                   order by state.name")
planList <- dbFetch(res)
dbClearResult(res)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pension Plan Visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h1("Select a plan"),
         selectInput("state", "State:", choices = planList$state, selected = "Texas")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("vis1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # df <- reactive({
  #   database %>% filter(state == input$state & plan == input$plan)
  # })
  #  output$vis1 <- renderPlot({
  #    
  #  })
}

# Run the application 
shinyApp(ui = ui, server = server)

