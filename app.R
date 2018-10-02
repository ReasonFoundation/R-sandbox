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
plans <- dbFetch(res) 
planList <- plans %>% mutate_if(sapply(plans, is.character), as.factor)
dbClearResult(res)
dbDisconnect(con)

# ----------Begin shiny app portion -------------

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Pension Plan Visualization"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        h2("Select a plan"),
        # select state to choose a plan from
        selectInput("state", "Select a state:", choices = levels(planList$state), selected = "Texas"),
        br(),
        uiOutput("plan"),
        
      # Built with Shiny by RStudio
        h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
         ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot #1",
#                             plotlyOutput(outputId = "p"),
                             br(),
                             uiOutput("text")
                    ),
                    tabPanel("Data", tableOutput(outputId = "table"),
                             br(),
                             downloadButton("download_data", "Download data"))
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plans <- reactive({
    planList %>% subset(state == input$state) %>% select(display_name)
  })
  
  output$plan <- renderUI({
    selectInput("plan", "Select a plan:", choices = plans())
  })

  dataset <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = trimws(pg$path),
                     host = pg$hostname,
                     port = pg$port,
                     user = pg$username,
                     password = pg$password,
                     sslmode = "require"
    )
    res2 <- dbSendQuery(con, paste0(q2, select(subset(planList,display_name == input$plan),id)$id," order by year, state, plan.display_name"))
    subset_data <- dbFetch(res2)
    dbClearResult(res2)
    dbDisconnect(con)
    subsetWide <- spread(select(subset_data, year, id, display_name, state, attribute_name, attribute_value),attribute_name, attribute_value)
    return(subsetWide)
  })
  
  output$text <- renderUI({
    HTML(paste0("The chosen state is: ",input$state,br(),"The chosen plan is: ", input$plan))
  })
  
  output$table <- renderTable({
    dataset()
  })
  
  output$download_data <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      #code
      write.csv(dataset(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

