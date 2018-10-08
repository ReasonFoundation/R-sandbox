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
library(DT)
library(plotly)

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
  where plan.id in (30, 31, 33, 89, 91, 1469, 1473, 1875, 1877, 1878, 1915)
  order by state.name"
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
  plan_id ="

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
        htmlOutput("text"),
        
      # Built with Shiny by RStudio
        h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
         ),
      
      mainPanel(
        tabsetPanel(
          type = "tabs", 
          tabPanel("Contributions", 
                   plotOutput(outputId = "p2"), 
                   br()
          ),
          tabPanel("Mountain of Debt", 
                   plotlyOutput(outputId = "p"), 
                   br()
                   ),
          tabPanel("Data", 
                   fluidRow(column(12,DTOutput(outputId = "table"))), 
                   br(), 
                   downloadButton("download_data", "Download data")
                   ) 
          )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plans <- reactive({
    planList %>% subset(state == input$state) %>% select(display_name)
  })
  
  query <- reactive({
    planList$id[planList$display_name == input$plan]
    paste0(q2, planList$id[planList$display_name == input$plan]," order by year, state, plan.display_name")
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
    res2 <- dbSendQuery(con, query())
    subset_data <- dbFetch(res2)
    dbClearResult(res2)
    dbDisconnect(con)
    subset_data %>% 
      select(year, id, display_name, state, attribute_name, attribute_value) %>% 
      spread(attribute_name, attribute_value) 
  })
  
  selectedData <- reactive({
    dataset() %>% 
      select(year, display_name, `Actuarial Assets under GASB standards`, `Actuarial Accrued Liabilities Under GASB Standards`, 
             `Employer Annual Required Contribution`, `Employer Contributions`, `Covered Payroll`) %>% 
      rename(actuarialAssets = `Actuarial Assets under GASB standards`, 
             AAL = `Actuarial Accrued Liabilities Under GASB Standards`, 
             ADEC = `Employer Annual Required Contribution`,
             empCont = `Employer Contributions`,
             payroll = `Covered Payroll`) %>%
      mutate(UAAL = as.numeric(AAL) - as.numeric(actuarialAssets), 
             fundedRatio = as.numeric(actuarialAssets) / as.numeric(AAL), 
             `ADEC Contribution Rates` = 100*as.numeric(ADEC)/as.numeric(payroll),
             `Actual Contribution Rates (Statutory)` = 100*as.numeric(empCont)/as.numeric(payroll)
      )
  })
  
  output$plan <- renderUI({
    selectInput("plan", "Select a plan:", choices = plans(), selected = "Texas Employees Retirement System")
  })
  
  output$text <- renderUI({
    HTML(paste0("The chosen state is: ",input$state,br(),"The chosen plan is: ", input$plan))
  })
  
  output$table <- renderDT({
    selectedData()
  })
  
  output$download_data <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
#      write.csv(dataset(), file)
      write.csv(debt(), file)
    }
  )
  
  output$p <- renderPlotly({
    df <- selectedData()
    #extrapolate data points to "smooth out" the area chart 
    df$year <- as.numeric(as.character(df$year))
    df$fundedRatio <- df$fundedRatio*100
    extrapo <- approx(df$year, df$UAAL, n = 10000)
    extrapo2 <- approx(df$year, df$fundedRatio, n = 10000)
    graph <- data.frame(year = extrapo$x, UAAL = extrapo$y, fundedRatio = extrapo2$y)
    
    #create a "negative-positive" column for fill aesthetic
    graph$sign[graph$UAAL >= 0] = "positive"
    graph$sign[graph$UAAL < 0] = "negative"
    cols <- c("positive" = "#CC0000", "negative" = "#669900")
    
    p <- ggplot(graph, aes(x = year)) +
      geom_area(aes(y = UAAL, fill = sign), show.legend = FALSE) +
      scale_fill_manual(values = cols) +
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
        panel.background = element_blank(),
        legend.position = "none"
      ) +
      scale_y_continuous(
        breaks = round(seq(min(graph$UAAL), max(graph$UAAL), by = 1000000), -3),
        labels = dollar_format(prefix = "$"),
        sec.axis = sec_axis(
          ~ . / (max(graph$UAAL) / 100),
          breaks = seq(-10, 135, 10),
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
    ggplotly(p)
  })
  
  output$p2 <- renderPlot({
    df <- selectedData() %>% 
      select(year, `ADEC Contribution Rates`, `Actual Contribution Rates (Statutory)`) %>% 
      mutate_all(funs(as.numeric)) %>% 
      gather(key = contribution, value = amount, -year)
    p2 <- ggplot(df, aes(x = year)) +
      geom_line(aes(y = amount, color = contribution)) +
      theme(
        axis.line = element_line(color = "black"),
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
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 14, color = "black"),
        legend.position = c(0.3, 0.8)
      ) +
      labs(y = "Employer Contribution (% of Payroll)") +
      scale_y_continuous(
        breaks = round(seq(0, max(df$amount), by = 2), 1),
        labels = percent_format(accuracy = 0.1, scale = 1)
      ) +
      scale_x_continuous(breaks = round(seq(min(df$year), max(df$year), by = 2), 1))
    p2
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

