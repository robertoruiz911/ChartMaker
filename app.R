# Chart Library 
# By: AESoto
# 20 April 2022

chartLibraries <- c('quantmod', 'tidyverse', 'highcharter',
                    'tidyquant', 'timetk', 'shinyWidgets', 'shiny' )
lapply(chartLibraries, require, character.only=TRUE)

############################
# Load data sets
# TODO: expand data load....

# 10 Yr Yield
# tenYr <- read.csv("DGS10.csv")
# tenYr <- tenYr[!(tenYr$DGS10=='.'),]

############################



# Define UI for application that draws charts
ui <- fluidPage(
  
  sidebarLayout(
    # Application title
    titlePanel("Chart Maker"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(2,
             awesomeRadio(
               inputId = "Id003",
               label = "Metric",
               choices = c("10Yr Yield", "Something1", "Something2"),
               selected = "10Yr Yield",
               status = "warning"
             )
      )
    ),
    
    fluidRow(
      column(2,
             dateInput("start",
                       "Start Date",
                       "2013-01-01",
                       format = "yyyy-mm-dd")
      )
    ),
    
    fluidRow(
      column(2,
             dateInput("end",
                       "End Date",
                       "2022-04-18",
                       format = "yyyy-mm-dd")
      )
    ),
    
    fluidRow(
      column(2,  
        actionButton("go", "Submit"),
      )
    ),
    # Generate chart
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server logic required to draw chart
server <- function(input, output) {
  
  observe({
    print(input$end)
  })
  
  timeSeries <- eventReactive(input$go, {
    # will apply switch statement to accomodate other chart subjects
    getSymbols('^TNX', from = as.Date(input$start), to=as.Date(input$end))
    tenYr <- na.omit(TNX)
    tenYr$TNX.Volume <- NULL
  
  timeSeries <- tenYr
  })  # closes timeSeries function....

output$distPlot <- renderPlot({
  # this applies the quantmod charting...
  chartSeries(timeSeries,
              type="line",
              theme=chartTheme('white'))
}) # closes renderPlot function
}

# Run the application 
shinyApp(ui = ui, server = server)
