# Chart Library 
# By: AESoto
# 20 April 2022

chartLibraries <- c('quantmod', 'tidyverse', 'highcharter', 'tidyquant', 'timetk', 'shinyWidgets', 'shiny' )
lapply(chartLibraries, require, character.only=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Chart Maker"),
  fluidRow(
    column(2,
       awesomeRadio(
         inputId = "metric",
         label = "Metric",
         choices = c("10Yr Yield", "EV/Sales", "PE-LTM", "PE-NTM"),
         selected = "10Yr Yield",
         status = "warning"
         ),
       dateInput("start",
                 "Start Date",
                 "2020-01-01",
                 format = "yyyy-mm-dd"
                 ),
       dateInput("end",
                 "End Date",
                 "2022-04-18",
                 format = "yyyy-mm-dd"
                 ),
       actionButton("go", "Submit"),
       actionButton("go", "Print")
    ),
    column(10,
      plotOutput('distPlot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if (input$metric == '10Yr Yield') {
      print('10Yr Yield')
      # 10 Yr Yield
      getSymbols('^TNX', from = as.Date(input$start), to=as.Date(input$end))
      tenYr <- na.omit(TNX)
      tenYr$TNX.Volume <- NULL
      timeSeries <- tenYr
    } else {
      print('Other Metric')
      getSymbols('^TNX', from = as.Date(input$start), to=as.Date(input$end))
      tenYr <- na.omit(TNX)
      tenYr$TNX.Volume <- NULL
      timeSeries <- tenYr
    }    

    chartSeries(timeSeries,
                type="line",
                theme=chartTheme('white'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
