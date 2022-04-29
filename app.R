# Chart Library 
# By: AESoto
# 20 April 2022

chartLibraries <- c('quantmod', 'tidyverse', 'highcharter', 'tidyquant', 'timetk', 'shinyWidgets', 'shiny' )
lapply(chartLibraries, require, character.only=TRUE)
library(fredr)
fredr_set_key('dad071a33ef9414bb0a759835d9ec507')


# Define UI for application that draws chart
ui <- fluidPage(
  
  # Application title
  titlePanel("Chart Maker"),
  fluidRow(
    column(2,
           awesomeRadio(
             inputId = "metric",
             label = "Metric",
             choices = c("10Yr Yield"= 'DGS10' ,
                         "EV/Sales" ='.',
                         "PE-LTM" ='.',
                         "PE-NTM"= '.'),
             selected = "DGS10",
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

# Define server logic required to draw chart
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    req(input$metric)
    req(input$metric !='.')
    
    timeSeries <- fredr(input$metric, 
                        observation_start = input$start,
                        observation_end = input$end) %>%
        drop_na() %>%
        select(1,3) %>%
        rename(Close = 2)
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)

    chartSeries(timeSeries,
                type="line",
                theme=chartTheme('white'))
    
    # if (input$metric == '10Yr Yield') {
    #   print('10Yr Yield')
    # 
    #   # 10 Yr Yield
    #   TNX <- fredr('DGS10', observation_start = input$start, observation_end = input$end) %>% 
    #     drop_na() %>% 
    #     select(1,3) %>% 
    #     rename(Close = 2)
    #   TNX <- xts(TNX[,2], order.by = TNX$date)
    #   timeSeries <- TNX
    # } else {
    #   print('Other Metric')
    #   getSymbols('^TNX', from = as.Date(input$start), to=as.Date(input$end))
    #   tenYr <- na.omit(TNX)
    #   tenYr$TNX.Volume <- NULL
    #   timeSeries <- tenYr
    # }    

  })
}

# Run the application 
shinyApp(ui = ui, server = server)
