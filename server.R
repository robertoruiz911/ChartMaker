library(lubridate)
library(quantmod)
library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(shinyWidgets)
library(shiny)
library(fredr)
library(here)
fredr_set_key('dad071a33ef9414bb0a759835d9ec507')


shinyServer(function(input, output) {

 output$distPlot <- renderPlot({
   
   if (input$metric == 'DGS10'){metricName <- '10Yr Yield'}
     
   timeSeries <- fredr(input$metric, 
                       observation_start = input$start,
                       observation_end = input$end) %>%
                  drop_na() %>%
                  select(1,3) %>%
                  rename(Close = 2)
   
   timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)

   chartSeries(timeSeries,
               name = metricName,
               type="line",
               theme=chartTheme('white'))
   

    })
 
 
 
 output$distPlot2 <- renderPlot({
   

     
     metricName <- ifelse(input$metric2 == 'DFII10', "10Y TIPS", 
                          ifelse(input$metric2 == 'T10YIE', "10Y Breakeven Inflation",
                                 ifelse(input$metric2 == 'T5YIE', "5Y Breakeven Inflation",
                                        ifelse(input$metric2 == 'T5YIFR', "5,5 Forward Inflation", NULL))))
     
   
     timeSeries <- fredr(input$metric2, 
                         observation_start = input$start2,
                         observation_end = input$end2) %>%
       drop_na() %>%
       select(1,3) %>%
       rename(Close = 2)
     
     timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
     
     chartSeries(timeSeries,
                 name = metricName,
                 type="line",
                 theme=chartTheme('white'))
   
   
 })

})
