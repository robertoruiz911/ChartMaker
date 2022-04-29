# Chart Maker
# By: AESoto
# 20 April 2022

chartLibraries <- c('quantmod', 'tidyverse', 'highcharter', 'tidyquant',
                    'timetk', 'shinyWidgets', 'shiny', 'fredr', 'here' )
lapply(chartLibraries, require, character.only=TRUE)
fredr_set_key('dad071a33ef9414bb0a759835d9ec507')


# Define UI for application that draws chart
ui <- fluidPage(
  # Application title
  titlePanel("Chart Maker"),
  # UI Layout
  fluidRow(
    column(2,
           awesomeRadio(
             inputId = "metric",
             label = "Metric",
             choices = c("10Yr Yield" = 'DGS10' ,
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
  # create chart
  output$distPlot <- renderPlot({
    req(input$metric)
    # req(input$metric != '.')
    
    if (input$metric == 'DGS10') {
      metricName <- '10Yr Yield'
      timeSeries <- fredr(input$metric, 
                          observation_start = input$start,
                          observation_end = input$end) %>%
                          drop_na() %>%
                          select(1,3) %>%
                          rename(Close = 2)
      timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    } else if (input$metric == '') { #EV/Sales variable
      metricName <- 'EV/Sales'
      # API Query, define time series
    } else if (input$metric == '') { #PE-LTM variable
      metricName <- 'PE-LTM'
      # API Query, define time series
    } else { #PE-NTM variable
      metricName <- 'PE-NTM'
      # API Query, define time series
    }
    # Question: I really need the chart title to be the chosen metric, not 'timeSeries' - I think this change does it
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
  })
  
  # I suppose there should be an 'output$___' type of statement where there is reaction to the 'Print' button
  # I don't quite understand how Shiny identifies reaction to a button, or between the two buttons in this case
  
  # The following likely contained within a function based on the 'Print' button (per comment above)
  # convert chart into image - I understand that officer does cannot accommodate a quantmod chart so we need to create a .png file
  chart_path <- here('tempChart.png') # temporary and ok with always overwriting this - big fan of the 'here' library, BTW
  png(filename = chart_path,
      width = 8.482499,
      height = 3.6515157
  )
  # The chart is created twice.  It is possible to assign it to a variable above?
  chartSeries(timeSeries,
              name = metricName,
              type="line",
              theme=chartTheme('white'))
  
  dev.off() # this tells R to save the chart with the previous settings
  
  # create slide
  slide <- read_pptx("ChartTemplate.pptx") %>%                            # reads in the template
    remove_slide(index = 1) %>%                                           # deletes the first slide. All presentations must have at lease one actual slide to be saved.
    add_slide(layout = "Company Summary", master = "Office Theme Content") %>%    # we add a slide according to the layout we have named "Company Summary" which is under the "Office Theme Content" master
    ph_with(value = external_img(chart_path), location = ph_location_label(ph_label = "Chart Placeholder 5"))  # HARD PART, look below: take the image located at ttm_performance_path and put it at the TTM Perfromance placeholder

  print(slide, here('testSlide.pptx')) # Saves slide
}

# Run the application
shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = dialogViewer("Chart Maker", width = 1000, height = 500))
