# Chart Library 
# By: AESoto
# 20 April 2022

chartLibraries <- c('tidyverse', 'highcharter', 'tidyquant', 'timetk', 'shinyWidgets', 'shiny' )
lapply(chartLibraries, require, character.only=TRUE)

############################
# Load data sets
# TODO: expand data load....

# 10 Yr Yield
tenYr <- read.csv("DGS10.csv")
tenYr <- tenYr[!(tenYr$DGS10=='.'),]

############################



# Define UI for application that draws charts
ui <- fluidPage(

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
  
  actionButton("go", "Submit"),
  
  # Generate chart
  mainPanel(
     plotOutput("distPlot")
  )
)


# Define server logic required to draw chart
server <- function(input, output) {
  timeSeries <- eventReactive(input$go, {
    prices <- switch(
      input$selected,
      "10Yr Yield" = tenYr,
      "Something1" = "Something1",
      "Something2" = "Something2"
    )
    startDate <- input$start
    endDate <- input$end
    timeSeries <- prices %>%        # needs to be enhanced for other choices
      select(DATE, DGS10) %>%
      filter(DATE > startDate & DATE < endDate)
  })  # closes timeSeries function....

  output$distPlot <- renderPlot({
    timeSeries() %>%
      ggplot(aes(x = DGS10)) + 
    geom_density(size = 1, color = "blue")
  }) # closes renderPlot function
}

# Run the application 
shinyApp(ui = ui, server = server)
