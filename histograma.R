
library(shiny)

# Read pre-processed data
#
bcl <- read.table("checkins2.dat", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel(""),
  
  fluidRow(
    column(3, 
           textInput("business", h3("Select the business"), 
                     value = "Business id...")),
    column(3, 
           sliderInput("num_year", "Select the year interval",
                       min = 2000, max = 2017, value = c(2015, 2017))
    ) 

  ),
  
  fluidRow(
  column(3, 
         sliderInput("num_stars", "Select the Number of stars",
                     min = 1, max = 5, value = c(4, 5))
  )
  ),
  
  fluidRow(
    "Data explorer",
    plotOutput("coolplot", height = 250))
)

server <- function(input, output) {

  filteredBiz <- reactive({
    lower <- 2000
    upper <- 2017
    
    Sbizrates <- bcl
    
    # filter on business
    bus <- (input$business)
    wo <- Sbizrates$biz_dat.business_id == bus
    Sbizrates <- Sbizrates[wo, ]
    
    # filter on year
    lower <- as.numeric(input$num_year[1])
    wo <- Sbizrates$Year >= lower
    Sbizrates <- Sbizrates[wo, ]
    upper <- as.numeric(input$num_year[2])
    wo <- Sbizrates$Year <= upper
    Sbizrates <- Sbizrates[wo, ]
    
    # filter on stars
    lower <- as.numeric(input$num_stars[1])
    wo <- Sbizrates$biz_dat.stars >= lower
    Sbizrates <- Sbizrates[wo, ]
    upper <- as.numeric(input$num_stars[2])
    wo <- Sbizrates$biz_dat.stars <= upper
    Sbizrates <- Sbizrates[wo, ]
    
  })
  
  output$coolplot <- renderPlot({
    print(filteredBiz())
    ggplot(filteredBiz(), aes(filteredBiz()$Year)) + geom_histogram() 
  })
}

shinyApp(ui = ui, server = server)