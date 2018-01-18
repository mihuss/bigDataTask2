




# Define UI for app that draws a histogram ----
ui <- fluidPage(# Give the page a title
  titlePanel("Telephones Around the World"),
  
  sidebarLayout(# Define the sidebar with one input
    sidebarPanel(
      selectInput(
        "selectedDay",
        "Select Day:",
        c(
          "Monday" = "Monday",
          "Tuesday" = "Tuesday",
          "Wednesday" = "Wednesday",
          "Thursday" = "Thursday",
          "Friday" = "Friday",
          "Saturday" = "Saturday",
          "Sunday" = "Sunday"
        )
      )
    ),
    
    # Create a spot for the barplot
    mainPanel(plotOutput("visitsPerDay"))))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  json_file <- "checkin1000.json"
  checkins <-
    fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse = ",")))
  

    output$visitsPerDay <- renderPlot({
    myBusiness <-
      subset(checkins, business_id == "kREVIrSBbtqBhIYkTccQUg")
    
    #print(which(colnames(myBusiness$time) == input$selectedDay))
    
    myThursday <-
      cbind(myBusiness$business_id, myBusiness$time[, which(colnames(myBusiness$time) == input$selectedDay)])
    
    hoursOfTheDay <- c(
      "0:00",
      "1:00",
      "2:00",
      "3:00",
      "4:00",
      "5:00",
      "6:00",
      "7:00",
      "8:00",
      "9:00",
      "10:00",
      "11:00",
      "12:00",
      "13:00",
      "14:00",
      "15:00",
      "16:00",
      "17:00",
      "18:00",
      "19:00",
      "20:00",
      "21:00",
      "22:00",
      "23:00"
    )
    
    visits <- lapply(hoursOfTheDay, function(x) {
      tmpVal <- myThursday[, which(colnames(myThursday) == x)]
      if (is.na(tmpVal)) {
        0
      } else {
        tmpVal
      }
    })
    
    df <-
      data.frame(hoursOfTheDay = hoursOfTheDay, visits = unlist(visits))
    
    # Render a barplot
    barplot(
      df$visits,
      main = "Total Checkins per Hour",
      ylab = "Number of Checkins",
      xlab = "Hour",
      ylim = range(0, max(c(5,max(df$visits)))),
      names.arg = df$hoursOfTheDay,
      las = 2
    )
  })
  
}

shinyApp(ui, server)