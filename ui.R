library(shiny)
library(leaflet)
library(ggplot2)

navbarPage(
  "Yelp",
  id = "nav",
  
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("mymap", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = FALSE,
        top = 60,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        
        h3('Search Parameters'),
        sliderInput(
          "num_stars",
          label = h3("Number of Stars"),
          min = 1,
          max = 5,
          value = c(1, 5)
        ),
        
        checkboxInput("reserve", label = "Takes Reservations", value =
                        TRUE),
        checkboxInput("takeout", label = "Take-out", value = TRUE),
        checkboxInput("wifi", label = "Free Wi-Fi", value = TRUE),
        checkboxInput("caters", label = "Caters", value = TRUE),
        plotOutput("scatterStarsReviewCount", height = 250)
      ),
      
      tags$div(
        id = "cite",
        'Universidad Politécnica de Madrid',
        tags$em('Course Work Big Data'),
        ' by Carolina Echavarria and Michael Hußnätter (Madrid, 2018).'
      )
    )
  ),
  
  tabPanel(
    "Data explorer",
    titlePanel(textOutput("restaurantName")),
    navlistPanel(widths = c(2, 6),
      "Header A",
      tabPanel("Checkins per Day",
               fluidRow(column(
                 4,
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
                 ),
                 plotOutput("visitsPerDay", height = 500, width = 500)
               ))),
      tabPanel("Component 2"),
      "Header B",
      tabPanel("Reviews per Year",
               fluidRow(column(
                 4,
                 radioButtons("reviewsPerYearRadio", h3("Select the plot"),
                              choices = list("Count of reviews" = 1, "Normalized" = 2), selected = 1),
                 
                 conditionalPanel(
                   condition = "input.reviewsPerYearRadio == 1",
                   plotOutput("reviewsPerYearNormalized", height = 500, width = 500)
                 ),
                 
                 conditionalPanel(
                   condition = "input.reviewsPerYearRadio == 2",
                   plotOutput("reviewsPerYearRegular", height = 500, width = 500)
                 )
               ))),
      tabPanel("Component 4"),
      "-----",
      tabPanel("Component 5")
    )
  )
)