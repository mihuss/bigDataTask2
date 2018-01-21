library(shiny)
library(leaflet)
library(ggplot2)

navbarPage(
  "Yelp Analyzer",
  id = "nav",
  
  tabPanel(
    "Look for a restaurant",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("mymap", width = "100%", height = "100%"),
      #leafletOutput("heatmapReviews", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = FALSE,
        top = 120,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        
        h3('Search Parameters'),
        sliderInput(
          "num_stars",
          label = "Number of Stars",
          min = 1,
          max = 5,
          value = c(1, 5)
        ),
        
        checkboxInput("reserve", label = "Takes Reservations", value =
                        TRUE),
        checkboxInput("takeout", label = "Take-out", value = TRUE),
        checkboxInput("wifi", label = "Free Wi-Fi", value = TRUE),
        checkboxInput("caters", label = "Caters", value = TRUE),
        
        textOutput("overviewRestaurants"),
        
        #### select plots
        
        conditionalPanel(
          condition = "output.restaurantBusinessId == 'NoRestaurantSelected'",
          h3("Please select a restaurant.")
        ),
        conditionalPanel(
          condition = "output.restaurantBusinessId != 'NoRestaurantSelected'",
          selectInput("selectPlot", "Select Plot",
                      choices = list("Visits per Day" = "visitsPerDay", 
                                     "Reviews per Year (reg.)" = "reviewsPerYearReg",
                                     "Reviews per Year (norm.)" = "reviewsPerYearNorm"
                                ), 
                      selected = 1
          ),
          
          conditionalPanel(
            condition = "input.selectPlot == 'visitsPerDay'",
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
            plotOutput("visitsPerDay", height = 250, width = 300)
          ),
          
          conditionalPanel(
            condition = "input.selectPlot == 'reviewsPerYearReg'",
            plotOutput("reviewsPerYearRegular", height = 250, width = 300)
          ),
          
          conditionalPanel(
            condition = "input.selectPlot == 'reviewsPerYearNorm'",
            plotOutput("reviewsPerYearNormalized", height = 250, width = 300)
          )
          
        )
        
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
    "Inspect Dataset",
    navlistPanel(widths = c(2, 6),
      "Available Plots",
      tabPanel("Stars By Number of Reviews",
                  plotOutput("scatterStarsReviewCount", height = 500, width = 500)
              ),
      tabPanel("Ratings By State",
               radioButtons("avgRatingsByStateRadio", h3("Select the plot"),
                            choices = list("Avg. Ratings By State" = 1, "Total Ratings By State" = 2), selected = 1),
               
               conditionalPanel(
                 condition = "input.avgRatingsByStateRadio == 1",
                 plotOutput("avgRatingsByState", height = 500, width = 900)
               ),
               
               conditionalPanel(
                 condition = "input.avgRatingsByStateRadio == 2",
                 plotOutput("totalRatingsByState", height = 500, width = 900)
               )
      )
    )
  )
)