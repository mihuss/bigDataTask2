library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Read pre-processed data
#
bizrates <- read.table("bizrates.dat")

function(input, output, session) {
  ## Controls / Filters ###########################################
  filteredBiz <- reactive({
    lower <- 1
    upper <- 5
    
    Sbizrates = bizrates
    
    # filter takeout only
    if (input$takeout)
      wo <- (Sbizrates$biz_dat.attributes..Take.out. == TRUE)
    else
      wo <- (Sbizrates$biz_dat.attributes..Take.out. == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    # filter Reservations only
    if (input$reserve)
      wo <-
      (Sbizrates$biz_dat.attributes..Takes.Reservations. == TRUE)
    else
      wo <-
      (Sbizrates$biz_dat.attributes..Takes.Reservations. == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    # filter WiFi only
    if (input$wifi)
      wo <- (Sbizrates$biz_dat.attributes..Wi.Fi. == "free")
    else
      wo <- (Sbizrates$biz_dat.attributes..Wi.Fi. == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    
    # filter Caters only
    if (input$caters)
      wo <- (Sbizrates$biz_dat.attributes.Caters == TRUE)
    else
      wo <- (Sbizrates$biz_dat.attributes.Caters == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    # filter on stars
    lower <- as.numeric(input$num_stars[1])
    wo <- Sbizrates$biz_dat.stars >= lower
    Sbizrates <- Sbizrates[wo, ]
    upper <- as.numeric(input$num_stars[2])
    wo <- Sbizrates$biz_dat.stars <= upper
    Sbizrates <- Sbizrates[wo, ]
    
  })
  
  ## Interactive Map ###########################################
  # Create the map
  output$mymap <- renderLeaflet({
    # generate map
    leaflet() %>%
      addTiles() %>%
      addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png")  %>%
      mapOptions(zoomToLimits = "always") %>%
      
      addMarkers(
        lat = filteredBiz()$biz_dat.latitude,
        lng = filteredBiz()$biz_dat.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "Rating: ",
          filteredBiz()$biz_dat.stars,
          "<br>",
          "Name: ",
          filteredBiz()$biz_dat.name,
          "<br>"
        )
      )
  })
  
  
  # A reactive expression that returns the set of restaurants that are
  # in bounds right now
  restaurantsInBounds <- reactive({
    if (is.null(input$mymap_bounds))
      return(filteredBiz()[FALSE, ])
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      filteredBiz(),
      biz_dat.latitude >= latRng[1] &
        biz_dat.latitude <= latRng[2] &
        biz_dat.longitude >= lngRng[1] &
        biz_dat.longitude <= lngRng[2]
    )
  })
  
  output$scatterStarsReviewCount <- renderPlot({
    # If no restaurants are in view, don't plot
    if (nrow(restaurantsInBounds()) == 0)
      return(NULL)
    
    cat(file = stderr(),
        "num restaurants:",
        nrow(restaurantsInBounds()),
        "\n")
    
    print(
      xyplot(
        restaurantsInBounds()$biz_dat.review_count ~ restaurantsInBounds()$biz_dat.stars,
        data = restaurantsInBounds()
      ),
      ylim = range(bizrates$biz_dat.stars),
      xlim = range(bizrates$biz_dat.review_count)
    )
  })
}
