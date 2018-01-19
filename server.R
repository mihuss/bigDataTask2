library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(jsonlite)
#library(dplyr)

# Read pre-processed data

bizrates <-
  read.table(
    "data/bizrates.dat",
    stringsAsFactors = FALSE,
    colClasses = c("biz_rest.review_count" = "numeric")
  )

# reviewsA <- read.table("data/reviewA.dat", stringsAsFactors = FALSE)
# reviewsB <- read.table("data/reviewB.dat", stringsAsFactors = FALSE)
# reviewsC <- read.table("data/reviewC.dat", stringsAsFactors = FALSE)
#
# reviewsTable <- rbind(reviewsA, reviewsB, reviewsC)
#
# checkins <- read.table("data/checkins.dat", stringsAsFactors = FALSE)

reviewsTable <-
  read.table("data/reviewDev.dat", stringsAsFactors = FALSE)
checkins <-
  read.table("data/checkinsDev.dat",
             stringsAsFactors = FALSE,
             header = TRUE)

function(input, output, session) {
  ## Controls / Filters ###########################################
  filteredBiz <- reactive({
    lower <- 1
    upper <- 5
    
    Sbizrates <- bizrates
    
    # filter takeout only
    if (input$takeout) {
      wo <- (Sbizrates$biz_rest.attributes.RestaurantsTakeOut == TRUE)
      Sbizrates <- Sbizrates[wo,]
    }
    
    # filter Reservations only
    if (input$reserve) {
      wo <-
        (Sbizrates$biz_rest.attributes.RestaurantsReservations == TRUE)
      Sbizrates <- Sbizrates[wo,]
    }
    
    # filter WiFi only
    if (input$wifi) {
      wo <- (Sbizrates$biz_rest.attributes.WiFi == "free")
      Sbizrates <- Sbizrates[wo,]
    }
    
    # filter Caters only
    if (input$caters) {
      wo <- (Sbizrates$biz_rest.attributes.Caters == TRUE)
      Sbizrates <- Sbizrates[wo,]
    }
    
    # filter on stars
    lower <- as.numeric(input$num_stars[1])
    wo <- Sbizrates$biz_rest.stars >= lower
    Sbizrates <- Sbizrates[wo,]
    upper <- as.numeric(input$num_stars[2])
    wo <- Sbizrates$biz_rest.stars <= upper
    Sbizrates <- Sbizrates[wo,]
    
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
        lat = filteredBiz()$biz_rest.latitude,
        lng = filteredBiz()$biz_rest.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "Rating: ",
          filteredBiz()$biz_rest.stars,
          "<br>",
          "Name: ",
          filteredBiz()$biz_rest.name,
          "<br>"
        )
      )
  })
  
  
  # A reactive expression that returns the set of restaurants that are
  # in bounds right now
  restaurantsInBounds <- reactive({
    if (is.null(input$mymap_bounds))
      return(filteredBiz()[FALSE,])
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      filteredBiz(),
      biz_rest.latitude >= latRng[1] &
        biz_rest.latitude <= latRng[2] &
        biz_rest.longitude >= lngRng[1] &
        biz_rest.longitude <= lngRng[2]
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
        restaurantsInBounds()$biz_rest.review_count ~ restaurantsInBounds()$biz_rest.stars,
        data = restaurantsInBounds(),
        xlab = "Stars",
        ylab = "Number of Reviews"
      ),
      ylim = range(bizrates$biz_rest.stars),
      xlim = range(bizrates$biz_rest.review_count)
    )
  })
  
  
  
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    
    activeRestaurants <- restaurantsInBounds()
    
    L <- activeRestaurants$biz_rest.latitude == click$lat
    restaurantName <-
      activeRestaurants[L, which(colnames(activeRestaurants) == "biz_rest.name")]
    restaurantBusinessId <-
      activeRestaurants[L, which(colnames(activeRestaurants) == "biz_rest.business_id")]
    
    # print("id: ")
    # print(restaurantBusinessId)
    # print("name: ")
    # print(restaurantName)
    # print("---")
    
    output$restaurantName <- renderText({
      restaurantName
    })
    
    restaurantBusinessId <- "7KPBkxAOEtb3QeIL9PEErg"
    print("Using dev business_id:")
    print(restaurantBusinessId)
    
    print(which(checkins$business_id == restaurantBusinessId))
    
    restaurantCheckins <-
      subset(checkins, business_id == restaurantBusinessId)
    
    if (nrow(restaurantCheckins) == 0) {
      printf("no statistics available")
      return(NULL)
    }
    
    # print("passed length check")
    
    output$visitsPerDay <- renderPlot(res = 100, expr = {
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
      
      visits <- lapply(hoursOfTheDay, function(hour) {
        currentCol <-
          paste("time", input$selectedDay, gsub(":", ".", hour), sep = ".")
        tmpVal <- restaurantCheckins[[currentCol]]
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
        ylim = range(0, max(c(
          5, max(df$visits)
        ))),
        names.arg = df$hoursOfTheDay,
        las = 2
      )
    })
    
    filterForReviewPerYear <- reactive({
      sReviewsTable <- reviewsTable
      
      # filter on business
      wo <-
        sReviewsTable$review_dat.business_id == restaurantBusinessId
      sReviewsTable <- sReviewsTable[wo,]
      
      print("filterForReviewPerYear:")
      print(nrow(sReviewsTable))
      
      # filter on stars
      lower <- as.numeric(input$reviewsPerYearNumStars[1])
      wo <- sReviewsTable$review_dat.stars >= lower
      sReviewsTable <- sReviewsTable[wo,]
      upper <- as.numeric(input$reviewsPerYearNumStars[2])
      wo <- sReviewsTable$review_dat.stars <= upper
      sReviewsTable <- sReviewsTable[wo,]
      
    })
    
    output$reviewsPerYear <- renderPlot(res = 100, expr = {
      p <-
        ggplot(filterForReviewPerYear(),
               aes(filterForReviewPerYear()$Year)) + geom_histogram(binwidth = 0.5)
      p <- p + xlab("Year") + ylab("Number of Reviews")
      p
      #print(filterForReviewPerYear())
      # ggplot(filterForReviewPerYear(),
      #         aes(filterForReviewPerYear()$Year),
      #        ylab = "Number of Checkins",
      #        xlab = "Hour")
      # + geom_histogram(binwidth = 0.5)
      # + aes_string(color="blue")
      # + geom_freqpoly(binwidth = 1, alpha = 0.5 )
    })
    
    #output$marker_lat <- click$lat
    #output$marker_lng <- click$lng
    #text<-paste("Stars ", click$biz_rest.stars)
    #text2<-paste("You've selected point ", click$biz_rest.stars)
    #mymap$clearPopups()
    #map$showPopup( click$biz_rest.stars, text2)
    updateNavbarPage(session, "nav", "Data explorer")
  })
  
}
