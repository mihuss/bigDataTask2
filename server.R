library(leaflet)
library(leaflet.extras)
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
      addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png")#  %>%
  })
  
  output$heatmapReviews <- renderLeaflet({
    
    # generate map
    leaflet() %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%

      addMarkers(
        lat = filteredBiz()$biz_rest.latitude,
        lng = filteredBiz()$biz_rest.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "Rating: ",
          filteredBiz()$biz_rest.review_count,
          "<br>",
          "Name: ",
          filteredBiz()$biz_rest.name,
          "<br>"
        )
      )
  })
  
  observe({
    print(max(filteredBiz()$biz_rest.review_count))
    leafletProxy("heatmapReviews") %>%
    addHeatmap(lng = filteredBiz()$biz_rest.longitude, lat = filteredBiz()$biz_rest.latitude, 
               minOpacity = 0.4, max = max(filteredBiz()$biz_rest.review_count), intensity = filteredBiz()$biz_rest.review_count,
               gradient = "YlOrRd", radius = 25, blur = 20, data = filteredBiz())
    
  })
  

  
  # Adding markers like this will prevent zoom level to be reset when changing map filters
  observe({
    leafletProxy("mymap") %>%
      clearShapes() %>%
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
      p<-ggplot(data=df, aes(x=hoursOfTheDay, y=visits)) +
        geom_bar(stat="identity", fill="steelblue")
      p <- p + labs(x ="Hour", y= "Number of Check-ins")
      p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))
      p <- p + scale_x_discrete(limits=hoursOfTheDay)
      p <- p + ylim(low = 0, high = max(c(5, max(df$visits))))
      p
      # barplot(
      #   df$visits,
      #   main = "Total Checkins per Hour",
      #   ylab = "Number of Checkins",
      #   xlab = "Hour",
      #   ylim = range(0, max(c(
      #     5, max(df$visits)
      #   ))),
      #   names.arg = df$hoursOfTheDay,
      #   las = 2
      # )
    })
    
    filterForReviewPerYear <- reactive({
      sReviewsTable <- reviewsTable
      
      # filter on business
      wo <-
        sReviewsTable$review_dat.business_id == restaurantBusinessId
      sReviewsTable <- sReviewsTable[wo,]
      
    })
    
    output$reviewsPerYearNormalized <- renderPlot(res = 100, expr = {
      p <- ggplot() 
      p <- p + geom_bar(aes(y = 1, #as.character(review_dat.stars), 
                            x = Year, fill = as.character(review_dat.stars)), 
                        data = filterForReviewPerYear(),
                        stat="identity", position = "fill")
      p <- p + labs(x ="Year", y= "Normalized Share of Reviews", fill = "Stars")
      p <- p + scale_y_continuous(labels = percent)
      p
    })
    
    output$reviewsPerYearRegular <- renderPlot(res = 100, expr = {
      p <- ggplot() 
      p <- p + geom_bar(aes(y = 1, 
                            x = filterForReviewPerYear()$Year, fill = as.character(review_dat.stars)), 
                        data = filterForReviewPerYear(),
                        stat="identity")
      p <- p + labs(x ="Year", y= "Number of Reviews", fill = "Stars")
      p
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
