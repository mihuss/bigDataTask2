library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(jsonlite)
library(dplyr)

# Read pre-processed data

bizrates <-
  read.table(
    "data/bizrates.dat",
    stringsAsFactors = FALSE,
    colClasses = c("biz_rest.review_count" = "numeric")
  )

reviewsTable <-
  read.table("data/review.dat", stringsAsFactors = FALSE)
checkins <-
  read.table("data/checkins.dat",
             stringsAsFactors = FALSE,
             header = TRUE)

function(input, output, session) {
  ## default value for businessId
  output$restaurantBusinessId <- renderText('NoRestaurantSelected')
  outputOptions(output, "restaurantBusinessId", suspendWhenHidden = FALSE)
  
  ## Controls / Filters ###########################################
  filteredBiz <- reactive({
    lower <- 1
    upper <- 5
    
    Sbizrates <- bizrates
    
    # filter takeout only
    if (input$takeout) {
      wo <- (Sbizrates$biz_rest.attributes.RestaurantsTakeOut == TRUE)
      Sbizrates <- Sbizrates[wo, ]
    }
    
    # filter Reservations only
    if (input$reserve) {
      wo <-
        (Sbizrates$biz_rest.attributes.RestaurantsReservations == TRUE)
      Sbizrates <- Sbizrates[wo, ]
    }
    
    # filter WiFi only
    if (input$wifi) {
      wo <- (Sbizrates$biz_rest.attributes.WiFi == "free")
      Sbizrates <- Sbizrates[wo, ]
    }
    
    # filter Caters only
    if (input$caters) {
      wo <- (Sbizrates$biz_rest.attributes.Caters == TRUE)
      Sbizrates <- Sbizrates[wo, ]
    }
    
    # filter on stars
    lower <- as.numeric(input$num_stars[1])
    wo <- Sbizrates$biz_rest.stars >= lower
    Sbizrates <- Sbizrates[wo, ]
    upper <- as.numeric(input$num_stars[2])
    wo <- Sbizrates$biz_rest.stars <= upper
    Sbizrates <- Sbizrates[wo, ]
    
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
    filteredBizStatic <- filteredBiz()
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      
      addMarkers(
        lat = filteredBizStatic$biz_rest.latitude,
        lng = filteredBizStatic$biz_rest.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "Rating: ",
          filteredBizStatic$biz_rest.review_count,
          "<br>",
          "Name: ",
          filteredBizStatic$biz_rest.name,
          "<br>"
        )
      )
  })
  
  
  # Adding markers like this will prevent zoom level to be reset when changing map filters
  observe({
    restaurantsInBoundsStatic <- restaurantsInBounds()
    filteredBizStatic <- filteredBiz()
    mymap <- leafletProxy("mymap") %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addMarkers(
        lat = filteredBizStatic$biz_rest.latitude,
        lng = filteredBizStatic$biz_rest.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "<b>",
          "Name: ",
          "</b>",
          filteredBizStatic$biz_rest.name,
          "<br>",
          "<b>",
          "Rating: ",
          "</b>",
          filteredBizStatic$biz_rest.stars,
          "Stars",
          "<br>",
          "<b>",
          "Opening hours: ",
          "</b>",
          "<br>",
          "Monday: ",
          filteredBizStatic$biz_rest.hours.Monday,
          "<br>",
          "Tuesday: ",
          filteredBizStatic$biz_rest.hours.Tuesday,
          "<br>",
          "Wednesday: ",
          filteredBizStatic$biz_rest.hours.Wednesday,
          "<br>",
          "Thursday: ",
          filteredBizStatic$biz_rest.hours.Thursday,
          "<br>",
          "Friday: ",
          filteredBizStatic$biz_rest.hours.Friday,
          "<br>",
          "Saturday: ",
          filteredBizStatic$biz_rest.hours.Saturday,
          "<br>",
          "Sunday: ",
          filteredBizStatic$biz_rest.hours.Sunday,
          "<br>"
        ),
        group = "Restaurants"
      ) %>%
      clearHeatmap()
      
      if(nrow(restaurantsInBoundsStatic) > 0) {
        mymap %>% addHeatmap(lng = restaurantsInBoundsStatic$biz_rest.longitude, lat = restaurantsInBoundsStatic$biz_rest.latitude,
                   minOpacity = 0.4, max = max(restaurantsInBoundsStatic$num_checkins), intensity = restaurantsInBoundsStatic$num_checkins,
                   gradient = "YlOrRd", radius = 25, blur = 20, data = restaurantsInBoundsStatic, group = "Heatmap Check-ins") %>%
        clearControls() %>%
        addLayersControl(
          overlayGroups = c("Heatmap Check-ins", "Restaurants"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topleft"
        ) %>%
        addLegend("bottomleft", pal = colorNumeric(
            palette = "YlOrRd",
            domain = restaurantsInBoundsStatic$num_checkins
          )
          , values = restaurantsInBoundsStatic$num_checkins,
          title = "Check-Ins",
          opacity = 1)
     }

  })

  # A reactive expression that returns the set of restaurants that are
  # in bounds right now
  restaurantsInBounds <- reactive({
    filteredBizStatic = filteredBiz()
    if (is.null(input$mymap_bounds))
      return(filteredBizStatic[FALSE, ])
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      filteredBizStatic,
      biz_rest.latitude >= latRng[1] &
        biz_rest.latitude <= latRng[2] &
        biz_rest.longitude >= lngRng[1] &
        biz_rest.longitude <= lngRng[2]
    )
  })
  
  output$overviewRestaurants <- renderText({
    paste("Shown Restaurants: ",
          nrow(restaurantsInBounds()),
          "of ",
          nrow(bizrates))
  })
  
  output$scatterStarsReviewCount <- renderPlot({
    
    print(
      xyplot(
        biz_rest.review_count ~ biz_rest.stars,
        data = bizrates,
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
    
    #restaurantBusinessId <- "7KPBkxAOEtb3QeIL9PEErg"
    print("Using dev business_id:")
    print(restaurantBusinessId)
    
    output$restaurantBusinessId <- renderText(restaurantBusinessId)
    
    print(which(checkins$biz_rest.business_id == restaurantBusinessId))
    
    restaurantCheckins <-
      subset(checkins, biz_rest.business_id == restaurantBusinessId)
    
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
      
      dfVisitsPerDay <-
        data.frame(hoursOfTheDay = hoursOfTheDay, visits = unlist(visits))
      
      # Render a barplot
      p <-
        ggplot(data = dfVisitsPerDay, aes(x = hoursOfTheDay, y = visits)) +
        geom_bar(stat = "identity", fill = "steelblue")
      p <- p + labs(x = "Hour", y = "Number of Check-ins")
      p <-
        p + theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 0
        ))
      p <- p + scale_x_discrete(limits = hoursOfTheDay)
      p <-
        p + ylim(low = 0, high = max(c(5, max(
          dfVisitsPerDay$visits
        ))))
      p
    })
    
    filterForReviewPerYear <- reactive({
      sReviewsTable <- reviewsTable
      
      # filter on business
      wo <-
        sReviewsTable$biz_rest.business_id == restaurantBusinessId
      sReviewsTable <- sReviewsTable[wo, ]
      
    })
    
    output$reviewsPerYearNormalized <-
      renderPlot(res = 100, expr = {
        p <- ggplot()
        p <- p + geom_bar(
          aes(
            y = 1,
            x = Year,
            fill = factor(review_dat.stars, levels=c("5","4","3","2","1"))
          ),
          data = filterForReviewPerYear(),
          stat = "identity",
          position = "fill"
        )
        p <-
          p + labs(x = "Year", y = "Normalized Share of Reviews", fill = "Stars")
        p <- p + scale_y_continuous(labels = percent)
        p
      })
    
    output$reviewsPerYearRegular <- renderPlot(res = 100, expr = {
      p <- ggplot()
      p <- p + geom_bar(
        aes(
          y = 1,
          x = filterForReviewPerYear()$Year,
          fill = factor(review_dat.stars, levels=c("5","4","3","2","1"))
        ),
        data = filterForReviewPerYear(),
        stat = "identity"
      )
      p <-
        p + labs(x = "Year", y = "Number of Reviews", fill = "Stars")
      p
    })
    
  })
  
  ####### average per state #########
  
  output$avgRatingsByState <- renderPlot(res = 100, expr = {
    dataGroupByStateStar <- bizrates %>%
      filter(biz_rest.state != '') %>% filter(biz_rest.state != "01") %>%
      group_by(biz_rest.state, biz_rest.stars)
    
    dataWeightedGroupByStateStar <- dataGroupByStateStar %>%
      summarise(totalByStar = n()) %>% arrange(desc(biz_rest.stars)) %>%
      mutate(total = sum(totalByStar)) %>% mutate(percent = round((totalByStar / total) *
                                                                    100, 1)) 
    p <-
      ggplot(
        dataWeightedGroupByStateStar,
        aes(x = biz_rest.state, y = biz_rest.stars, label = percent)
      )
    p <-
      p + geom_point(aes(
        size = percent * 2,
        colour = biz_rest.stars
        ), alpha = 0.5)
    p <- p + geom_text(hjust = 0.4, size = 4)
    p <- p + scale_size(range = c(1, 30), guide = "none")
    p <- p + scale_color_gradient(low = "red", high = "green")
    p <-
      p + labs(x = "State", y = "Avg. Rating", color = "Stars")
    p <-
      p + scale_y_continuous(breaks = seq(1, 5, 0.5))
    p
    
  })
  
  output$totalRatingsByState <- renderPlot(res = 100, expr = {
    dataGroupByStateStar <- bizrates %>%
      filter(biz_rest.state != '') %>% filter(biz_rest.state != "01") %>%
      group_by(biz_rest.state, biz_rest.stars)
    
    dataWeightedGroupByStateStar <- dataGroupByStateStar %>%
      summarise(totalByStar = n()) %>% arrange(desc(biz_rest.stars)) %>%
      mutate(total = sum(totalByStar))
    
    p <-
      ggplot(
        dataWeightedGroupByStateStar,
        aes(x = biz_rest.state, y = biz_rest.stars, label = totalByStar)
      )
    p <-
      p + geom_point(aes(
        size = totalByStar * 2,
        colour = biz_rest.stars
      ), alpha = 0.5)
    p <- p + geom_text(hjust = 0.4, size = 4)
    p <- p + scale_size(range = c(1, 30), guide = "none")
    p <- p + scale_color_gradient(low = "red", high = "green")
    p <-
      p + labs(x = "State", y = "Total Rating", color = "Stars")
    p <-
      p + scale_y_continuous(breaks = seq(1, 5, 0.5))
    p
    
  })
  
}
