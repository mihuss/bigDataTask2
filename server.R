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

# reviewsA <- read.table("data/reviewA.dat", stringsAsFactors = FALSE)
# reviewsB <- read.table("data/reviewB.dat", stringsAsFactors = FALSE)
# reviewsC <- read.table("data/reviewC.dat", stringsAsFactors = FALSE)
# 
# reviewsTable <- rbind(reviewsA, reviewsB, reviewsC)
# 
# checkins <- read.table("data/checkins.dat", stringsAsFactors = FALSE)

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
  
  
  # Adding markers like this will prevent zoom level to be reset when changing map filters
  observe({
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addMarkers(
        lat = filteredBiz()$biz_rest.latitude,
        lng = filteredBiz()$biz_rest.longitude,
        clusterOptions = markerClusterOptions(),
        popup = paste(
          "<b>",
          "Name: ",
          "</b>",
          filteredBiz()$biz_rest.name,
          "<br>",
          "<b>",
          "Rating: ",
          "</b>",
          filteredBiz()$biz_rest.stars,
          "Stars",
          "<br>",
          "<b>",
          "Opening hours: ",
          "</b>",
          "<br>",
          "Monday: ",
          filteredBiz()$biz_rest.hours.Monday,
          "<br>",
          "Tuesday: ",
          filteredBiz()$biz_rest.hours.Tuesday,
          "<br>",
          "Wednesday: ",
          filteredBiz()$biz_rest.hours.Wednesday,
          "<br>",
          "Thursday: ",
          filteredBiz()$biz_rest.hours.Thursday,
          "<br>",
          "Friday: ",
          filteredBiz()$biz_rest.hours.Friday,
          "<br>",
          "Saturday: ",
          filteredBiz()$biz_rest.hours.Saturday,
          "<br>",
          "Sunday: ",
          filteredBiz()$biz_rest.hours.Sunday,
          "<br>"
        ),
        group = "Restaurants"
      ) %>%
      addHeatmap(lng = restaurantsInBounds()$biz_rest.longitude, lat = restaurantsInBounds()$biz_rest.latitude,
                 minOpacity = 0.4, max = max(restaurantsInBounds()$num_checkins), intensity = restaurantsInBounds()$num_checkins,
                 gradient = "YlOrRd", radius = 25, blur = 20, data = bizrates, group = "Heatmap Check-ins")
    
  })
  
  observeEvent(input$mymap_groups,{
    mymap <- leafletProxy("mymap")
    mymap %>% clearControls()
    mymap %>% addLayersControl(
      overlayGroups = c("Heatmap Check-ins", "Restaurants"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topleft"
    )
    if (input$mymap_groups[1] == "Heatmap Check-ins" | length(input$mymap_groups) == 2) {
      mymap %>% addLegend("bottomleft", pal = colorNumeric(
        palette = "YlOrRd",
        domain = restaurantsInBounds()$num_checkins
      ) 
      , values = restaurantsInBounds()$num_checkins,
      title = "Check-Ins",
      opacity = 1)
    }
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
        #restaurantsInBounds()$biz_rest.review_count ~ restaurantsInBounds()$biz_rest.stars,
        data = bizrates,
        #data = restaurantsInBounds(),
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
      print("1")
      p <- p + labs(x = "Hour", y = "Number of Check-ins")
      print("2")
      p <-
        p + theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 0
        ))
      print("3")
      p <- p + scale_x_discrete(limits = hoursOfTheDay)
      print("4")
      p <-
        p + ylim(low = 0, high = max(c(5, max(
          dfVisitsPerDay$visits
        ))))
      print("5")
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
      mutate(tsum = n()) %>% group_by(biz_rest.state, biz_rest.stars)
    
    dataWeightedGroupByStateStar <- dataGroupByStateStar %>%
      summarise(totalByStar = n()) %>% arrange(desc(biz_rest.stars)) %>%
      mutate(total = sum(totalByStar)) %>% mutate(percent = round((totalByStar / total) *
                                                                    100, 1)) %>%
      mutate(percentWeight = percent, 1)
    
    p <-
      ggplot(
        dataWeightedGroupByStateStar,
        aes(x = biz_rest.state, y = biz_rest.stars, label = percent)
      )
    p <-
      p + geom_point(aes(
        size = percentWeight * 2,
        colour = biz_rest.stars,
        alpha = 0.05
      ))
    p <- p + geom_text(hjust = 0.4, size = 4)
    p <- p + scale_size(range = c(1, 30), guide = "none")
    p <- p + scale_color_gradient(low = "darkblue", high = "red")
    p <-
      p + labs(title = "A grid of detailed avg.ratings by state ", x = "State", y = "Detailed Avg.Ratings")
    p <-
      p + scale_y_continuous(breaks = seq(1, 5, 0.5)) + theme(legend.title = element_blank())
    p
    
  })
  
}
