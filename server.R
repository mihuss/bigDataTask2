library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
#library(dplyr)

# Read pre-processed data
#
bizrates <- read.table("bizrates.dat")

json_file <- "checkin.json"
checkins <-
  fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse = ",")))

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
      # wo <- (Sbizrates$biz_dat.attributes..Take.out. == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    # filter Reservations only
    if (input$reserve)
      wo <-
      (Sbizrates$biz_dat.attributes..Takes.Reservations. == TRUE)
    else
      # wo <-
      # (Sbizrates$biz_dat.attributes..Takes.Reservations. == FALSE)
    Sbizrates <- Sbizrates[wo, ]
    
    # filter WiFi only
    if (input$wifi)
      wo <- (Sbizrates$biz_dat.attributes..Wi.Fi. == "free")
    else
      # wo <- (Sbizrates$biz_dat.attributes..Wi.Fi. == FALSE)
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
  
  
  
  observeEvent(input$mymap_marker_click,{
    click <- input$mymap_marker_click

    activeRestaurants <- restaurantsInBounds()
    
    # currentBusinessIdFrame <- activeRestaurants %>% filter(biz_dat.latitude == click$lat) %>% select(biz_dat.business_id)
    # currentBusinessId <- currentBusinessIdFrame[,]
    
    L = activeRestaurants$biz_dat.latitude == click$lat
    currentBusinessId <- as.character(activeRestaurants[L,]$biz_dat.business_id[1])
    currentBusinessId <- unlist(strsplit(currentBusinessId, " ")[1])
    
    print("id: ")
    print(currentBusinessId)
    print("---")

    output$console <- renderPrint({
      print(currentBusinessId)
    })
    
    #currentBusinessId <- "EeM2Zfji_KrjcQorUgG74A"
    
    print(which(checkins$business_id == currentBusinessId))
    
    myBusiness <-
      subset(checkins, business_id == currentBusinessId)
    #subset(checkins, business_id == "kREVIrSBbtqBhIYkTccQUg")

    if(nrow(myBusiness) == 0){
      return(NULL)
    }
    
    print("passed length check")
    
    output$visitsPerDay <- renderPlot({
      

      
      myBusinessDays <-
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
        tmpVal <- myBusinessDays[, which(colnames(myBusinessDays) == x)]
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
    
    #output$marker_lat <- click$lat
    #output$marker_lng <- click$lng
    #text<-paste("Stars ", click$biz_dat.stars)
    #text2<-paste("You've selected point ", click$biz_dat.stars)
    #mymap$clearPopups()
    #map$showPopup( click$biz_dat.stars, text2)
    updateNavbarPage(session, "nav", "Data explorer")
  })
}
