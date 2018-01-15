library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Read pre-processed data
#
bizrates <- read.table("bizrates.dat")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

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
  output$map <- renderLeaflet({

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
    if (is.null(input$map_bounds))
      return(filteredBiz()[FALSE, ])
    bounds <- input$map_bounds
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
  
  
  
  
  ## Data Explorer ###########################################
  #
  # observe({
  #   cities <- if (is.null(input$states))
  #     character(0)
  #   else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectInput(session,
  #                     "cities",
  #                     choices = cities,
  #                     selected = stillSelected)
  # })
  #
  # observe({
  #   zipcodes <- if (is.null(input$states))
  #     character(0)
  #   else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <-
  #     isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectInput(session,
  #                     "zipcodes",
  #                     choices = zipcodes,
  #                     selected = stillSelected)
  # })
  #
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  #
  # output$ziptable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(
  #       Action = paste(
  #         '<a class="go-map" href="" data-lat="',
  #         Lat,
  #         '" data-long="',
  #         Long,
  #         '" data-zip="',
  #         Zipcode,
  #         '"><i class="fa fa-crosshairs"></i></a>',
  #         sep = ""
  #       )
  #     )
  #   action <- DT::dataTableAjax(session, df)
  #
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}
