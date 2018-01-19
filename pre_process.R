# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(leaflet)
library(maps)
#
# Read the businesses json file
json_file<-"business.json"
biz_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

#biz_dat

#create a table of categories:
cats<-biz_dat$categories
listcats<-unlist(cats)
allcat<-table(listcats)
category_table<-as.data.frame(allcat)

# we want Restaurants
# 
restaurants<-grep(pattern="",biz_dat$categories)
biz_rest<-biz_dat[restaurants,]
#
# and we want number of stars, latitude, longitude and state
#  features
#
#catego<-fromJSON(biz_dat$categories, simplifyVector = true)

bizrates<-data.frame(biz_rest$business_id, biz_rest$name, biz_rest$stars,biz_rest$longitude,biz_rest$latitude, biz_rest$state,
                     biz_rest$attributes$RestaurantsTakeOut,biz_rest$attributes$RestaurantsReservations,biz_rest$attributes$WiFi,
                     biz_rest$attributes$Caters,biz_rest$review_count)

cc<-complete.cases(bizrates)
bizrates<-bizrates[cc,]
write.table(bizrates,"bizrates.dat")
#
# Make a map
#
# #mymap<-leaflet() %>% 
#   addTiles() %>% 
#   addTiles(urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png")  %>%  
#   mapOptions(zoomToLimits="always") %>% 
#   addMarkers(lat=bizrates$biz_dat.latitude,lng=bizrates$biz_dat.longitude,
#              clusterOptions = markerClusterOptions(),popup=bizrates$biz_dat.business_id) 

#
# display map
#
#mymap

# Gather some metrics
#num_biz<-nrow(biz_dat)
#num_rest<-nrow(bars)
#num_complete<-nrows(bizrates)
# plot
#barplot(c(num_biz,num_rest,num_complete), main="Number of Business",names.arg=c("Businesses","Restaurants","Full Service"))