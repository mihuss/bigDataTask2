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
json_file<-"yelp_academic_dataset_business.json"
biz_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

#biz_dat

#create a table of categories:
cats<-biz_dat$categories
listcats<-unlist(cats)
allcat<-table(listcats)
category_table<-as.data.frame(allcat)

# we want Restaurants
# 
#restaurants<-grep(pattern="",biz_dat$categories)
#bars<-biz_rest<-biz_dat[restaurants,]
#
# and we want number of stars, latitude, longitude and state
#  features
#
#catego<-fromJSON(biz_dat$categories, simplifyVector = true)

bizrates<-data.frame(biz_dat$business_id,biz_dat$name,biz_dat$stars,biz_dat$longitude,biz_dat$latitude, biz_dat$state,
                     biz_dat$attributes$`Take-out`,biz_dat$attributes$`Takes Reservations`,biz_dat$attributes$`Wi-Fi`,
                     biz_dat$attributes$Caters, biz_dat$review_count)#,I(biz_dat$categories))

#
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