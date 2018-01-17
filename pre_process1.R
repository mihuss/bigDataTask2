# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(leaflet)
library(maps)
library(dplyr)
library(tidyr)
library(reshape2)


#
# Read the businesses json file
json_file<-"review1000.json"
biz_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

dates<-biz_dat$date
listdates<-unlist(dates)
alldates<-table(listdates)
days_table<-as.data.frame(alldates)

bizrates<-data.frame(biz_dat$business_id, biz_dat$stars, biz_dat$date)#, biz_dat$time$Tuesday, biz_dat$time$Wednesday, biz_dat$time$Thursday, biz_dat$time$Friday, biz_dat$time$Saturday, biz_dat$time$Sunday, row.names = c(NA, -7L), class = "data.frame")#,I(biz_dat$categories))

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(biz_dat$date, "-", newColNames)
bizrates2 <- cbind(bizrates, newCols)


#Day <- c('Friday', 'Friday','Friday', 'Friday', 'Friday')
#Hour <- c('12:00 ', '13:00', '14:00', '15:00', '16:00')
#value <- c('4 ', '5', '8', '4','10')

#df2 <- data.frame(Day, Hour, value)

cc<-complete.cases(bizrates2)
bizrates2<-bizrates2[cc,]
write.table(bizrates2,"checkins2.dat")
