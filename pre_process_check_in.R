# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(leaflet)
library(maps)
library(data.table)
#library(reshape2)
#
# Read the businesses json file
json_file<-"checkin1000.json"
checkins<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

myBusiness <- subset(checkins, business_id == "kREVIrSBbtqBhIYkTccQUg")

myThursday <- cbind(myBusiness$business_id, myBusiness$time$Thursday)

meltedThursday <- melt(myThursday, id.vars = 1, variable.name = "hours")

hoursOfTheDay <- c("0:00","1:00","2:00","3:00","4:00","5:00",
                   "6:00","7:00","8:00","9:00","10:00","11:00",
                   "12:00","13:00","14:00","15:00","16:00","17:00",
                   "18:00","19:00","20:00","21:00","22:00","23:00")

visits <- lapply(hoursOfTheDay, function(x) {
  tmpVal <- myThursday[, which(colnames(myThursday) == x)]
  if (is.na(tmpVal)) {
    0
  } else {
    tmpVal
  }
})

df <- data.frame(hoursOfTheDay=hoursOfTheDay, visits=unlist(visits))




# lapply(hoursOfTheDay, function(x)
#   print(myThursday[1,which( colnames(myThursday)==x )])
# 
# 
#   )

# for (currentHour in hoursOfTheDay){
#   print(which( colnames(myThursday)==currentHour ))
# }


#meltedThursdaySorted <- meltedThursday[order(as.POSIXct(meltedThursday[,2]), format = "%H:%M"),]
#meltedThursdaySorted <- meltedThursday[order(-meltedThursday[,2]),]

#as.POSIXct( df_dataSet$time , format = "%I:%M:%S")

#dd[ order(-dd[,4], dd[,1]), ]


# df <- as.data.frame(checkins)
# 
# shaped_checkins <- melt(df, id.vars = "business_id")


#biz_dat

#create a table of categories:
# cats<-biz_dat$categories
# listcats<-unlist(cats)
# allcat<-table(listcats)
# category_table<-as.data.frame(allcat)

# we want Restaurants
# 
#restaurants<-grep(pattern="",biz_dat$categories)
#bars<-biz_rest<-biz_dat[restaurants,]
#
# and we want number of stars, latitude, longitude and state
#  features
#
#catego<-fromJSON(biz_dat$categories, simplifyVector = true)
# 
# bizrates<-data.frame(biz_dat$business_id,biz_dat$name,biz_dat$stars,biz_dat$longitude,biz_dat$latitude, biz_dat$state,
#                      biz_dat$attributes$`Take-out`,biz_dat$attributes$`Takes Reservations`,biz_dat$attributes$`Wi-Fi`,
#                      biz_dat$attributes$Caters, biz_dat$review_count)#,I(biz_dat$categories))
# 
# #
#   cc<-complete.cases(bizrates)
# bizrates<-bizrates[cc,]
# write.table(bizrates,"bizrates.dat")
