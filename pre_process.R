# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(reshape2)
#
# Read the businesses json file
business_file<-"data/business.json"
biz_dat<-stream_in(file(business_file))

checkins_file<-"data/checkin.json"
checkins<-stream_in(file(checkins_file))

# we want Restaurants
restaurants<-grep(pattern="",biz_dat$categories)
biz_rest<-biz_dat[restaurants,]

bizrates<-data.frame(biz_rest$business_id, 
                     biz_rest$state, 
                     biz_rest$name, 
                     biz_rest$stars,
                     biz_rest$longitude, 
                     biz_rest$latitude, 
                     biz_rest$attributes$RestaurantsTakeOut, 
                     biz_rest$attributes$RestaurantsReservations,
                     biz_rest$attributes$WiFi,
                     biz_rest$attributes$Caters,
                     biz_rest$review_count,
                     biz_rest$hours$Monday,
                     biz_rest$hours$Tuesday,
                     biz_rest$hours$Wednesday,
                     biz_rest$hours$Thursday,
                     biz_rest$hours$Friday,
                     biz_rest$hours$Saturday,
                     biz_rest$hours$Sunday
                     )

checkins_flat <- flatten(checkins)
checkins_flat$num_checkins <- rowSums(checkins_flat[,2:169], na.rm = TRUE)

# perform the join, eliminating not matched rows from Right
bizrates <- merge(bizrates, checkins_flat[, c(1,170)], by.x= "biz_rest.business_id", by.y = "business_id")

cc<-complete.cases(bizrates)
bizrates<-bizrates[cc,]
write.table(bizrates,"data/bizrates.dat")

checkins_flat <- merge(bizrates[, 1, drop=FALSE], checkins_flat[, -170], by.x= "biz_rest.business_id", by.y = "business_id")
write.table(checkins_flat,"data/checkins.dat")


##### Processing reviews.json #####
###################################

review_file<-"data/splitReviewA.json"
review_dat<-stream_in(file(review_file))
reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf <- cbind(reviewDf[, 1:2], newCols[, 1, drop=FALSE]) #take only Year

reviewDf <- merge(bizrates[, 1, drop=FALSE], reviewDf, by.x= "biz_rest.business_id", by.y = "review_dat.business_id")

cc<-complete.cases(reviewDf)
reviewDfFinal<-reviewDf[cc,]

## Review file B ##
review_file<-"data/splitReviewB.json"
review_dat<-stream_in(file(review_file))
reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf <- cbind(reviewDf[, 1:2], newCols[, 1, drop=FALSE]) #take only Year

reviewDf <- merge(bizrates[, 1, drop=FALSE], reviewDf, by.x= "biz_rest.business_id", by.y = "review_dat.business_id")

cc<-complete.cases(reviewDf)
reviewDfFinal <- rbind(reviewDfFinal, reviewDf[cc,])

## Review file C ##
review_file<-"data/splitReviewC.json"
review_dat<-stream_in(file(review_file))
reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf <- cbind(reviewDf[, 1:2], newCols[, 1, drop=FALSE]) #take only Year

reviewDf <- merge(bizrates[, 1, drop=FALSE], reviewDf, by.x= "biz_rest.business_id", by.y = "review_dat.business_id")

cc<-complete.cases(reviewDf)
reviewDfFinal <- rbind(reviewDfFinal, reviewDf[cc,])

write.table(reviewDfFinal,"data/reviewtest.dat")





