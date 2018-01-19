# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(reshape2)

# CREATE FILE A

json_file<-"splitReviewA.json"
review_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)#, review_dat$time$Tuesday, review_dat$time$Wednesday, review_dat$time$Thursday, review_dat$time$Friday, review_dat$time$Saturday, review_dat$time$Sunday, row.names = c(NA, -7L), class = "data.frame")#,I(review_dat$categories))

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf2 <- cbind(reviewDf, newCols)

cc<-complete.cases(reviewDf2)
reviewDf2<-reviewDf2[cc,]
write.table(reviewDf2,"reviewA.dat")

# CREATE FILE B

json_file<-"splitReviewB.json"
review_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)#, review_dat$time$Tuesday, review_dat$time$Wednesday, review_dat$time$Thursday, review_dat$time$Friday, review_dat$time$Saturday, review_dat$time$Sunday, row.names = c(NA, -7L), class = "data.frame")#,I(review_dat$categories))

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf2 <- cbind(reviewDf, newCols)

cc<-complete.cases(reviewDf2)
reviewDf2<-reviewDf2[cc,]
write.table(reviewDf2,"reviewB.dat")

# CREATE FILE C

json_file<-"splitReviewC.json"
review_dat<-fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

reviewDf<-data.frame(review_dat$business_id, review_dat$stars, review_dat$date)#, review_dat$time$Tuesday, review_dat$time$Wednesday, review_dat$time$Thursday, review_dat$time$Friday, review_dat$time$Saturday, review_dat$time$Sunday, row.names = c(NA, -7L), class = "data.frame")#,I(review_dat$categories))

newColNames <- c("Year", "Month", "Day")
newCols <- colsplit(review_dat$date, "-", newColNames)
reviewDf2 <- cbind(reviewDf, newCols)

cc<-complete.cases(reviewDf2)
reviewDf2<-reviewDf2[cc,]
write.table(reviewDf2,"reviewC.dat")

