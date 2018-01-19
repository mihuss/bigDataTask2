# 
# pre_process.R - Preprocess data files for Shiny app
#
# Load libraries
#
library(jsonlite)
library(UsingR)
library(data.table)

# Read the checkins json file
json_file<-"data/checkin.json"
checkins<-stream_in(file(json_file))

checkins_flat <- flatten(checkins)

write.table(checkins_flat,"data/checkins.dat")

day <- "Thursday.21:00"

myBusiness <- subset(checkins_flat, business_id == "7KPBkxAOEtb3QeIL9PEErg")

col <- paste("time", day, sep=".")

print(myBusiness[[col]])


