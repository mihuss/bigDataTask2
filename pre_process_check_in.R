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

checkins_flat$num_checkins <- rowSums(checkins_flat[,2:169], na.rm = TRUE)

write.table(checkins_flat,"data/checkins.dat")



