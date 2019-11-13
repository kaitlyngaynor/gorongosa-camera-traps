## Code to convert complete record table to 15 minute independent records
## Original by LLB, 13/07/2018
## Modified by KG, 30/10/2018

## Get the libraries you need
library(dplyr)
library(data.table) ## tbh I don't know if you actually need this one
library(progress)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid")

record.table.all <- read.csv("Gaynor_ALLrecords_cleaned_June16_to_June17.csv", stringsAsFactors = FALSE)

##### CAMTRAPR ADAPTATION

record.table.15min <- record.table.all[record.table.all$delta.time.secs >= 900 |  # because 900 seconds = 15 minutes
                               record.table.all$delta.time.secs == 0, ] # so that you keep the ones where delta.time.secs is 0 (these are the 'first' records)

# I realized that the original record table includes records from outside camera operation dates
# (this arises due to tall grass, with few detections, and I want to exclude these dates)
metadata <- read.csv("R/Data/Raw Data/cam_metadata_all.csv")

# merge camera start, end, and problem dates with the record table
metadata <- metadata[, 1:5]
record.table.15min <- join(record.table.15min, metadata) # join by camera

# get the dates into date format
record.table.15min$Date <- as.Date(record.table.15min$Date, format = "%m/%d/%y")
record.table.15min$Start <- as.Date(record.table.15min$Start, format = "%m/%d/%y")
record.table.15min$End <- as.Date(record.table.15min$End, format = "%m/%d/%y")
record.table.15min$Problem1_from <- as.Date(record.table.15min$Problem1_from, format = "%m/%d/%y")
record.table.15min$Problem1_to <- as.Date(record.table.15min$Problem1_to, format = "%m/%d/%y")

# label records to drop if outside of operation date (either before start, after end, or during problem window)
record.table.15min$drop <- FALSE  # create default of false
for (i in 1:nrow(record.table.15min)) {
  if (record.table.15min$Date[i] < record.table.15min$Start[i]) {
    record.table.15min$drop[i] <- TRUE}
  else if (record.table.15min$Date[i] > record.table.15min$End[i]) {
    record.table.15min$drop[i] <- TRUE}
  else if ((is.na(record.table.15min$Problem1_from[i]) = FALSE) & (record.table.15min$Date[i] > record.table.15min$Problem1_from[i]) & (record.table.15min$Date[i] < record.table.15min$Problem1_to[i])) {
    record.table.15min$drop[i] <- TRUE}
  else {
    record.table.15min$drop[i] <- FALSE}
}

summary(record.table.15min$drop)

# exclude records outside of operation dates
record.table.15min <- record.table.15min[record.table.15min$drop == FALSE,]

# get rid of extra columns
record.table.15min <- record.table.15min[,1:12]

write.csv(record.table.15min, "Gaynor_15minrecords_June2016_to_June2017.csv", row.names = FALSE)



### IGNORE CODE BELOW

##### LYNN METHOD - didn't work for these purposes. 
## This create a new "independent" record every time 15 min has passed, even if there are lots of photos in the interim.


# Correct the date and time
#record.table.all$DTNew <- paste(record.table.all$Date, record.table.all$Time, sep =" ")
#record.table.all$DTNew <- as.POSIXct(record.table.all$DTNew, format = "%m/%d/%y %H:%M:%S")

## Make sure we're actually sorted in chronological/alphabetical order
#record.table.all <- record.table.all[with(record.table.all, order(Camera, Species, DTNew)),]

## Make an empty column to mark duplicate photos in
#record.table.all$Check <- as.character("")

## And make a progress bar (and honestly a spotify playlist and a cup of tea) because this will take a while
#pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
#                 total = nrow(record.table.all), clear = FALSE, width= 60)

## Run the magic loop
#### Assuming that all photos are timestamped sequentially, this loop compares each line of data to the line 
#### before it, and sees if they have the same camera/species/date, and, if so, whether they fall within 30 seconds

#for(i in 1:nrow(record.table.all)){
#  pb$tick() ## This line makes a progress bar appear
#  ifelse(difftime(record.table.all[i+1, "DTNew"], record.table.all[i, "DTNew"], units = "mins") <= 15 ## Change time frame here 
#         && record.table.all[i, "Camera"] == record.table.all[i+1, "Camera"]
#         && record.table.all[i, "Species"] == record.table.all[i+1, "Species"], 
#         record.table.all[i+1, "Check"] <- "REMOVE", record.table.all[i,])
#}


## Get rid of all the second photos, as well as the extra columns we made (Except the DTNew, that's a good one)
#record.table.15min <- record.table.all[record.table.all$Check != "REMOVE", 1:13]

## Make a .csv file of the new data
#write.csv(record.table.15min, "Gaynor_15minrecords_June2016_to_June2017.csv", sep=",", row.names = FALSE)


