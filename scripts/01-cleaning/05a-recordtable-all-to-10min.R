## Code to convert complete record table to 10 minute independent records
## Original by LLB, 13/07/2018
## Modified by KG, 30/10/2018

## Get the libraries you need
library(dplyr)
library(data.table) ## tbh I don't know if you actually need this one
library(progress)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid")

record.table.all <- read.csv("Gaynor_ALLrecords_cleaned_June16_to_June17.csv", stringsAsFactors = FALSE)

##### CAMTRAPR ADAPTATION

record.table.10min <- record.table.all[record.table.all$delta.time.secs >= 600 |  # because 900 seconds = 15 minutes
                               record.table.all$delta.time.secs == 0, ] # so that you keep the ones where delta.time.secs is 0 (these are the 'first' records)

# I realized that the original record table includes records from outside camera operation dates
# (this arises due to tall grass, with few detections, and I want to exclude these dates)
metadata <- read.csv("R/Data/Raw Data/cam_metadata_all.csv")

# merge camera start, end, and problem dates with the record table
metadata <- metadata[, 1:5]
record.table.10min <- merge(record.table.10min, metadata) # join by camera

# get the dates into date format
record.table.10min$Date <- as.Date(record.table.10min$Date, format = "%m/%d/%y")
record.table.10min$Start <- as.Date(record.table.10min$Start, format = "%m/%d/%y")
record.table.10min$End <- as.Date(record.table.10min$End, format = "%m/%d/%y")
record.table.10min$Problem1_from <- as.Date(record.table.10min$Problem1_from, format = "%m/%d/%y")
record.table.10min$Problem1_to <- as.Date(record.table.10min$Problem1_to, format = "%m/%d/%y")

# label records to drop if outside of operation date (either before start, after end, or during problem window)
record.table.10min$drop <- FALSE  # create default of false
for (i in 1:nrow(record.table.10min)) {
  if (record.table.10min$Date[i] < record.table.10min$Start[i]) {
    record.table.10min$drop[i] <- TRUE}
  else if (record.table.10min$Date[i] > record.table.10min$End[i]) {
    record.table.10min$drop[i] <- TRUE}
  else if ((is.na(record.table.10min$Problem1_from[i]) = FALSE) & (record.table.10min$Date[i] > record.table.10min$Problem1_from[i]) & (record.table.10min$Date[i] < record.table.10min$Problem1_to[i])) {
    record.table.10min$drop[i] <- TRUE}
  else {
    record.table.10min$drop[i] <- FALSE}
}

summary(record.table.10min$drop)

# exclude records outside of operation dates
record.table.10min <- record.table.10min[record.table.10min$drop == FALSE,]

# get rid of extra columns
record.table.10min <- record.table.10min[,1:12]

write.csv(record.table.10min, "Gaynor_10minrecords_June2016_to_June2017.csv", row.names = FALSE)

