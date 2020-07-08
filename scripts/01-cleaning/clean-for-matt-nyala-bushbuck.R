library(here)
library(overlap)
library(maptools)
library(lubridate)
library(tidyverse)

# Import and merge record tables ---------------------------------------------

records1 <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/recordtable_year1_allrecordscleaned.csv")
records2 <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/recordtable_year2_allrecordscleaned.csv")
records <- rbind(records1, records2) %>% 
  filter(Species %in% c("Bushbuck", "Nyala"))

metadata <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/Camera_operation_years1and2.csv")

# get the dates into date format
metadata[, 2:ncol(metadata)] <- lapply(metadata[, 2:ncol(metadata)], as.Date, format = "%m/%d/%y")

records$Date <- as.POSIXct(records$Date, 
                           format = "%m/%d/%y",
                           tz = "Africa/Maputo")

records <- left_join(records, metadata) # join by camera

# label records to drop if outside of operation date (either before start, after end, or during problem window)
records$drop <- FALSE  # create default of false
for (i in 1:nrow(records)) {
  if (records$Date[i] < records$Start[i]) {
    records$drop[i] <- TRUE}
  else if (records$Date[i] > records$End[i]) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem1_from[i]) == FALSE) & (records$Date[i] > records$Problem1_from[i]) & (records$Date[i] < records$Problem1_to[i])) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem2_from[i]) == FALSE) & (records$Date[i] > records$Problem2_from[i]) & (records$Date[i] < records$Problem2_to[i])) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem3_from[i]) == FALSE) & (records$Date[i] > records$Problem3_from[i]) & (records$Date[i] < records$Problem3_to[i])) {
    records$drop[i] <- TRUE}
  else {
    records$drop[i] <- FALSE}
}

summary(records$drop)

# exclude records outside of operation dates
records <- records[records$drop == FALSE,]

# take the columns we want
records <- records %>% 
  select(Camera, Species, DateTimeOriginal, Date, Time, delta.time.secs, FileName)

write.csv(records, "data/records_bushbuck_nyala_070820.csv", row.names=F)
