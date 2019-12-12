setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid")

library(tidyverse)
library(dplyr)
library(plyr)
library(progress)


# need to create this file using other code
record.table.year1 <- read.csv("Gaynor_15minrecords_June2016_to_June2017.csv", stringsAsFactors = FALSE)
record.table.year2 <- read.csv("R/Data/Raw Data/recordtable_year2_15min.csv", stringsAsFactors = FALSE)
record.table <- rbind(record.table.year1, record.table.year2)

# bring in csv with camera operation dates for both years
camera.operation <- read.csv("R/Data/Camera_operation_years1and2.csv", header=T)


########################################################################################################################
#  DROP ANY RECORDS THAT ARE OUTSIDE OF THE CAMERA OPERATION DATES
########################################################################################################################

# merge camera start, end, and problem dates with the record table
record.table <- join(record.table, camera.operation) # join by camera

# get the dates into date format
record.table$Date <- as.Date(record.table$Date, format = "%m/%d/%y")
record.table$Start <- as.Date(record.table$Start, format = "%m/%d/%y")
record.table$End <- as.Date(record.table$End, format = "%m/%d/%y")
record.table$Problem1_from <- as.Date(record.table$Problem1_from, format = "%m/%d/%y")
record.table$Problem1_to <- as.Date(record.table$Problem1_to, format = "%m/%d/%y")
record.table$Problem2_from <- as.Date(record.table$Problem2_from, format = "%m/%d/%y")
record.table$Problem2_to <- as.Date(record.table$Problem2_to, format = "%m/%d/%y")
record.table$Problem3_from <- as.Date(record.table$Problem3_from, format = "%m/%d/%y")
record.table$Problem3_to <- as.Date(record.table$Problem3_to, format = "%m/%d/%y")

# alternatively, could have done this with mutate_at (added 11-Dec-19 during Shiny app development)

# record.table <- join(record.table, camera.operation) %>%
#  mutate_at(c("Date", "Start", "End", "Problem1_from", "Problem1_to",
#              "Problem2_from", "Problem2_to", "Problem3_from", "Problem3_to"),
#            ~as.Date(., format = "%m/%d/%y"))

# takes super long time to run
# make a progress bar
pb <- progress_bar$new(format = "  running [:bar] :percent eta: :eta",
                       total = nrow(record.table), clear = FALSE, width= 60)

# label records to drop if outside of operation date (either before start, after end, or during any of the 3 problem windows)
record.table$drop <- FALSE  # create default of false
for (i in 1:nrow(record.table)) {
  pb$tick() ## This line makes a progress bar appear
  if (record.table$Date[i] < record.table$Start[i]) {
    record.table$drop[i] <- TRUE}
  else if (record.table$Date[i] > record.table$End[i]) {
    record.table$drop[i] <- TRUE}
  else if ((is.na(record.table$Problem1_from[i]) == FALSE) & (record.table$Date[i] > record.table$Problem1_from[i]) & (record.table$Date[i] < record.table$Problem1_to[i])) {
    record.table$drop[i] <- TRUE}
  else if ((is.na(record.table$Problem2_from[i]) == FALSE) & (record.table$Date[i] > record.table$Problem2_from[i]) & (record.table$Date[i] < record.table$Problem2_to[i])) {
    record.table$drop[i] <- TRUE}
  else if ((is.na(record.table$Problem3_from[i]) == FALSE) & (record.table$Date[i] > record.table$Problem3_from[i]) & (record.table$Date[i] < record.table$Problem3_to[i])) {
    record.table$drop[i] <- TRUE}
  else {
    record.table$drop[i] <- FALSE}
}

# see where the issues are
summary(record.table$drop)

# exclude records outside of operation dates
record.table <- record.table[record.table$drop == FALSE,]

# get rid of extra columns
record.table <- record.table[,1:12] 

# export
write.csv(record.table, "R/Data/Raw Data/recordtable_year1and2_15min.csv", row.names=F)



# to read back in if need be
record.table <- read.csv("R/Data/Raw Data/recordtable_year1and2_15min.csv")
record.table$Date <- as.Date(record.table$Date)


# bring in camera operation file for the time period of interest
Camop.dry.year1 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_7_1_16_9_30_16.csv")
Camop.wet3mo.year1 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_1_1_17_3_31_17.csv")
Camop.wet4mo.year1 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_12_1_16_3_31_17.csv")
Camop.dry.year2 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_7_1_17_9_30_17.csv")
Camop.wet3mo.year2 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_1_1_18_3_31_18.csv")
Camop.wet4mo.year2 <- read.csv("R/Data/Cleaned_occupancy_records/Camoperation_12_1_17_3_31_18.csv")


# set start and end date of interest
# ONLY RUN THE DATES I WANT!

  # dry year 1
  camop <- Camop.dry.year1
  start.date <- as.Date("7/1/16", format = "%m/%d/%y")
  end.date <- as.Date("9/30/16", format = "%m/%d/%y")

  # wet3mo year 1
  camop <- Camop.wet3mo.year1
  start.date <- as.Date("1/1/17", format = "%m/%d/%y")
  end.date <- as.Date("3/31/17", format = "%m/%d/%y")

  # wet4mo year 1
  camop <- Camop.wet4mo.year1
  start.date <- as.Date("12/1/16", format = "%m/%d/%y")
  end.date <- as.Date("3/31/17", format = "%m/%d/%y")

  # dry year 2
  camop <- Camop.dry.year2
  start.date <- as.Date("7/1/17", format = "%m/%d/%y")
  end.date <- as.Date("9/30/17", format = "%m/%d/%y")
  
  # wet3mo year 2
  camop <- Camop.wet3mo.year2
  start.date <- as.Date("1/1/18", format = "%m/%d/%y")
  end.date <- as.Date("3/31/18", format = "%m/%d/%y")
  
  # wet4mo year 2
  camop <- Camop.wet4mo.year2 
  start.date <- as.Date("12/1/17", format = "%m/%d/%y")
  end.date <- as.Date("3/31/18", format = "%m/%d/%y")
  
  
# subset record table to date of interest
record.table <- record.table[record.table$Date >= start.date & record.table$Date <= end.date,]

# just take the columns we need (camera and species)
record.table <- record.table[,2:3]

# change column names to match other datasets
colnames(record.table) = c("StudySite", "CommName")

# remove species that we don't care about
for (i in c("Bat", "Bird_other", "Duiker_unknown", "Fire", "Ghost", "Ghosts Part 1", 
                 "Ghosts Part 2", "Ground_hornbill", "Guineafowl_crested", 
                 "Guineafowl_helmeted", "Hornbill_ground", "Hornbill_ground 2", 
                 "Human", "Insect", "Mongoose_other", "Mongoose_unknown", 
                 "Monitor_lizard", "Rain", "Reptile", "Rodent", "Setup",
                 "Snake", "Unknown", "Unknown_antelope")) {
  record.table <- record.table[record.table$CommName != i, ]
}

# calculates number of observations of each species at each camera
records <- record.table %>%
    dplyr::group_by(CommName, StudySite) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = CommName, value = Detections)  # gets from long to wide format

# replace NA values with 0 (no detections)
records[is.na(records)] <- 0

# confirm how many species were detected (subtract one for column with study site) 
# should be 38 or code won't run properly below
ncol(records) - 1

# see what's missing
names(records)

# DRY YEAR 1 AND YEAR 2 ONLY: we have to add a hippo column
records$Hippo <- 0

# WET 3MO ONLY: we have to add multiple columns (missing 3 species)
records$Mongoose_dwarf <- 0
records$Bushbaby <- 0
records$Pangolin <- 0

# WET 4MO YEAR 1 ONLY: we have to add a dwarf mongoose column
records$Mongoose_dwarf <- 0

# WET 3MO YEAR 2 ONLY:
records$Bushbaby <- 0
records$Mongoose_dwarf <- 0

# join camera operation dates and species observations
RAI.table <- join(camop, records)

# gather data so each species-camera is its own row again
RAI.table <- RAI.table %>% gather(3:40, key = "CommName", value = "Count")

# replace 0 with NA (not sure why they got un-replaced...)
RAI.table[is.na(RAI.table)] <- 0

# calculate RAI
RAI.table$RAI <- RAI.table$Count / RAI.table$Operation

# write csv
write.csv(RAI.table, "R/Results/RAI/RAI_dry_year1.csv", row.names=F)
write.csv(RAI.table, "R/Results/RAI/RAI_wet3mo_year1.csv", row.names=F)
write.csv(RAI.table, "R/Results/RAI/RAI_wet4mo_year1.csv", row.names=F)
write.csv(RAI.table, "R/Results/RAI/RAI_dry_year2.csv", row.names=F)
write.csv(RAI.table, "R/Results/RAI/RAI_wet3mo_year2.csv", row.names=F)
write.csv(RAI.table, "R/Results/RAI/RAI_wet4mo_year2.csv", row.names=F)




##### OLD VERSION, in which I calculated RAI with wide data

# create new columns for RAI to go into
#RAI.table[41:78] <- 1

# name these columns Species.RAI
#names(RAI.table) <- c(names(RAI.table[1:40]), paste(names(RAI.table[3:40]), "RAI", sep="."))
#names(RAI.table) # check

# calculate RAI and fill in columns. Will need to adjust if the number of species is different
#for (i in names(RAI.table[3:40])) {  # for all species columns
#  j = match(i, names(RAI.table))     # get the column index for that species
#  k = j + 38                         # column index for new column
#  RAI.table[, k] = RAI.table[, j] / RAI.table$Operation  # calculate RAI (detections / operation days)
#}




