########################################################################################################################
#  SETUP
########################################################################################################################

library(camtrapR)
library(plyr)
library(progress)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# using 1 hr data because it doesn't really matter for occupancy and takes much less time to run the loops

# import year 1 data
record.table.year1 <- read.csv("Data/Raw Data/Gaynor_60minrecords_June2016_to_June2017.csv")

# import year 2 data
record.table.year2 <- read.csv("Data/Raw Data/recordtable_year2_1hr.csv")

# merge record tables from both years
record.table <- rbind(record.table.year1, record.table.year2)

# bring in csv with camera operation dates for both years
camera.operation <- read.csv("Data/Camera_operation_years1and2.csv", header=T)


########################################################################################################################
#  DROP ANY RECORDS THAT ARE OUTSIDE OF THE CAMERA OPERATION DATES
########################################################################################################################

# merge camera start, end, and problem dates with the record table
record.table <- join(record.table, camera.operation) # join by camera

# get the dates into date format
# I tried this as a loop and it didn't work? hmph
record.table$Date <- as.Date(record.table$Date, format = "%m/%d/%y")
record.table$Start <- as.Date(record.table$Start, format = "%m/%d/%y")
record.table$End <- as.Date(record.table$End, format = "%m/%d/%y")
record.table$Problem1_from <- as.Date(record.table$Problem1_from, format = "%m/%d/%y")
record.table$Problem1_to <- as.Date(record.table$Problem1_to, format = "%m/%d/%y")
record.table$Problem2_from <- as.Date(record.table$Problem2_from, format = "%m/%d/%y")
record.table$Problem2_to <- as.Date(record.table$Problem2_to, format = "%m/%d/%y")
record.table$Problem3_from <- as.Date(record.table$Problem3_from, format = "%m/%d/%y")
record.table$Problem3_to <- as.Date(record.table$Problem3_to, format = "%m/%d/%y")

# takes super long time to run
# make a progress bar
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
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
write.csv(record.table, "Data/Raw Data/recordtable_year1and2_1hr.csv", row.names=F)


########################################################################################################################
#  CREATE OCCUPANCY MATRICES
########################################################################################################################

# read back in if need be
#record.table <- read.csv("Data/Raw Data/recordtable_year1and2_1hr.csv")
#camera.operation <- read.csv("Data/Camera_operation_years1and2.csv", header=T)

# create matrix of daily camera operability
camera.matrix <- cameraOperation(CTtable = camera.operation,        ## data frame with metadata
                              stationCol = "Camera",
                              setupCol = "Start",       ## name of column in metadata with setup date
                              retrievalCol = "End",     ## name of column in metadata with end date
                              hasProblems = TRUE,
                              dateFormat = "%m/%d/%y",
                              writecsv = FALSE)

# read in species metadata for the 38 species of interest
species <- read.csv("Data/2018spp.csv")

# create a list of all of these species
all.species <- levels(species$CommName) %>% as.list()

# and a list of their codes
# NOTE: originally, I used levels(species$SppCode) %>% as.list() but then this resulted in
# the codes being alphabetical, and species being alphabetical, so the codes weren't matched
# up with the species. Caused all sorts of problems downstream. Ugh!!! So redone manually.
all.species.codes <- c("ORAF", "PACY", "SYCA", "OTCR", "TRSY", "POLA", "CICI", "SYGR", 
                       "CENA", "TAOR", "LOAF", "GEGE", "LEMI", "ALBU", "HIAM", "MECA",
                       "AEME", "TRST", "PALE", "MUMU", "BDCR", "HEPA", "HEIC", "ATPA",
                       "GASA", "ICAL", "TRAN", "OUOU", "MATE", "HYAF", "RERE", "HINI",
                       "CEAL", "LESE", "CHPY", "PHAF", "KOEL", "COTA")

# make blank list to put species matrices in
all.matrices <- list()

# run loop that creates occupancy matrix for each species
for (i in 1:length(all.species)) {
  x <- detectionHistory(recordTable = record.table,
                                        species = all.species[[i]],
                                        camOp = camera.matrix,
                                        stationCol = "Camera",
                                        speciesCol = "Species",
                                        recordDateTimeCol = "DateTimeOriginal",
                                        recordDateTimeFormat = "%m/%d/%y %H:%M",
                                        occasionLength = 1,
                                        datesAsOccasionNames = TRUE,
                                        day1 = "survey",
                                        includeEffort = FALSE,
                                        scaleEffort = FALSE,
                                        timeZone = "Africa/Maputo",
                                        writecsv = TRUE,
                                        outDir = "~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/Data/Raw Data/Occupancy matrices by date year 2")
  
  x <- as.data.frame(x)
  y <- NULL
  y$Camera <- rownames(x) # add column for camera (currently row name)
  y$Species <- all.species.codes[[i]] # add column for species (4-letter code)
  y <- as.data.frame(y)
  x <- cbind(y,x) # combine column/species with occupancy matrix (I wasn't sure how else to add columns to beginning, this is a workaround)
  
  all.matrices[[i]] <- as.data.frame(x) # store in matrix
  
}

# bind all into one data frame
all.matrices.rbind <- do.call("rbind", all.matrices)

# export csv
write.csv(all.matrices.rbind, "Data/Raw Data/All_species_by_date_year1and2_031419.csv", row.names = F)

# THEN GO INTO EXCEL - change row names so they are dates (6/23/16, etc) so it will work with subsequent code. 
# I'm not sure how to do this in R and didn't feel like figuring it out so I didn't...
# I just did it in Excel by typing in the first two dates and then dragging horizontally across the columns
