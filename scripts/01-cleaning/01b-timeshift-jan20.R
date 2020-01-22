# for WildCam cameras; generate spreadsheet to be sent to Sarah Huebner at Snapshot

library(camtrapR)
library(tidyverse)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/Github Repos/gorongosa-camera-traps")

# old file - uses previous file names, now lives in archives
#timeShiftTable <- read.csv("data/timeshift-archives/timeshift_012020.csv", header=T)
timeShiftTable <- read.csv("data/timeshift-archives/timeshift_012120.csv", header=T)


# set wd to wherever the camera trap images live
setwd("~/Desktop/Date To Correct October")

# first generate record table with the INCORRECT times
database_wrong <- recordTable(inDir = "To_fix",        ## folder with the camera folders in it
                        IDfrom = "directory",     ## indicates that species ID is directory (not metadata)
                        minDeltaTime = 0,        ## time (in minutes) between 'independent' sightings
                        timeZone = "Africa/Maputo", 
                        stationCol = "Camera",       ## name of the station in output file
                        writecsv = TRUE,          ## exports file
                        removeDuplicateRecords = FALSE)    ## doesn't delete duplicate files

timeshift_run <- timeShiftImages(inDir                = "To_fix",
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "folder",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "offset",
                                 timeShiftSignColumn  = "sign"
)

# then generate for correct times
database_correct <- recordTable(inDir = "To_fix",        ## folder with the camera folders in it
                              IDfrom = "directory",     ## indicates that species ID is directory (not metadata)
                              minDeltaTime = 0,        ## time (in minutes) between 'independent' sightings
                              timeZone = "Africa/Maputo", 
                              stationCol = "Camera",       ## name of the station in output file
                              writecsv = TRUE,          ## exports file
                              removeDuplicateRecords = FALSE)    ## doesn't delete duplicate files

head(database_wrong)
head(database_correct)

# merge the two together
database_wrong <- dplyr::select(database_wrong, Camera, DateTimeOriginal, FileName)
database_correct <- dplyr::select(database_correct, Camera, DateTimeOriginal, FileName)
names(database_correct) <- c("Camera", "DateTimeCorrected", "FileName")
database <- left_join(database_wrong, database_correct)



#write.csv(database, "wildcam_corrected_times_D05.csv", row.names = FALSE)
write.csv(database, "wildcam_corrected_times_B07.csv", row.names = FALSE)
