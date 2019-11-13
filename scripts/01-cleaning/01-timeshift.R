# set wd to wherever the camera trap images live
setwd("/Volumes/Seagate Blue")

library(camtrapR)

# old file - uses previous file names, now lives in archives
timeShiftTable <- read.csv("Time_shift_table.csv", header=T)

timeshift_run <- timeShiftImages(inDir                = "To change",
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "Station",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign"
)


## creating camera database from records
database <- recordTable(inDir = "To change time 2",        ## folder with the camera folders in it
                        IDfrom = "directory",     ## indicates that species ID is directory (not metadata)
                        minDeltaTime = 60,        ## time (in minutes) between 'independent' sightings
                        deltaTimeComparedTo = "lastRecord",    ## indicates that time is chained together for determining independence
                        timeZone = "Africa/Maputo", 
                        stationCol = "Camera",       ## name of the station in output file
                        writecsv = TRUE,          ## exports file
                        removeDuplicateRecords = FALSE)    ## doesn't delete duplicate files

activityHistogram(database, allSpecies=TRUE)
activityDensity(database, allSpecies=TRUE)
