# load required package
library(camtrapR)  

# set working directory to the folder with all of the cameras in it
setwd("~/Dropbox/JUNE 2018 SORTED")

# create record table
record.table.year2.all <- recordTable(inDir = "Ready",  ## folder in working directory with the camera folders in it
                        IDfrom = "directory",     ## indicates that species ID is directory, not metadata
                        minDeltaTime = 0,        ## time (in minutes) between 'independent' sightings
                        exclude= c("Ghost", "Unknown", "Unknown_antelope", "Mongoose_unknown", "Setup"),
                        timeZone = "Africa/Maputo",   ## time zone that photos are in. 
                        stationCol = "Camera",       ## name of the station in output file
                        writecsv = FALSE,          ## exports csv to the folder with the camera folders in it
                        removeDuplicateRecords = TRUE)    ## removes duplicate records

# 10 min
record.table.year2.10min <- recordTable(inDir = "Ready",  ## folder in working directory with the camera folders in it
                                        IDfrom = "directory",     ## indicates that species ID is directory, not metadata
                                        minDeltaTime = 10,        ## time (in minutes) between 'independent' sightings
                                        deltaTimeComparedTo = "lastRecord",    ## indicates that time is chained together for determining independence
                                        exclude= c("Ghost", "Unknown", "Unknown_antelope", "Mongoose_unknown", "Setup"),
                                        timeZone = "Africa/Maputo",   ## time zone that photos are in. 
                                        stationCol = "Camera",       ## name of the station in output file
                                        writecsv = FALSE,          ## exports csv to the folder with the camera folders in it
                                        removeDuplicateRecords = TRUE)    ## removes duplicate records

# 15 min
record.table.year2.15min <- recordTable(inDir = "Ready",  ## folder in working directory with the camera folders in it
                                  IDfrom = "directory",     ## indicates that species ID is directory, not metadata
                                  minDeltaTime = 15,        ## time (in minutes) between 'independent' sightings
                                  deltaTimeComparedTo = "lastRecord",    ## indicates that time is chained together for determining independence
                                  exclude= c("Ghost", "Unknown", "Unknown_antelope", "Mongoose_unknown", "Setup"),
                                  timeZone = "Africa/Maputo",   ## time zone that photos are in. 
                                  stationCol = "Camera",       ## name of the station in output file
                                  writecsv = FALSE,          ## exports csv to the folder with the camera folders in it
                                  removeDuplicateRecords = TRUE)    ## removes duplicate records

# 1 hr
record.table.year2.1hr <- recordTable(inDir = "Ready",  ## folder in working directory with the camera folders in it
                                  IDfrom = "directory",     ## indicates that species ID is directory, not metadata
                                  minDeltaTime = 60,        ## time (in minutes) between 'independent' sightings
                                  deltaTimeComparedTo = "lastRecord",    ## indicates that time is chained together for determining independence
                                  exclude= c("Ghost", "Unknown", "Unknown_antelope", "Mongoose_unknown", "Setup"),
                                  timeZone = "Africa/Maputo",   ## time zone that photos are in. 
                                  stationCol = "Camera",       ## name of the station in output file
                                  writecsv = FALSE,          ## exports csv to the folder with the camera folders in it
                                  removeDuplicateRecords = TRUE)    ## removes duplicate records
# change working directory
setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# clean so each record is >29 seconds apart (one row per detection/trigger event)
record.table.year2.all <- record.table.year2.all[record.table.year2.all$delta.time.secs >= 30 |  record.table.year2.all$delta.time.secs == 0, ]

# export cleaned record table
write.csv(record.table.year2.all, file="Data/Raw Data/recordtable_year2_allrecordscleaned.csv")

# export 10 min record
write.csv(record.table.year2.10min, file="Data/Raw Data/recordtable_year2_10min.csv")

# export 15 min record
write.csv(record.table.year2.15min, file="Data/Raw Data/recordtable_year2_15min.csv")

# export 1 hr record
write.csv(record.table.year2.1hr, file="Data/Raw Data/recordtable_year2_1hr.csv")

# bring in camera coordinates
# NOTE: this metadata doesn't match year 2 (the operation dates are for year 1) but I'm just bringing it in for the coordinates
cameras <- read.csv("Data/Raw Data/cam_metadata_all.csv", header=T)


## exploratory map of # of observations at each camera location
detectionMaps(CTtable=cameras, Xcol ="Longitude", Ycol="Latitude", stationCol="Camera", speciesCol = "Species", 
              recordTable=record.table.year2.15min, writePNG = FALSE, plotR = TRUE, printLabels = TRUE,
              speciesPlots = TRUE, addLegend = TRUE)








