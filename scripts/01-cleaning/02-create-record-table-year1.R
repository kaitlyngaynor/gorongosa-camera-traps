###################################################################################################
##  Camera Trap Photo Organization: Creating Detection Record .csv with camTrapR
##  K. Gaynor
##
##  A. Creating Detection Record .csv with camTrapR
##  B. Plotting Diel Activity Patterns with overlap
##
###################################################################################################

##  A. Creating Detection Record .csv with camTrapR

library(camtrapR)  ## load required package

setwd("~/Dropbox/projects/GORONGOSA/Camera Trap Grid/JUNE 2017 SORTED")
setwd("~/Dropbox/projects/GORONGOSA/Camera Trap Grid/R")
setwd("/Volumes/Seagate Blue")  ## Working directory should be the directory that
## contains the directory with the camera folders in it (so, NOT the folder with all
## of the folders in it, but one above that)


## create camera database from records; this part takes a little while to run
## you just need to change the first line to reflect the name of the folder with the camera folders in it

database <- recordTable(inDir = "Complete",  ## folder in working directory with the camera folders in it
                        IDfrom = "directory",     ## indicates that species ID is directory, not metadata
                        minDeltaTime = 0,        ## time (in minutes) between 'independent' sightings
                        exclude= c("Ghost", "Unknown", "Unknown_antelope", "Mongoose_unknown", "Setup"),
                        deltaTimeComparedTo = "lastRecord",    ## indicates that time is chained together for determining independence
                        timeZone = "Africa/Maputo",   ## time zone that photos are in. I'm still not sure how DST factors into this...
                        stationCol = "Camera",       ## name of the station in output file
                        writecsv = FALSE,          ## exports csv to the folder with the camera folders in it
                        removeDuplicateRecords = TRUE)    ## removes duplicate records

write.csv(database, file="Record_table_season2_1hr.csv")

## now you should have a .csv with all of the cameras, date, time, species!

## read in a csv that is already written
database <- read.csv("Data/Raw Data/Record_table_all_1hr.csv", header=T)


###################################################################################################

##  B.  Joining metadata (latitude and longitude) and exploring data patterns

## camera location file with metadata 
## All this needs is a column for Camera name (must be same as recordTable that you made above, which is "Camera"); Latitude, Longitude
# cameras <- read.table("cam_metadata.txt", header=T)

cameras <- read.csv("Data/Raw Data/cam_metadata_all.csv", header=T)


## exploratory map of species richness at each camera location
detectionMaps(CTtable=cameras, Xcol ="Longitude", Ycol="Latitude", stationCol="Camera", speciesCol = "Species", 
              recordTable=database, writePNG = FALSE, plotR = TRUE, printLabels = TRUE,
              richnessPlot = TRUE, speciesPlots = FALSE, addLegend = TRUE)

## exploratory map of # of observations at each camera location
detectionMaps(CTtable=cameras, Xcol ="Longitude", Ycol="Latitude", stationCol="Camera", speciesCol = "Species", 
              recordTable=database, writePNG = FALSE, plotR = TRUE, printLabels = TRUE,
              speciesPlots = TRUE, addLegend = TRUE)

## histogram of activity at each hour (for each species)
activityHistogram(database, allSpecies=TRUE, recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%m/%d/%y %H:%M:%S")

## smoothed density plots of activity throughout the day
activityDensity(database, allSpecies=TRUE, recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%m/%d/%y %H:%M:%S")



## creates matrix of daily camera operability
cam_matrix <- cameraOperation(CTtable = cameras,        ## data frame with metadata
                              stationCol = "Camera",
                              setupCol = "Start",       ## name of column in metadata with setup date
                              retrievalCol = "End",     ## name of column in metadata with end date
                              hasProblems = TRUE,
                              dateFormat = "%m/%d/%y",
                              writecsv = FALSE)

## creates matrix of species detections for occupancy analysis (baboon example)
detection_matrix <- detectionHistory(recordTable = database,
                                     species = "Wildebeest",
                                     camOp = cam_matrix,
                                     stationCol = "Camera",
                                     speciesCol = "Species",
                                     recordDateTimeCol = "DateTimeOriginal",
                                     recordDateTimeFormat = "%m/%d/%y %H:%M:%S",
                                     occasionLength = 1,
                                     day1 = "survey",
                                     includeEffort = FALSE,
                                     scaleEffort = FALSE,
                                     timeZone = "Africa/Maputo",
                                     writecsv = TRUE,
                                     outDir = "~/Dropbox/projects/GORONGOSA/Projects/Occupancy modeling/R/Data/Raw Data/Occupancy matrices by date"
)

surveyReport(recordTable = database,
             CTtable = cameras,
             speciesCol = "Species",
             stationCol = "Camera",
             setupCol = "Start",
             retrievalCol = "End",
             CTDateFormat = "%m/%d/%y",
             CTHasProblems = TRUE,
             recordDateTimeCol = "DateTimeOriginal",
             recordDateTimeFormat = "%m/%d/%y %H:%M:%S",
             Xcol = "Longitude",
             Ycol = "Latitude", 
             sinkpath = "~/Dropbox/projects/GORONGOSA/Projects/Occupancy modeling/R",
             makezip = TRUE)
