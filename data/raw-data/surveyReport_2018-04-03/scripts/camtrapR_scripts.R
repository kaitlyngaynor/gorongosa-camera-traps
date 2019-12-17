###  load data tables  ### 

directory.data <- PLEASE_SPECIFY        # this is the directory you got after unzipped the zip file to (e.g. .../surveyReport_2016-02-29/)  

CTtable     <- read.csv(paste(directory.data, 'CTtable.csv', sep = '/'))
recordTable <- read.csv(paste(directory.data, 'recordTable.csv', sep = '/'))


###  plot species detections  ### 

Xcol <- 'Longitude'
Ycol <- 'Latitude'

detections <- detectionMaps(CTtable = CTtable,
            recordTable  = recordTable,
            Xcol         = Xcol,
            Ycol         = Ycol,
            stationCol   = 'Camera',
            speciesCol   = 'Species',
            writePNG     = FALSE,
            plotR        = TRUE,
            printLabels  = TRUE,
            richnessPlot = TRUE,
            addLegend    = TRUE
  ) 


###  camera operation matrix  ### 

cameraOperation <- cameraOperation(CTtable = CTtable,
            stationCol                                  = 'Camera',
            #cameraCol,
            setupCol                                    = 'Start',
            retrievalCol                                = 'End',
            hasProblems                                 = 'TRUE',
            #byCamera,
            #allCamsOn,
            #camerasIndependent,
            dateFormat                                  = '%m/%d/%y' #,
            #writecsv                                   = FALSE,
            #outDir
  ) 


###  detection histories  ### 

day1              <- PLEASE_SPECIFY 
occasionLength    <- PLEASE_SPECIFY
speciesOfInterest <- PLEASE_SPECIFY
timeZone          <- PLEASE_SPECIFY 

detHist <- detectionHistory(recordTable = recordTable,
            species                                  = speciesOfInterest,
            camOp                                    = cameraOperation,
            stationCol                               = 'Camera',
            speciesCol                               = 'Species',
            recordDateTimeCol                        = 'DateTimeOriginal',
            recordDateTimeFormat                     = '%m/%d/%y %H:%M:%S',
            occasionLength                           = occasionLength,
            #maxNumberDays,
            day1                                     = day1,
            #buffer                                  = 0,
            includeEffort                            = TRUE,
            scaleEffort                              = FALSE,
            occasionStartTime                        = 0,
            datesAsOccasionNames                     = FALSE,
            timeZone                                 = timeZone,
            writecsv                                 = FALSE #,
            # outDir
  ) 


