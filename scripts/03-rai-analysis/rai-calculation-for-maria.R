### CODE FOR CALCULATING RELATIVE ACTIVITY INDICES OF SPECIES IN A GIVEN TIME PERIOD
### KAITLYN GAYNOR, 03/03/2020

# load libraries - install packages if you need them
library(tidyverse)
library(camtrapR)
library(magrittr)


# Bring in the data and clean it up ---------------------------------------

# read in record tables. You'll need to make sure that the file paths work for you
records_year1 <- read.csv("data/raw-data/recordtable_year1_allrecordscleaned.csv")
records_year2 <- read.csv("data/raw-data/recordtable_year2_allrecordscleaned.csv")

# join the two years into one data frame and format date as date
records <- rbind(records_year1, records_year2) %>% 
  mutate_at("Date", ~as.Date(., format = "%m/%d/%y"))

# import camera operation spreadsheet and format the date columns correctly
camera_operation <- read_csv("data/Camera_operation_years1and2.csv") %>%
  mutate_at(c("Start", "End", "Problem1_from", "Problem1_to",
              "Problem2_from", "Problem2_to", "Problem3_from", "Problem3_to"),
            ~as.Date(., format = "%m/%d/%y"))

# generate a camera operation matrix using function from camtrapR package
camera_operation_matrix <- cameraOperation(CTtable = camera_operation,        ## data frame
                                           stationCol = "Camera",
                                           setupCol = "Start",       ## name of column in metadata with setup date
                                           retrievalCol = "End",     ## name of column in metadata with end date
                                           hasProblems = TRUE,
                                           writecsv = FALSE) %>% 
  as_tibble(rownames = "Camera")


# Specify rai.calculate function ------------------------------------------

rai.calculate <- function(records, camera_operation_matrix, start_date, end_date, independence_min = 15) {
  
  # SUBSET AND COUNT RECORDS FOR TIME PERIOD
  
  # subset record table to specified start and end dates
  records_subset <- records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * independence_min),
             Date >= as.Date(start_date), Date <= as.Date(end_date))
  
  # calculate number of observations of each classification type at each camera
  record_count <- records_subset %>%
    dplyr::group_by(Camera, Species) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format so we'll have 0s for species that are absent
  
  # gather data so each class-camera-month is its own row again
  record_count <- record_count %>% 
    gather(3:ncol(record_count), key = "Species", value = "Detections")
  
  # DETERMINE CAMERA OPERATION DATES
  
  # calculate how long the camera was functioning in that time period
  
  # change start and end date to character
  start_date <- as.character(start_date)
  end_date <- as.character(end_date)
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camera_operation_matrix$Operation <- rowSums(dplyr::select(camera_operation_matrix, start_date:end_date), na.rm=TRUE) 
  
  # get rid of the individual day columns, just select Camera, Operation
  camera_operation_matrix <- dplyr::select(camera_operation_matrix, Camera, Operation) 
  
  
  # JOIN DETECTIONS AND OPERATION TO CALCULATE RELATIVE ACTIVITY
  
  # join camera operation dates and observations and reorder columns
  RAI.table <- left_join(camera_operation_matrix, record_count) %>% 
    select(Camera, Species, Operation, Detections)
  
  # replace NA with 0 
  RAI.table[is.na(RAI.table)] <- 0
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Detections / RAI.table$Operation
  
  # replace infinity with NA
  RAI.table %<>% mutate_if(is.numeric, list(~na_if(., Inf)))
  
  return(RAI.table)
  
}

# Run the function ---------------------------------------

# I arbitrarily selected start and end dates, but you can assign these manually based on the windows before or after the fire
test <- rai.calculate(records, camera_operation_matrix, start_date = "2016-09-04", end_date = "2016-11-30", independence_min = 15)

head(test)

# let's say that you want to subset to only a single camera of interest (I'll pick G02 for no reason) - run this code to filter

G02 <- test %>% 
  filter(Camera == "G02")

# and let's add a column to specify that this is "Pre" fire
G02$TimePeriod <- "PreFire"

head(G02)

# and if you want to export the file...
write.csv(G02, "G02_RAI.csv")
