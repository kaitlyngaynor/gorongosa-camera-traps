## Code to remove duplicated data from camera trap records
## LLB, 13/07/2018

## Clear the workspace
rm(list = ls())

## Get the libraries you need
library(dplyr)
library(data.table) ## tbh I don't know if you actually need this one
library(progress)

## Check the working directory
getwd()
setwd("D:/Kaitlyn's Data")

## Import the original ALLrecords files from Kaitlyn
OriginalJune <- read.csv(file = "./Gaynor_ALLrecords_June_to_Nov_2016.csv", stringsAsFactors = FALSE)
OriginalNov <- read.csv(file = "./Gaynor_ALLrecords_Nov_2016_to_June_2017.csv", stringsAsFactors = FALSE)

## Because June_to_Nov has no seconds in the DateTimeOriginal, but Nov_to_June does, and they are in different forms, standardize it
#### Also turn date/time column into class POSIXct for working with 
OriginalJune$DTNew <- paste(OriginalJune$Date, OriginalJune$Time, sep =" ")
OriginalJune$DTNew <- as.POSIXct(OriginalJune$DTNew, format = "%m/%d/%y %H:%M:%S")

OriginalNov$DTNew <- paste(OriginalNov$DateTimeOriginal)
OriginalNov$DTNew <- as.POSIXct(OriginalNov$DTNew, format = "%Y-%m-%d %H:%M:%S")

## Turn them into one file
Original <- rbind(OriginalJune, OriginalNov)

## Make sure we're actually sorted in chronological/alphabetical order
Original <- Original[with(Original, order(Camera, Species, DTNew)),]

## Make an empty column to mark second photos in
Original$Check <- as.character("")

## And make a progress bar (and honestly a spotify playlist and a cup of tea) because this will take a while
pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                 total = nrow(Original), clear = FALSE, width= 60)

## Run the magic loop
#### Assuming that all photos are timestamped sequentially, this loop compares each line of data to the line 
#### before it, and sees if they have the same camera/species/date, and, if so, whether they fall within 30 seconds

for(i in 1:nrow(Original)){
  pb$tick() ## This line makes a progress bar appear
  ifelse(difftime(Original[i+1, "DTNew"], Original[i, "DTNew"], units = "secs") <= 29 ## Change time frame here 
         && Original[i, "Camera"] == Original[i+1, "Camera"]
         && Original[i, "Species"] == Original[i+1, "Species"], 
         Original[i+1, "Check"] <- "REMOVE", Original[i,])
}

## Get rid of all the second photos, as well as the extra columns we made
Fixed <- Original[Original$Check != "REMOVE", 1:12]

## And I guess if you want all the records that are doubled, you can have those too
Duplicated <- Original[Original$Check == "REMOVE", 1:12]

## Make a time-stamped .csv file of the new data
write.csv(Fixed, file = paste("ALLDataFixed", "_", format(Sys.Date(), "%Y%m%d"), ".csv", collapse = "", sep = ""), row.names = FALSE)








#########################################################################################
## The "probably useless but may come in handy later" bits (ie the bits that didn't work)
#########################################################################################
for(i in 1:nrow(DT)){
  pb$tick()
  ifelse(difftime(DT[i+1, "DTNew"], DT[i, "DTNew"], units = "secs") <= 3  
         && DT[i, "Camera"] == DT[i+1, "Camera"]
         && DT[i, "Species"] == DT[i+1, "Species"], 
         DT[i+1, "Check"] <- "REMOVE", DT[i,])
}



DT = as.data.table(Original)
DT = DT[c(1:10, 15000:15010),]
DT[, .(Camera = Camera, Species = Species, DTNew = DTNew)
   , by = .()]

library(data.table)

dt = as.data.table(DF) # or convert in place using setDT


dt[, .(Start = min(Start), End = max(End), State = State[1])
   , by = .(SampleID, Chr, rleid(State),
            cumsum(c(FALSE, head(End + 1, -1) < tail(Start, -1))))]


Original %>% select(X, Camera, Species, DTNew) %>%
  filter()


## Create a separate data frame for each camera
x <- split(Filter, Filter$Camera)

Names <- unique(Filter$Camera)
names(x) <- Names
list2env(x, envir = .GlobalEnv)
###############################################

## Count how many photos were taken per hour
HourCounts <- data.frame(table(cut(Filter$DTNew, breaks = "hour")))
############################################


Filter[2, "DTNew"] - Filter[1, "DTNew"] == "1 secs"

difftime(Filter[2,"DTNew"], Filter[1, "DTNew"], units = "secs") < 3

?difftime
####################################################
