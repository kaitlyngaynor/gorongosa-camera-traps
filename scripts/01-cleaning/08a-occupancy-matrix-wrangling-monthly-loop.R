# The goal here is to be able to easily generate summarized occupancy records for different time periods in the necessary format for analysis using Lindsey's code.
# See "Occupancy_matrix_wrangling.Rmd" for a more heavily annotated version
# This script allows the creation of multiple matrices for multiple periods using a for loop (the Rmd doc is manual)

library(tidyverse)
library(sjmisc)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# import matrix
matrix.all <- read.csv("Data/Raw Data/All_species_by_date_062316_090417.csv", header=T)

# make list of start and end dates; the order matters, and the [i] start date must correspond to the [i] end date
starts <- c("X7.1.16", "X8.1.16", "X9.1.16", "X10.1.16", "X11.1.16", "X12.1.16", "X1.1.17", "X2.1.17", "X3.1.17", "X4.1.17", "X5.1.17", "X6.1.17")
ends <- c("X7.31.16", "X8.31.16", "X9.30.16", "X10.31.16", "X11.30.16", "X12.31.16", "X1.31.17", "X2.28.17", "X3.31.17", "X4.30.17", "X5.31.17", "X6.30.17")

# big loop to calculate for every month

for (i in 1:12) {
  
    # specify dates
    start.date <- starts[i]
    end.date <- ends[i]
    
    # selects columns within specified dates
    matrix.subset <- select(matrix.all, Camera, Species, start.date:end.date) 
    
    # sum rows
    matrix.subset$Detections <- rowSums(select(matrix.subset, start.date:end.date), na.rm=TRUE) 
    
    # get rid of the day detections, just select Camera, Species, Detections
    matrix.subset <- select(matrix.subset, Camera, Species, Detections) 
    
    # spread data so one column per species, one row per camera
    matrix.final <- spread(matrix.subset, key = Species, value = Detections)
    
    # export csv
    start.date.2 <- gsub("[.]", "_", start.date)
    start.date.2 <- gsub("X", "", start.date.2)
    
    end.date.2 <- gsub("[.]", "_", end.date)
    end.date.2 <- gsub("X", "", end.date.2)
    
    write.csv(matrix.final, file = paste("Data/Cleaned_occupancy_records/Monthly/Detections_", start.date.2, "_", end.date.2, ".csv", collapse = "", sep = ""))
    
    # spreadsheet of camera operation
    
    # selects columns within specified dates
    matrix.subset.2 <- select(matrix.all, Camera, Species, start.date:end.date) 
    
    # count the columns with zeroes and ones
    zeroes <- row_count(matrix.subset.2, start.date:end.date, count = 0, append = FALSE) 
    ones <- row_count(matrix.subset.2, start.date:end.date, count = 1, append = FALSE)
    
    # add zeroes and ones together into a new data frame
    Operation <- zeroes + ones 
    
    # combine this count dataframe with the matrix subset
    matrix.subset.2 <- cbind(matrix.subset.2, Operation) 
    
    # put the row count into a new variable called "Operation"
    matrix.subset.2$Operation <- matrix.subset.2$rowcount 
    
    # get rid of the day detections and species, just select Camera, Operation
    matrix.subset.2 <- select(matrix.subset.2, Camera, Operation) 
    
    # extract only the top 60 rows, since they are duplicated 42 times (one for each species)
    matrix.subset.2 <- matrix.subset.2[1:60,] 
    
    write.csv(matrix.subset.2, file = paste("Data/Cleaned_occupancy_records/Monthly/Camoperation_", start.date.2, "_", end.date.2, ".csv", collapse = "", sep = ""),row.names=F)
}

