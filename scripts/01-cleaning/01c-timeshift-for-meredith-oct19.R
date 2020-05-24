library(camtrapR)

# I recommend making a copy of the two folders to be shifted somewhere else, just in case anything goes wrong

# set working directory as the directory that contains the folder with the camera folders in it
# in other words, [working_directory] > "To_fix" > [camera folders named "B07" and "H07"]
setwd("~/Desktop")

timeShiftTable <- data.frame(folder = c("B07", "H07"),
                             sign = c("+", "+"),
                             offset = c("0:0:1685 10:8:0", "0:0:1609 11:18:0"))

timeshift_run <- timeShiftImages(inDir                = "To correct",
                                 timeShiftTable       = timeShiftTable,
                                 stationCol           = "folder",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "offset",
                                 timeShiftSignColumn  = "sign"
)