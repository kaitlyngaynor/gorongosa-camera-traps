library(camtrapR)

# I recommend making a copy of the two folders to be shifted somewhere else, just in case anything goes wrong

# set working directory as the directory that contains the folder with the camera folders in it
# here is what the directory structure should look like:
# — Working directory
# --- To_fix
# ----- B07
# ------- B07_R1
# ----- H07
# ------- H07_R1

setwd("")

timeShiftTable <- data.frame(folder = c("B07", "H07"),
                             sign = c("+", "+"),
                             offset = c("0:0:1685 10:8:0", "0:0:1609 11:18:0"))

timeShiftImages(inDir                = "To_fix",
                timeShiftTable       = timeShiftTable,
                stationCol           = "folder",
                hasCameraFolders     = FALSE,
                timeShiftColumn      = "offset",
                timeShiftSignColumn  = "sign"
)
