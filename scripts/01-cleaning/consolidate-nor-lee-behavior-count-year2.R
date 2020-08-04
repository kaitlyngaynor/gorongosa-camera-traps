library(tidyverse)

file_names <- dir("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/nor-lee-spreadsheets")

your_data_frame <- do.call(rbind,
                           lapply(paste("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/nor-lee-spreadsheets/", file_names, sep=""),
                                  read.csv))

write.csv(your_data_frame, file = "data/raw-data/nor-lee-spreadsheets/consolidated_year2.csv", row.names = F)
