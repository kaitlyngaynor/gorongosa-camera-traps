library(tidyverse)
library(magrittr)
library(lubridate)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

recordtable <- read.csv("Data/Raw Data/Record_table_all_1hr.csv", header=T)

## for Fig's analysis:
recordtable <- read.csv("Data/Baboon_nocturnal_records.csv", header=T)

sun <- read.csv("Data/Raw Data/sunrise_sunset.csv", header=T)

## add sunrise and sunset times to the record table (join by date)
recordtable2 <- left_join(recordtable, sun, by = "Date")

## convert times (including sunrise and sunset) to numeric so they can be compared

recordtable2$Time.Numeric <- as.character(recordtable2$Time)
recordtable2$Time.Numeric <- sapply(strsplit(recordtable2$Time.Numeric,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60+x[3]/3600
       }
)

recordtable2$Sunrise.Numeric <- as.character(recordtable2$Sunrise)
recordtable2$Sunrise.Numeric <- sapply(strsplit(recordtable2$Sunrise.Numeric,":"),
                            function(x) {
                              x <- as.numeric(x)
                              x[1]+x[2]/60+x[3]/3600
                            }
)

recordtable2$Sunset.Numeric <- as.character(recordtable2$Sunset)
recordtable2$Sunset.Numeric <- sapply(strsplit(recordtable2$Sunset.Numeric,":"),
                            function(x) {
                              x <- as.numeric(x)
                              x[1]+x[2]/60+x[3]/3600
                            }
)

## compare time with sunrise/sunset to make day/night column
recordtable2 <- recordtable2 %>% mutate(TimePeriod = ifelse(Time.Numeric < Sunrise.Numeric, 'Night',
                                            ifelse(Time.Numeric > Sunset.Numeric, 'Night',
                                                   ifelse(Time.Numeric >= Sunrise.Numeric & Time.Numeric <= Sunset.Numeric, 'Day', NA))))

## now add season

## create new column for month (numeric)
recordtable2$Month <- month(as.Date(recordtable2$Date, format="%m/%d/%Y"), label=FALSE)

## assign seasons based on months
recordtable2 <- recordtable2 %>% mutate(Season = ifelse(Month < 4, 'Wet',
                                                        ifelse(Month > 10, 'Wet',
                                                            ifelse(Month >= 4 & Month <= 10, 'Dry', NA))))


## export csv of all records
write.csv(recordtable2, "Data/Raw Data/Record_table_all_1hr_timeperiods.csv")

# for fig:
write.csv(recordtable2, "Data/Baboon_nocturnal_records_timeperiods.csv")

## dry only
recordtable2.dry <- subset(recordtable2, Season == "Dry")
write.csv(recordtable2.dry, "Data/Raw Data/Record_table_all_1hr_dryseason.csv")

## wet only
recordtable2.wet <- subset(recordtable2, Season == "Wet")
write.csv(recordtable2.wet, "Data/Raw Data/Record_table_all_1hr_wetseason.csv")

## day only
recordtable2.day <- subset(recordtable2, TimePeriod == "Day")
write.csv(recordtable2.day, "Data/Raw Data/Record_table_all_1hr_day.csv")

## night only
recordtable2.night <- subset(recordtable2, TimePeriod == "Night")
write.csv(recordtable2.night, "Data/Raw Data/Record_table_all_1hr_night.csv")

## dry & day only
recordtable2.dry.day <- subset(recordtable2.dry, TimePeriod == "Day")
write.csv(recordtable2.dry.day, "Data/Raw Data/Record_table_all_1hr_dryseason_day.csv")

## dry & night only
recordtable2.dry.night <- subset(recordtable2.dry, TimePeriod == "Night")
write.csv(recordtable2.dry.night, "Data/Raw Data/Record_table_all_1hr_dryseason_night.csv")

## wet & day only
recordtable2.wet.day <- subset(recordtable2.wet, TimePeriod == "Day")
write.csv(recordtable2.wet.day, "Data/Raw Data/Record_table_all_1hr_wetseason_day.csv")

## wet & night only
recordtable2.wet.night <- subset(recordtable2.wet, TimePeriod == "Night")
write.csv(recordtable2.wet.night, "Data/Raw Data/Record_table_all_1hr_wetseason_night.csv")
