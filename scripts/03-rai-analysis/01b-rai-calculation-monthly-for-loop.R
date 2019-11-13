setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid")

library(tidyverse)
library(dplyr)
library(plyr)

# bring in record table
record.table.15min <- read.csv("Gaynor_15minrecords_June2016_to_June2017.csv", stringsAsFactors = FALSE)

# format the date column as a date
record.table.15min$Date <- as.Date(record.table.15min$Date, format = "%m/%d/%y")

# remove species that we don't care about
for (species in c("Bat", "Bird_other", "Duiker_unknown", "Fire", "Ghost", "Ghosts Part 1", 
            "Ghosts Part 2", "Ground_hornbill", "Guineafowl_crested", 
            "Guineafowl_helmeted", "Hornbill_ground", "Hornbill_ground 2", 
            "Human", "Insect", "Mongoose_other", "Mongoose_unknown", 
            "Monitor_lizard", "Rain", "Reptile", "Rodent", "Setup",
            "Snake", "Unknown", "Unknown_antelope")) {
  record.table.15min <- record.table.15min[record.table.15min$Species != species, ]
}

# define list of all species columns that we want (will need this later to ensure these columns are present in all tables)
allspecies <- c(Aardvark = NA_real_, Baboon = NA_real_, Buffalo = NA_real_, Bushbaby = NA_real_, Bushbuck = NA_real_, 
                Bushpig = NA_real_, Civet = NA_real_, Duiker_common = NA_real_, Duiker_red = NA_real_,
                Eland = NA_real_, Elephant = NA_real_, Genet = NA_real_, Hare = NA_real_, Hartebeest = NA_real_, 
                Hippopotamus = NA_real_, Honey_badger = NA_real_, Impala = NA_real_, Kudu = NA_real_,
                Lion = NA_real_, Mongoose_banded = NA_real_, Mongoose_bushy_tailed = NA_real_, 
                Mongoose_dwarf = NA_real_, Mongoose_large_grey = NA_real_, Mongoose_marsh = NA_real_,
                Mongoose_slender = NA_real_, Mongoose_white_tailed = NA_real_, Nyala = NA_real_, Oribi = NA_real_, 
                Pangolin = NA_real_, Porcupine = NA_real_, Reedbuck = NA_real_,
                Sable_antelope = NA_real_, Samango = NA_real_, Serval = NA_real_, Vervet = NA_real_, 
                Warthog = NA_real_, Waterbuck = NA_real_, Wildebeest = NA_real_)  

# bring in camera operation files for each month, store in list
month.list <- list()
month.list[[1]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_7_1_16_7_31_16.csv")
month.list[[2]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_8_1_16_8_31_16.csv")
month.list[[3]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_9_1_16_9_30_16.csv")
month.list[[4]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_10_1_16_10_31_16.csv")
month.list[[5]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_11_1_16_11_30_16.csv")
month.list[[6]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_12_1_16_12_31_16.csv")
month.list[[7]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_1_1_17_1_31_17.csv")
month.list[[8]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_2_1_17_2_28_17.csv")
month.list[[9]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_3_1_17_3_31_17.csv")
month.list[[10]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_4_1_17_4_30_17.csv")
month.list[[11]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_5_1_17_5_31_17.csv")
month.list[[12]] <- read.csv("R/Data/Cleaned_occupancy_records/Monthly/Camoperation_6_1_17_6_30_17.csv")

# define start and end dates for subsetting record table
starts <- c("7/1/16", "8/1/16", "9/1/16", "10/1/16", "11/1/16", "12/1/16", "1/1/17", "2/1/17", "3/1/17", "4/1/17", "5/1/17", "6/1/17")
ends <- c("7/31/16", "8/31/16", "9/30/16", "10/31/16", "11/30/16", "12/31/16", "1/31/17", "2/28/17", "3/31/17", "4/30/17", "5/31/17", "6/30/17")

# define names to use in file names (should match above)
starts.names <- c("070116", "080116", "090116", "100116", "110116", "120116", "010117", "020117", "030117", "040117", "050117", "060117")
ends.names <- c("073116", "083116", "093016", "103116", "113016", "123116", "013117", "022817", "033117", "043017", "053117", "063017")

# define month names (order should match above)
month.names <- c("Jul16", "Aug16", "Sep16", "Oct16", "Nov16", "Dec16", 
                 "Jan17", "Feb17", "Mar17", "Apr17", "May17", "Jun17")

# for loop to calculate RAI

for (i in 1:12) {

    # select time period of interest
    camop <- month.list[[i]]
    
    # change column names
    names(camop) <- c("StudySite", "Operation")
    
    # set start and end date of interest - change to the dates you want!
    start.date <- as.Date(starts[i], format = "%m/%d/%y")
    end.date <- as.Date(ends[i], format = "%m/%d/%y")
      
    # subset record table to date of interest
    record.table.15min.subset <- record.table.15min[record.table.15min$Date >= start.date & record.table.15min$Date <= end.date,]
    
    # add columns for month - in retrospect, probably could have done this above and not gone through the trouble of specifying start/end dates by month
    # record.table.15min.subset$Month <- month(record.table.15min.subset$Date)
    # code above works (with some minor tweaks below) BUT doesn't assign months for species that are absent and added later

    # now just take the columns we need (camera, species, month)
    record.table.15min.subset <- record.table.15min.subset[,2:3]
    
    # change names
    names(record.table.15min.subset) <- c("StudySite", "CommName")
    
    # calculates number of observations of each species at each camera
    records <- record.table.15min.subset %>%
        dplyr::group_by(CommName, StudySite) %>%
        dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
        spread(key = CommName, value = Detections)  # gets from long to wide format
    
    # add columns for species not present
    records <- add_column(records, !!!allspecies[!names(allspecies) %in% names(records)])

    # replace NA values with 0 (no detections)
    records[is.na(records)] <- 0
    
    # join camera operation dates and species observations
    RAI.table <- join(camop, records)
    
    # gather data so each species-camera is its own row again
    RAI.table <- RAI.table %>% gather(3:40, key = "CommName", value = "Count")
    
    # replace 0 with NA (not sure why they got un-replaced...)
    RAI.table[is.na(RAI.table)] <- 0
    
    # calculate RAI
    RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
    
    # add new column for month
    RAI.table$Month <- month.names[i]
    
    # write csv
    write.csv(RAI.table, file = paste("R/Results/RAI_monthly/RAI_", starts.names[i], "_", ends.names[i], ".csv", collapse = "", sep = ""),row.names=F)
  
}
