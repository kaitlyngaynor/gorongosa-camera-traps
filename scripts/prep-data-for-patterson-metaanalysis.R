# format data for Patterson meta-analysis on the effects of roads

library(camtrapR)
library(dplyr)
library(tibble)


# Summarize sampling effort -----------------------------------------------

# import sampling effort
operation <- read.csv("data/Camera_operation_years1and2.csv")

# generate camera operation matrix
operation_matrix <- cameraOperation(CTtable = operation,
                                    stationCol = "Camera",
                                    setupCol = "Start",
                                    retrievalCol = "End",
                                    dateFormat = "%m/%d/%y",
                                    hasProblems = TRUE,
                                    writecsv = FALSE)

# convert row names to a new column
operation_matrix <- rownames_to_column(as.data.frame(operation_matrix), var = "Camera")

# calculate number of operation days for each camera
operation_matrix$Operation <- rowSums(dplyr::select(operation_matrix, 2:ncol(operation_matrix)), na.rm=TRUE) 

# drop other rows
operation_sum <- dplyr::select(operation_matrix, Camera, Operation)



# Calculate species counts ------------------------------------------------

# import detection data at 10 min sampling interval (has already been cleaned to exclude records outside of sampling in 01a-rai-calculation-10min.R)
records <- read.csv("data/raw-data/recordtable_year1and2_10min.csv")

unique(records$Species)

`%notin%` <- Negate(`%in%`)

record_count <- records %>% 
  mutate(Species = recode(Species, "Hornbill_ground 2" = "Ground_hornbill", "Hornbill_ground" = "Ground_hornbill",
                          "Lizard" = "Monitor_lizard")) %>% 
  filter(Species %notin% c("Setup", "Bird_other", "Unknown", "Bat", "Snake", "Rain", "Flood", 
                           "Fire", "Human", "Unknown_antelope", "Rodent", "Insect", "Reptile",
                           "Mongoose_other", "Duiker_unknown")) %>% 
  count(Camera, Species) %>% 
  # pivot wider to fill empty cells with 0s
  pivot_wider(names_from = "Species", values_from = "n") %>% 
  replace(is.na(.), 0) %>% 
  # pivot back to calculate RAI
  pivot_longer(cols = -c(Camera), names_to = "Species", values_to = "Detection_Count") %>% 
  left_join(operation_sum) %>% 
  mutate(Relative_Activity = Detection_Count/Operation) 

sort(unique(record_count$Species))
head(record_count)



# Join with road data -----------------------------------------------------

# import metadata
metadata <- read.csv("data/cam_metadata_raw_raster.csv")
head(metadata)
hist(metadata$road_major_dist)

# filter just roads
metadata_clean <- metadata %>% 
  select(StudySite, road_major_dist) %>% 
  rename(Camera = StudySite,
         Road_Dist_Meters = road_major_dist)
head(metadata_clean)

# combine and export
to_export <- left_join(record_count, metadata_clean)
head(to_export)


# Randomize camera names -----------------------------------------------------

integer_seq <- seq(from = 1, to = length(unique(to_export$Camera)))
cam_seq <- paste0("Camera", integer_seq)
random_order <- sample(1:100, length(integer_seq), replace=FALSE)

key <- cbind(cam_seq, random_order) %>% 
  as_tibble() %>% 
  arrange(random_order) %>% 
  cbind(unique(to_export$Camera)) %>% 
  rename(Camera = "unique(to_export$Camera)")

to_export_random <- left_join(to_export, key) %>% 
  select(-c(Camera, random_order)) %>% 
  rename(Camera = cam_seq) %>% 
  arrange(Camera, Species) %>% 
  select(Camera, Species, Detection_Count, Operation, Relative_Activity, Road_Dist_Meters)

write.csv(to_export_random, "data/Gaynor_data_for_Patterson.csv", row.names = FALSE)
