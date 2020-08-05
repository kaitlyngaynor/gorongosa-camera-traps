library(tidyverse)

file_names <- dir("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/nor-lee-spreadsheets")

record_table <- do.call(rbind,
                           lapply(paste("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/nor-lee-spreadsheets/", file_names, sep=""),
                                  read.csv))

write.csv(record_table, file = "data/raw-data/recordtable_year2_withbehaviorcount.csv", row.names = F)

# why are there two more rows in the original dataframe?
original_df <- read.csv("data/raw-data/recordtable_year2_allrecordscleaned.csv")

original_df_test <- select(original_df, X)
record_table_test <- select(record_table_test, X)

anti_join(original_df_test, record_table_test) # 3108, 22684, 136412

original_df[original_df$X == "3108",] # but double-checked images, not actually an animal present
original_df[original_df$X == "22684",] # one male impala; not sure why he got dropped - copy back in manually
original_df[original_df$X == "136412",] # was in the "in" file but not the "out" (last row) - copy back in manually

anti_join(record_table_test, original_df_test)

# not sure how this arose, to be honest - they were absent from the input files, too. 
# Did I not use this as an input?! 
