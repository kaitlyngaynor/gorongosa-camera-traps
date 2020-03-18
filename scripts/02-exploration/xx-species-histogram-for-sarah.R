library(tidyverse)

records1 <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/recordtable_year1_allrecordscleaned.csv")
records2 <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/raw-data/recordtable_year2_allrecordscleaned.csv")
records <- rbind(records1, records2)

`%notin%` <- Negate(`%in%`)

records_sum <-
  records %>% 
  group_by(Species) %>% 
  filter(Species %notin% c("Lizard", "Bat", "Snake", "Hornbill_ground 2", "Hippopotamus", "Duiker_unknown", "Insect", "Rain", "Ground_hornbill", "Mongoose_other", "Mongoose_unknown")) %>% 
  summarise(Count = n()) %>% 
  mutate(Species = fct_reorder(Species, Count))

ggplot(records_sum, aes(x = Species, y = Count)) + 
  geom_bar(stat="identity") +
  coord_flip()

# how many are of the most common species?
records_sum_common4 <- records_sum %>% 
  filter(Species %in% c("Waterbuck", "Baboon", "Warthog", "Impala"))
records_sum_common5 <- records_sum %>% 
  filter(Species %in% c("Waterbuck", "Baboon", "Warthog", "Impala", "Bushbuck"))

sum(records_sum_common4$Count) / sum(records_sum$Count)
sum(records_sum_common5$Count) / sum(records_sum$Count)
