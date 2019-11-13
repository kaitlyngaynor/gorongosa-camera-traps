setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

library(sp) # create and reproject spatial objects
library(raster) # analysis with raster data
library(rgdal) # for reading GIS layers
library(ggplot2) # plotting!
library(rgeos) # some spatial functions including gDistance (distance between geometries)
library(tidyverse) # general tidy coding
library(RStoolbox) # for normalizing rasters
library(RColorBrewer)
library(viridis)

# bring in Stalmans count data
count.all <- read.csv("Data/Stalmans_count_data_all.csv")

# subset 2016 only - take only within the core 2014 count block (not strips on side)
count.16 <- count.all[count.all$Count == 2016 & count.all$Within.Rift.in.2014.count.block == 1,]
  
# convert to spatial points data frame
count.16.sp <- count.16[, c('Longitude', 'Latitude')] %>% 
  SpatialPointsDataFrame(data = count.16, proj4string=CRS("+proj=longlat +ellps=WGS84")) %>% 
  spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84"))

# bring in boundary
boundary.lyr <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Spatial data/GIS layers", "gnp_boundary_west_straight_latlong") %>% 
  spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84"))

# bring in camera hexes
hexes <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Camera layers", "CameraGridHexes") %>% spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84")) 

# landscapes - extract floodplain, mask to boundary
landscapes <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Data from Marc", "gnp_landscapes_park&buffer_latlong") %>% 
  spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84"))
floodplain <- landscapes[landscapes@data$NAME == "Rift Valley Riverine & Floodplain" | landscapes@data$NAME == "Rift Valley Lake Urema",]
floodplain <- gIntersection(floodplain, boundary.lyr)
savanna <- gDifference(boundary.lyr, floodplain)

# only floodplain counts
count.16.sp.floodplain <- count.16.sp[floodplain,]
count.16.sp.savanna <- count.16.sp[savanna,]
plot(count.16.sp.floodplain)
plot(count.16.sp.savanna)

# summarize counts of species in each
floodplain.counts <- as_tibble(count.16.sp.floodplain@data)
floodplain.counts$Habitat <- "Floodplain"
savanna.counts <- as_tibble(count.16.sp.savanna@data)
savanna.counts$Habitat <- "Savanna"
counts.byhabitat <- rbind(floodplain.counts, savanna.counts)

# there were capitalization problems for one wildebeest and oribi record; fix here
counts.byhabitat$Species <- fct_collapse(counts.byhabitat$Species, Blue_wildebeest = c("Blue wildebeest", "Blue Wildebeest"))
counts.byhabitat$Species <- fct_collapse(counts.byhabitat$Species, Oribi = c("oribi", "Oribi"))

counts.by.habitat.summary <- counts.byhabitat %>% 
                                group_by(Species, Habitat) %>%
                                summarise(n.indiv = sum(Number, na.rm = TRUE))
head(counts.by.habitat.summary)

# wide data 
counts.summary.wide <- counts.by.habitat.summary %>%
  spread(key = Habitat, value = n.indiv)

# replace NA with 0
counts.summary.wide[is.na(counts.summary.wide)] <- 0

# calculate total and percentages
counts.summary.wide$Total <- counts.summary.wide$Floodplain + counts.summary.wide$Savanna
counts.summary.wide$Floodplain.pct <- counts.summary.wide$Floodplain / counts.summary.wide$Total
counts.summary.wide$Savanna.pct <- counts.summary.wide$Savanna / counts.summary.wide$Total

head(counts.summary.wide)

# add column for SppCode so that metadata can be matched
# can't use common name because we were not the same (ex. Common reedbuck vs. Reedbuck; Duiker grey vs Duiker_grey, Wildebeest vs Blue_wildebeest)
# * NOTE: this only works if the columns are kept in alphabetical order; be careful here !! double-check that it's right
counts.summary.wide$SppCode <- c("COTA", "SYCA", "TRSY", "POLA", "RERE", "SYGR", "CENA", "TAOR", 
                                 "LOAF", "ALBU", "HIAM", "AEME", "TRST", "TRAN", "OUOU", "HINI", 
                                 "PHAF", "KOEL", "EQQU")

# merge with mass data
spp.features <- read.csv("Data/2018spp_kingdon.csv") %>% select(SppCode, Hempsen_mass_kg)
counts.summary.wide <- left_join(counts.summary.wide, spp.features)

# calculate percentages in each habitat
counts.summary.wide$Floodplain.pct <- 

# calculate mass totals and percents
counts.summary.wide$Floodplain.biomass <- counts.summary.wide$Floodplain * counts.summary.wide$Hempsen_mass_kg
counts.summary.wide$Savanna.biomass <- counts.summary.wide$Savanna * counts.summary.wide$Hempsen_mass_kg
counts.summary.wide$Total.biomass <- counts.summary.wide$Total * counts.summary.wide$Hempsen_mass_kg

# total biomass across species
savanna.total <- sum(counts.summary.wide$Savanna.biomass, na.rm=T)
floodplain.total <- sum(counts.summary.wide$Floodplain.biomass, na.rm=T)
total.biomass <- sum(counts.summary.wide$Total.biomass, na.rm=T)

savanna.total / total.biomass
floodplain.total / total.biomass

savanna.total 
floodplain.total
total.biomass

# percent of total number for each species
total.count <- sum(counts.summary.wide$Total, na.rm=T)
counts.summary.wide$count.pct.of.total <- counts.summary.wide$Total / total.count

# percent of total biomass for each species
counts.summary.wide$biomass.pct.of.total <- counts.summary.wide$Total.biomass / total.biomass

# percent of total biomass for each species (excluding elephant)
total.biomass.noelephant <- total.biomass - counts.summary.wide$Total.biomass[counts.summary.wide$Species == "Elephant"]
counts.summary.wide$biomass.pct.of.total.noele <- counts.summary.wide$Total.biomass / total.biomass.noelephant

# export
write.csv(counts.summary.wide, "Data/Stalmans_count_data_summary_by_habitat.csv", row.names=F)


