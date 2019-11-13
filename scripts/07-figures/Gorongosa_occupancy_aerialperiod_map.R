# I don't think we need all of these; they are an artifact from other code but whatever
library(gtools)
library(plyr)
library(tidyverse)
library(ggplot2)
library(glmulti)
library(plotrix) # for SE function
library(broom)
library(purrr)
library(rgdal) # for loading shape file
library(RColorBrewer)
library(raster)
library(dplyr)
library(MuMIn)
library(nlme)
library(viridis)
library(gstat)


setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

fire.crop.res.norm.coarse <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/fire.crop.res.norm.coarse.tif")
roadsmajor.dist.norm.coarse <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/roadsmajor.dist.norm.coarse.tif")
urema.dist.norm.coarse <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/urema.dist.norm.coarse.tif")
tree.hansen <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/tree.hansen.crop.res.norm.coarse.tif")
poaching.3levels.mask.norm.coarse <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/poaching.3levels.mask.norm.coarse.tif")
termites.crop.100m.norm.coarse <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/termites.crop.100m.norm.coarse.tif")

raster.stack <- raster::stack(fire.crop.res.norm.coarse, roadsmajor.dist.norm.coarse,
                              urema.dist.norm.coarse, tree.hansen,
                              poaching.3levels.mask.norm.coarse, termites.crop.100m.norm.coarse)


# make sure the names match the names of the covariates in the output; they probably won't, and will need changing. Also this order will have to change to match the order in which the rasters are read? maybe? (not sure how period is incorporated into alphabetical order) and maybe there are some rasters in the folder that are not listed here
names(raster.stack) <- c("fire.interval", "road.major.distance", "urema.distance", "tree.hansen", "poaching", "termite.count.100m")

# read in modeled richness
site.richness <- read.csv('Results/Occupancy/Aerial_nogroup/Aerial_spprich.csv')

# bring in associated site metadata
site.metadata <- read.csv('Data/cam_metadata_norm_031519.csv')

site.richness <- plyr::join(site.richness, site.metadata)

# dredge models 
fit <- glmulti(Mean ~ poaching + fire.interval + road.major.distance + urema.distance + tree.hansen + termite.count.100m,
               data = site.richness,
               family = gaussian,
               method = "h",
               crit="aic", # use AIC as model selection criterion
               level=1) # only main effects are to be used, no interactions

# best model - just poaching, fire, road
fit.best <- fit@objects[[1]]

# complete model - I determined it was 8th best just by visually scrolling through summaries
fit.complete <- fit@objects[[8]]

# predict
prediction.best <- raster::predict(raster.stack, fit.best, type = "response")
prediction.complete <- raster::predict(raster.stack, fit.complete, type = "response")

# make nice palette
viridis.palette <- viridis(100, option="B")

# plot
plot(prediction.best, col = viridis.palette)

pdf('Figures/Aerial/richness_map.pdf')
plot(prediction.complete, col = viridis.palette)
dev.off()

# looks funky with poaching polygons in there

##############################################################################################################

# trying without poaching, because it's a sensitive data layer anyway

# dredge models WITHOUT poaching
fit <- glmulti(Mean ~ fire.interval + road.major.distance + urema.distance + tree.hansen + termite.count.100m,
               data = site.richness,
               family = gaussian,
               method = "h",
               crit="aic", # use AIC as model selection criterion
               level=1) # only main effects are to be used, no interactions

# complete model - I determined it was 7th best just by visually scrolling through summaries
fit.complete <- fit@objects[[7]]

prediction.complete <- raster::predict(raster.stack, fit.complete, type = "response")

# make nice palette
viridis.palette <- viridis(100, option="B")

# plot
pdf('Figures/Aerial/richness_map_nopoaching.pdf')
plot(prediction.complete, col = viridis.palette)
dev.off()

##############################################################################################################

# trying Tara's inverse-distance-weighted approach??

data <- read.csv("GIS/Camera layers/Camera_coordinates.csv", header = T)  ## csv with camera coordinates
cameras <- data[, c('Longitude', 'Latitude')]
cameras.sp <- SpatialPoints(cameras, proj4string=CRS("+proj=longlat +ellps=WGS84")) # Data in degrees
camerasUTM.sp <- spTransform(cameras.sp, CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) # Data in UTM (meters)
cameras.UTM <- coordinates(camerasUTM.sp)

plot(cameras.UTM)

# create spatial points data frame
richness.spdf <- SpatialPointsDataFrame(coords = camerasUTM.sp, data = site.richness)

# bring in reference raster
r <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/boundary.dist.norm.coarse.tif")

# nearest neighbor interpolation
gs <- gstat(formula = Mean ~ 1, locations = richness.spdf, nmax = 5, set = list(idp = 0))
nn <- interpolate(r, gs)
plot(nn)

# inverse distance-weighted interpolation
gs <- gstat(formula = Mean ~ 1, locations = richness.spdf)
idw <- interpolate(r, gs)
plot(idw)

# these look horrible!
