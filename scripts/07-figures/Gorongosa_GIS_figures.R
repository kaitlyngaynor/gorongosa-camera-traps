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

# bring in boundary
boundary.lyr <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Spatial data/GIS layers", "gnp_boundary_west_straight_latlong") %>% 
  spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84"))

# bring in camera hexes
hexes <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Camera layers", "CameraGridHexes") %>% spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84")) 

# bring in camera locations
data <- read.csv("GIS/Camera layers/Camera_coordinates.csv", header = T)  ## csv with camera coordinates
cameras <- data[, c('Longitude', 'Latitude')]
cameras.sp <- SpatialPoints(cameras, proj4string=CRS("+proj=longlat +ellps=WGS84")) # Data in degrees
camerasUTM.sp <- spTransform(cameras.sp, CRS("+proj=utm +south +zone=36 +ellps=WGS84")) # Data in UTM (meters)
cameras.UTM <- coordinates(camerasUTM.sp)

# lake
lake <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Spatial data/GIS layers", "lake_urema_latlong") %>% spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84")) 

# rivers
rivers.lyr <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Spatial data/GIS layers", "gnp_main_rivers_latlong")
rivers.lyr.UTM <- spTransform(rivers.lyr, CRS("+proj=utm +south +zone=36 +ellps=WGS84"))
rivers.lyr.UTM <- gIntersection(rivers.lyr.UTM, boundary.lyr)

# landscapes - extract floodplain, mask to boundary
landscapes <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Data from Marc", "gnp_landscapes_park&buffer_latlong") %>% 
  spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84"))
floodplain <- landscapes[landscapes@data$NAME == "Rift Valley Riverine & Floodplain",]
floodplain <- gIntersection(floodplain, boundary.lyr)

# calculate areas for later
area(floodplain) # 765567600 m2, or 765.57 square kilometers
area(boundary.lyr) # 3687976749 m2, or 3687.977 square kilometers
area(floodplain) /area(boundary.lyr) # = 20.75% of the area of the park is floodplain
area(lake) # 23 km2 area lake
area(floodplain) /area(lake) # floodplain is 33 times the size of the lake
(area(boundary.lyr)-area(lake))/area(boundary.lyr) # 99.4% uninundated in the dry
(area(boundary.lyr)-area(floodplain))/area(boundary.lyr) # 79.2% uninundated in the dry
(area(boundary.lyr)-area(floodplain))/(area(boundary.lyr)-area(lake)) # available dry area decreases by 20%


# general study area map
pdf('Figures/Study_area/Park.pdf', useDingbats = FALSE) # need to specify useDingbats or the points turn out all weird
plot(boundary.lyr, col = "grey")
plot(floodplain, col = "lightblue", add = T)
lines(rivers.lyr.UTM, col = "blue", add = T)
plot(lake, col = "blue", add = T)
points(camerasUTM.sp, pch=16, cex=.75)
raster::scalebar(10000, xy = c(625000, 7874500), type = "bar", divs = 5, label = c("0", "5", "10 km"))
dev.off()

# bring in rasters
fire <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/fire.crop.res.norm.coarse.tif")
poaching <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/poaching.3levels.mask.norm.coarse.tif")
roads <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/roadsmajor.dist.norm.coarse.tif")
termites <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/termites.crop.100m.norm.coarse.tif")
urema <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/urema.dist.norm.coarse.tif")
lion.wet <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/lion.wet.tif")
lion.dry <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/lion.dry.tif")
tree <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/tree.hansen.crop.res.norm.coarse.tif")
settlement <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/settlement.dist.norm.coarse.tif")
pans <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/panscon.250.norm.coarse.tif")
river <- raster("GIS/Rasters for stacking/Normalized and masked and coarse rasters/rivers.dist.norm.coarse.tif")

viridis.palette <- viridis(100, option="B")

pdf('Figures/Covariate_maps/fire.pdf')
plot(fire, col=viridis.palette, main = "Fire Frequency")
lines(hexes, col = "white")
raster::scalebar(4000, xy = c(638000, 7894500), type = "bar", divs = 5, label = c("0", "2", "4 km"))
dev.off()

pdf('Figures/Covariate_maps/river.pdf')
plot(river, col=viridis.palette, main = "Distance to River")
lines(hexes, col = "white")
raster::scalebar(4000, xy = c(638000, 7894500), type = "bar", divs = 5, label = c("0", "2", "4 km"))
dev.off()

pdf('Figures/Covariate_maps/poaching.pdf')
plot(poaching, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/roads.pdf')
plot(roads, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/termites.pdf')
plot(termites, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/urema.pdf')
plot(urema, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/lion.wet.pdf')
plot(lion.wet, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/lion.dry.pdf')
plot(lion.dry, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/tree.pdf')
plot(tree, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/settlement.pdf')
plot(settlement, col=viridis.palette)
lines(hexes, col = "white")
dev.off()

pdf('Figures/Covariate_maps/pans.pdf')
plot(pans, col=viridis.palette)
lines(hexes, col = "white")
dev.off()


# bring in roads
roads <- readOGR("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R/GIS/Spatial data/GIS layers", "Roads_all") %>% spTransform(CRS("+proj=utm +south +zone=36 +ellps=WGS84")) # project in UTM

# bring in full hansen tree layer
#tree.full <- raster("GIS/Spatial data/Tree cover/Hansen_10S_030E_treecover2010_v3.tif") %>% projectRaster(crs = "+proj=utm +south +zone=36 +ellps=WGS84")
tree.park <- raster("GIS/Rasters for stacking/tree.hansen.gnp.tif") %>% projectRaster(crs = "+proj=utm +south +zone=36 +ellps=WGS84")

# study area map of camera sites
plot(tree)
#plot(hexes, add = TRUE)
lines(roads)
plot(lake, add = TRUE, col = "blue")
points(camerasUTM.sp, pch = 16)
#lines(boundary.lyr, col = "red")



#### WATERBUCK MAPS

waterbuck_dry_raster <- raster('waterbuck_dry_raster.tif')
waterbuck_wet_raster <- raster('waterbuck_wet_raster.tif')
waterbuck_wet_minus_dry <- raster('waterbuck_wet_minus_dry.tif')

breaks <- seq(-0.6827588, 1.8418657, by = 0.01)

pdf('Figures/waterbuck_dry_map.pdf')
plot(boundary.lyr)
#plot(floodplain, col = "lightblue", add = T)
plot(waterbuck_dry_raster, breaks = breaks, add = T)
plot(lake, add = TRUE, col = "blue")
raster::scalebar(10000, xy = c(625000, 7874500), type = "bar", divs = 5, label = c("0", "5", "10 km"))
dev.off()

pdf('Figures/waterbuck_wet_map.pdf')
plot(boundary.lyr)
#plot(floodplain, col = "lightblue", add = T)
plot(waterbuck_wet_raster, breaks = breaks, add = T)
plot(lake, add = TRUE, col = "blue")
raster::scalebar(10000, xy = c(625000, 7874500), type = "bar", divs = 5, label = c("0", "5", "10 km"))
dev.off()



### study area map for Aerial count IZ manuscript

pdf('Figures/Aerial/Study_area.pdf', useDingbats = FALSE)
plot(hexes)
plot(boundary.lyr, col = "grey", add = TRUE)
plot(tree, add = TRUE)
lines(roads)
plot(lake, add = TRUE, col = "blue")
points(camerasUTM.sp, pch = 16)
raster::scalebar(5000,  type = "bar", divs = 5, label = c("0", "2.5", "5 km"))
dev.off()
