# Was curious to see how similar the metadata values were at each camera site if the covariate rasters were 
# standardized before extraction, versus standardized among the 60 values at the camera sites
# Lindsey suggested they should be similar, or the camera locations don't accurately represent
# the study area!

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# bring in metadata values BEFORE normalization
metadata.original <- read.csv("Data/cam_metadata_master_not_normalized.csv")
names(metadata.original)

# bring in metadata values AFTER normalization (at the scale of entire grid network)
metadata.norm <- read.csv("Data/cam_metadata_norm_121218.csv")
names(metadata.norm)

# define function for standardizing
zstd <- function (x)  (x-mean(x,na.rm = T))/sd(x,na.rm = T)  

# pull covariates to standardize
metadata.original.subset = metadata.original[,c(17:42)]

# rename
names(metadata.original.subset) <- paste(names(metadata.original.subset), ".norm2", sep = "")

# standardize
metadata.original.subset <- apply(metadata.original.subset,2,zstd)

# combine
metadata <- cbind(metadata.norm, metadata.original.subset)
names(metadata)

# explore a handful of variables
plot(fire.interval ~ fire.interval.norm2, data = metadata)
plot(tree.100m ~ tree.100m.norm2, data = metadata)
plot(tree.250m ~ tree.250m.norm2, data = metadata)
plot(tree.500m ~ tree.500m.norm2, data = metadata)
plot(poaching.high ~ poaching.high.norm2, data = metadata)
plot(road.distance ~ road.distance.norm2, data = metadata)

# these are suspiciously identical ... ??
# I had to go back and confirm that these were indeed the values extracted from the normalized stack (they are)

# actually, they are correlated but not identical
plot(road.distance ~ road.distance.norm2, data = metadata)
abline(lm(road.distance ~ road.distance.norm2, data = metadata))
abline(0,1) # diagonal = 1 to 1

plot(tree.1km ~ tree.1km.norm2, data = metadata)
abline(lm(tree.1km ~ tree.1km.norm2, data = metadata))
abline(0,1) # diagonal = 1 to 1

# perfect linear fit
fit <- lm(road.distance ~ road.distance.norm2, data = metadata)
summary(fit)

# i'm still kind of suspicious of the complete lack of noise here but whatever
# doesn't matter much since this only has implications for making maps later
