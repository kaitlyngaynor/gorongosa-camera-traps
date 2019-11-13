# Temporal overlap analysis

# set working directory
setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# load libraries
library(overlap)
library(maptools)
library(lubridate)
library(plyr)

# set up candidate start/end dates
# dry year 1
start.date.dry.year1 <- as.Date("7/1/16", format = "%m/%d/%y")
end.date.dry.year1 <- as.Date("9/30/16", format = "%m/%d/%y")
# wet4mo year 1
start.date.wet.year1 <- as.Date("12/1/16", format = "%m/%d/%y")
end.date.wet.year1 <- as.Date("3/31/17", format = "%m/%d/%y")
# dry year 2
start.date.dry.year2 <- as.Date("7/1/17", format = "%m/%d/%y")
end.date.dry.year2 <- as.Date("9/30/17", format = "%m/%d/%y")
# wet4mo year 2
start.date.wet.year2 <- as.Date("12/1/17", format = "%m/%d/%y")
end.date.wet.year2 <- as.Date("3/31/18", format = "%m/%d/%y")

# import record table
records <- read.csv("Data/Raw Data/recordtable_year1and2_15min.csv")

##################################################################################################
#  Format and scale times
##################################################################################################

# set spatial coordinates
coords <- matrix(c(34.50, -18.82), nrow=1)
Coords <- sp::SpatialPoints(coords,
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# specify date format
records$Date <- as.POSIXct(records$Date, tz = "Africa/Maputo")

# convert time to radians
records$Time.Corrected <- hms(records$Time)
records$Time.Decimal <- records$Time.Corrected$hour + records$Time.Corrected$minute/60 + records$Time.Corrected$second/3600
records$Time.Scaled <- records$Time.Decimal / 24
records$Time.Radians <- records$Time.Scaled * 2 * pi
# calculate suntime using function from overlap package, and coordinates and dates as formatted above
records$Time.Sun <- sunTime(records$Time.Radians, records$Date, Coords)


##################################################################################################
#  Format and scale times
##################################################################################################

# bring in species traits
species <- read.csv("Data/2018spp_kingdon.csv")

# only lmh and lion
species <- species[species$LMH == "yes" | species$CommName == "Lion",]

# extract codes + common names
codes <- species[,c(1,4)]
names(codes) <- c("SppCode", "Species")

# pair with record table so each record is associated with code
records <- join(records, codes)

# remove records with NA SppCode (outside of core LMH species)
records <- records[complete.cases(records),]

# subset record table to date of interest
records.dry.year1 <- records[records$Date >= start.date.dry.year1 & records$Date <= end.date.dry.year1,]
records.wet.year1 <- records[records$Date >= start.date.wet.year1 & records$Date <= end.date.wet.year1,]
records.dry.year2 <- records[records$Date >= start.date.dry.year2 & records$Date <= end.date.dry.year2,]
records.wet.year2 <- records[records$Date >= start.date.wet.year2 & records$Date <= end.date.wet.year2,]
records.dry.year1and2 <- rbind(records.dry.year1, records.dry.year2)
records.wet.year1and2 <- rbind(records.wet.year1, records.wet.year2)

# bring in list of all pairwise relationships (generated previously for SIF models)
pairs <- read.csv("Data/SIF/fewer_cameras/Dry.peak.SppPairs.csv")

# subset to only LMH species (couldn't figure out a more elegant way to do this)
pairs <- subset(pairs, SppCode1 == "AEME" | SppCode1 == "ALBU" | SppCode1 == "CENA" | SppCode1 == "COTA" | SppCode1 == "HIAM"
                | SppCode1 == "HINI" | SppCode1 == "KOEL" | SppCode1 == "OUOU" | SppCode1 == "PHAF" | SppCode1 == "POLA"
                | SppCode1 == "RERE" | SppCode1 == "SYGR" | SppCode1 == "SYCA" | SppCode1 == "TAOR" | SppCode1 == "TRAN"
                | SppCode1 == "TRST" | SppCode1 == "TRSY")
pairs <- subset(pairs, SppCode2 == "AEME" | SppCode2 == "ALBU" | SppCode2 == "CENA" | SppCode2 == "COTA" | SppCode2 == "HIAM"
                | SppCode2 == "HINI" | SppCode2 == "KOEL" | SppCode2 == "OUOU" | SppCode2 == "PHAF" | SppCode2 == "POLA"
                | SppCode2 == "RERE" | SppCode2 == "SYGR" | SppCode2 == "SYCA" | SppCode2 == "TAOR" | SppCode2 == "TRAN"
                | SppCode2 == "TRST" | SppCode2 == "TRSY")

# edit so there is just one row per pairwise relationship, and columns for the two species
pairs <- pairs[pairs$StudySite == "A06", 2:3]

# create new column for temporal overlap
pairs$Dhat1.dry.year1 <- NA
pairs$Dhat1.wet.year1 <- NA
pairs$Dhat1.dry.year2 <- NA
pairs$Dhat1.wet.year2 <- NA
pairs$Dhat1.dry.year1and2 <- NA
pairs$Dhat1.wet.year1and2 <- NA

# calculate seasonal temporal overlap for each pairwise interaction
for (i in 1:nrow(pairs)) {
  
  species1 <- as.character(pairs[i,1]) # get SppCode1 value
  species2 <- as.character(pairs[i,2]) # get SppCode2 value
  
  # dry year 1 - remove hippo (HIAM)
  if (species1 == "HIAM") {pairs[i,3] <- NA} else if (species2 == "HIAM") {pairs[i,3] <- NA} else {pairs[i,3] <- overlapEst(records.dry.year1[records.dry.year1$SppCode == species1,]$Time.Sun, records.dry.year1[records.dry.year1$SppCode == species2,]$Time.Sun)[1]} # calculate Dhat1 for these two species, store in third column of "pairs"

  # wet year 1
  pairs[i,4] <- overlapEst(records.wet.year1[records.wet.year1$SppCode == species1,]$Time.Sun, records.wet.year1[records.wet.year1$SppCode == species2,]$Time.Sun)[1] # calculate Dhat1 for these two species, store in fourth column of "pairs"

  # dry year 2 - remove hippo (HIAM)
  if (species1 == "HIAM") {pairs[i,5] <- NA} else if (species2 == "HIAM") {pairs[i,5] <- NA} else {pairs[i,5] <- overlapEst(records.dry.year2[records.dry.year2$SppCode == species1,]$Time.Sun, records.dry.year2[records.dry.year2$SppCode == species2,]$Time.Sun)[1]} # calculate Dhat1 for these two species, store in third column of "pairs"

  # wet year 2
  pairs[i,6] <- overlapEst(records.wet.year2[records.wet.year2$SppCode == species1,]$Time.Sun, records.wet.year2[records.wet.year2$SppCode == species2,]$Time.Sun)[1] # calculate Dhat1 for these two species, store in fourth column of "pairs"

  # dry year 1 and 2 - remove hippo (HIAM)
  if (species1 == "HIAM") {pairs[i,7] <- NA} else if (species2 == "HIAM") {pairs[i,7] <- NA} else {pairs[i,7] <- overlapEst(records.dry.year1and2[records.dry.year1and2$SppCode == species1,]$Time.Sun, records.dry.year1and2[records.dry.year1and2$SppCode == species2,]$Time.Sun)[1]} # calculate Dhat1 for these two species, store in third column of "pairs"
  
  # wet year 1 and 2
  pairs[i,8] <- overlapEst(records.wet.year1and2[records.wet.year1and2$SppCode == species1,]$Time.Sun, records.wet.year1and2[records.wet.year1and2$SppCode == species2,]$Time.Sun)[1] # calculate Dhat1 for these two species, store in fourth column of "pairs"
  
}

# export overlap
write.csv(pairs, "Results/Overlap_temporal_031919.csv", row.names=F)

# note that if there are too few records for either species (not sure exactly what cut-off is), overlap will be NA

###############################################
# make a plot for each species' temporal activity pattern

# define plotting function
overlapPlot2<-function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#F8766D", "#00BFC4"),  linewidth = c(2, 2),
                         n.grid = 128, kmax = 3, adjust = 1, 
                        ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[[1]])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[[2]])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}

timeplotwet <-function (A, n.grid = 128, kmax = 3, linecol = "#00BFC4",  ...) 
{
  
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                               "Sunrise", "Noon", "Sunset", "Midnight"))
  lines(xx, densA, lty = 1, col = linecol, lwd = 2)
  return(invisible(list(x = xx, densityA = densA)))
}

# create list of species names
common.names.lmh2 <- rbind(c("Impala", "Hartebeest", "Duiker_red", "Wildebeest", "Hippo", "Sable_antelope", 
                             "Waterbuck", "Elephant", "Oribi", "Warthog", "Bushpig", "Reedbuck", "Buffalo", 
                             "Duiker_common", "Eland", "Nyala", "Kudu", "Bushbuck"))
common.names.lmhbetter <- rbind(c("Impala", "Hartebeest", "Red duiker", "Common wildebeest", "Hippopotamus", "Sable antelope", 
                             "Waterbuck", "Elephant", "Oribi", "Warthog", "Bushpig", "Southern reedbuck", "African Buffalo", 
                             "Common duiker", "Eland", "Nyala", "Greater kudu", "Bushbuck"))



par(mfrow = c(6,3))
# run loop to generate a plot for each species
for (i in c(1:4, 6:18)) {
  dry.toplot <- records.dry.year1and2[records.dry.year1and2$Species == common.names.lmh2[[i]],]$Time.Sun
  wet.toplot <- records.wet.year1and2[records.wet.year1and2$Species == common.names.lmh2[[i]],]$Time.Sun
  overlapPlot2(dry.toplot, wet.toplot, main = common.names.lmhbetter[[i]])
}
#hippo by itself
timeplotwet(records.wet.year1and2[records.wet.year1and2$Species == common.names.lmh2[[5]],]$Time.Sun, main = common.names.lmhbetter[[5]])

# Wow, kind of interesting that some species change so dramatically. let's put lions on there....
overlapPlot2(records.dry.year1and2[records.dry.year1and2$Species == "Lion",]$Time.Sun, 
             records.wet.year1and2[records.wet.year1and2$Species == "Lion",]$Time.Sun)

# both seasons combined
timeplotwet(c(records.dry.year1and2[records.dry.year1and2$Species == "Lion",]$Time.Sun,
              records.wet.year1and2[records.wet.year1and2$Species == "Lion",]$Time.Sun))

# take only the species that may be lion prey
common.names.lionfood <- rbind(c("Impala", "Hartebeest", "Duiker_red", "Wildebeest", "Sable_antelope", 
                             "Waterbuck", "Oribi", "Warthog", "Bushpig", "Reedbuck", "Buffalo", 
                             "Eland", "Nyala", "Kudu", "Bushbuck"))
common.names.lionfoodbetter <- rbind(c("Impala", "Hartebeest", "Red duiker", "Common wildebeest", "Sable antelope", 
                                  "Waterbuck", "Oribi", "Warthog", "Bushpig", "Southern reedbuck", "African Buffalo", 
                                  "Eland", "Nyala", "Greater kudu", "Bushbuck"))

# plot ungulates against lions - DRY SEASON

par(mfrow = c(5,3)) 
# run loop to generate a plot for each species
for (i in 1:length(common.names.lionfood)) {
  dry.ungulate <- records.dry.year1and2[records.dry.year1and2$Species == common.names.lionfood[[i]],]$Time.Sun
  dry.lion <- records.dry.year1and2[records.dry.year1and2$Species == "Lion",]$Time.Sun
  overlapPlot2(dry.ungulate, dry.lion, main = common.names.lionfoodbetter[[i]], linetype = c(1, 2), linecol = c("#F8766D", "black"), linewidth = c(2, 1))
}

# plot ungulates against lions - WET SEASON

par(mfrow = c(5,3))
# run loop to generate a plot for each species
for (i in 1:length(common.names.lionfood)) {
  wet.ungulate <- records.wet.year1and2[records.wet.year1and2$Species == common.names.lionfood[[i]],]$Time.Sun
  wet.lion <- records.wet.year1and2[records.wet.year1and2$Species == "Lion",]$Time.Sun
  overlapPlot2(wet.ungulate, wet.lion, main = common.names.lionfoodbetter[[i]], linetype = c(1, 2), linecol = c("#00BFC4", "black"), linewidth = c(2, 1))
}


# calculate change in dry vs wet overlap for oribi, reedbuck, sable

dry.lion <- records.dry.year1and2[records.dry.year1and2$Species == "Lion",]$Time.Sun
dry.oribi <- records.dry.year1and2[records.dry.year1and2$Species == "Oribi",]$Time.Sun
dry.reedbuck <- records.dry.year1and2[records.dry.year1and2$Species == "Reedbuck",]$Time.Sun
dry.sable <- records.dry.year1and2[records.dry.year1and2$Species == "Sable_antelope",]$Time.Sun

wet.lion <- records.wet.year1and2[records.wet.year1and2$Species == "Lion",]$Time.Sun
wet.oribi <- records.wet.year1and2[records.wet.year1and2$Species == "Oribi",]$Time.Sun
wet.reedbuck <- records.wet.year1and2[records.wet.year1and2$Species == "Reedbuck",]$Time.Sun
wet.sable <- records.wet.year1and2[records.wet.year1and2$Species == "Sable_antelope",]$Time.Sun

bs_time.lion.dry <- resample(dry.lion, 10000)
bs_time.lion.wet <- resample(wet.lion, 10000)

### ORIBI

# dry
(Dhats_inout <- overlapEst(dry.lion, dry.oribi))  ## use Dhat4 formula (for sample sizes >50)
bs_time.oribi.dry <- resample(dry.oribi, 10000)
bsOut <- bootEst(bs_time.oribi.dry, bs_time.lion.dry)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

# wet
(Dhats_inout <- overlapEst(wet.lion, wet.oribi))  ## use Dhat4 formula (for sample sizes >50)
bs_time.oribi.wet <- resample(wet.oribi, 10000)
bsOut <- bootEst(bs_time.oribi.wet, bs_time.lion.wet)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

### REEDBUCK

# dry
(Dhats_inout <- overlapEst(dry.lion, dry.reedbuck))  ## use Dhat4 formula (for sample sizes >50)
bs_time.reedbuck.dry <- resample(dry.reedbuck, 10000)
bsOut <- bootEst(bs_time.reedbuck.dry, bs_time.lion.dry)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

# wet
(Dhats_inout <- overlapEst(wet.lion, wet.reedbuck))  ## use Dhat4 formula (for sample sizes >50)
bs_time.reedbuck.wet <- resample(wet.reedbuck, 10000)
bsOut <- bootEst(bs_time.reedbuck.wet, bs_time.lion.wet)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

### SABLE

# dry
(Dhats_inout <- overlapEst(dry.lion, dry.sable))  ## use Dhat4 formula (for sample sizes >50)
bs_time.sable.dry <- resample(dry.sable, 10000)
bsOut <- bootEst(bs_time.sable.dry, bs_time.lion.dry)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic

# wet
(Dhats_inout <- overlapEst(wet.lion, wet.sable))  ## use Dhat4 formula (for sample sizes >50)
bs_time.sable.wet <- resample(wet.sable, 10000)
bsOut <- bootEst(bs_time.sable.wet, bs_time.lion.wet)
colMeans(bsOut) ## dhat bootstrapped
bs <- as.vector(bsOut[,2])
(bsCI_inout <- bootCI(Dhats_inout[2], bs)) ## use basic