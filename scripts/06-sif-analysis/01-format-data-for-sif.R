setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

library(R2jags); library(reshape); library(plyr)

# import species codes and traits
Spp <- read.csv("Data/2018spp_kingdon.csv") # Species IDs and traits

# import seasonal detections
D.dry.year1 <- read.csv("Data/Cleaned_occupancy_records/Detections_7_1_16_9_30_16.csv")
D.wet3mo.year1 <- read.csv("Data/Cleaned_occupancy_records/Detections_1_1_17_3_31_17.csv")
D.wet4mo.year1 <- read.csv("Data/Cleaned_occupancy_records/Detections_12_1_16_3_31_17.csv")
D.dry.year2 <- read.csv("Data/Cleaned_occupancy_records/Detections_7_1_17_9_30_17.csv")
D.wet3mo.year2 <- read.csv("Data/Cleaned_occupancy_records/Detections_1_1_18_3_31_18.csv")
D.wet4mo.year2 <- read.csv("Data/Cleaned_occupancy_records/Detections_12_1_17_3_31_18.csv")

# version with only LMH
D.dry.year1.lmh <- dplyr::select(D.dry.year1, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.wet3mo.year1.lmh <- dplyr::select(D.wet3mo.year1, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.wet4mo.year1.lmh <- dplyr::select(D.wet4mo.year1, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.dry.year2.lmh <- dplyr::select(D.dry.year2, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.wet3mo.year2.lmh <- dplyr::select(D.wet3mo.year2, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.wet4mo.year2.lmh <- dplyr::select(D.wet4mo.year2, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)

# import seasonal camera operation dates
Camop.dry.year1 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_7_1_16_9_30_16.csv")
Camop.wet3mo.year1 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_1_1_17_3_31_17.csv")
Camop.wet4mo.year1 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_12_1_16_3_31_17.csv")
Camop.dry.year2 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_7_1_17_9_30_17.csv")
Camop.wet3mo.year2 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_1_1_18_3_31_18.csv")
Camop.wet4mo.year2 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_12_1_17_3_31_18.csv")

# make versions where we count the cameras twice
# copy year 2 over temporarily
D.dry.year2.v2 <- D.dry.year2
Camop.dry.year2.v2 <- Camop.dry.year2
D.wet4mo.year2.v2 <- D.wet4mo.year2
Camop.wet4mo.year2.v2 <- Camop.wet4mo.year2
# change camera names
D.dry.year2.v2$StudySite <- paste(D.dry.year2$StudySite, "_2", sep="")
Camop.dry.year2.v2$StudySite <- paste(Camop.dry.year2$StudySite, "_2", sep="")
D.wet4mo.year2.v2$StudySite <- paste(D.wet4mo.year2$StudySite, "_2", sep="")
Camop.wet4mo.year2.v2$StudySite <- paste(Camop.wet4mo.year2$StudySite, "_2", sep="")
# combine
D.dry.year1and2 <- rbind(D.dry.year1, D.dry.year2.v2)
Camop.dry.year1and2 <- rbind(Camop.dry.year1, Camop.dry.year2.v2)
D.wet4mo.year1and2 <- rbind(D.wet4mo.year1, D.wet4mo.year2.v2)
Camop.wet4mo.year1and2 <- rbind(Camop.wet4mo.year1, Camop.wet4mo.year2.v2)
D.dry.year1and2.lmh <- dplyr::select(D.dry.year1and2, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)
D.wet4mo.year1and2.lmh <- dplyr::select(D.wet4mo.year1and2, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)

# specify which one season to use (from above files that were just imported)
D <- D.dry.year1and2.lmh
Camop <- Camop.dry.year1and2

# Remove columns with species to be excluded from analysis
#Spp$SppCode # lists the species codes in that file (38 levels)
#nrow(Spp) # there are 38 species with traits (all mammals)
#names(D) # all species currently included in detection matrix (42 currently - we need to exclude Guinefaowl_crested, Guineafowl_helmeted, Hornbill_ground, and Human
#D <- D[,c(1:12, 15:18, 21:43)] # take only the non-human mammals
#names(D)
#ncol(D) - 1 # confirming 38 species in this file now

# melt detections
D <- melt(D,vars = c("StudySite"))
colnames(D)[2:3] = c("SppCode","Detections")
head(D) # make sure it worked

# Join the indepedent data files
D <- join(D, Camop)

# Confirm columns for SIF analysis (StudySite, SppCode, Detections, Operation)
head(D)

# Create a data frame that gives every combination of all SppCodes for each site
SPP1<- expand.grid(unique(D$SppCode),unique(D$SppCode),unique(D$StudySite))

# Remove the rows where Spp1 = Spp2
SPP1 <- SPP1[SPP1[,1]!=SPP1[,2],]

# Remove duplicate pairs of species (where Species 1 in row i = Species 2 in row i+1 and Species 2 in row i = Species 1 in row i+1)
dat.sort = t(apply(SPP1, 1, sort))
SPP1 <- SPP1[!duplicated(dat.sort),]

# Assign column names
colnames(SPP1)<-c('SppCode1','SppCode2','StudySite')

# Merge the list of unique species combinations at each site with their number of detections and num.nights
SP1<-merge(SPP1, D, by.x=c('SppCode1','StudySite'), by.y=c('SppCode','StudySite'))
SP1<-merge(SP1, D, by.x=c('SppCode2','StudySite'), by.y=c('SppCode','StudySite'))

# Rearrange the output by column
SP1<-SP1[,c(2,3,1,4,6,5)]

# Assign new column names
colnames(SP1)<-c('StudySite','SppCode1','SppCode2','Spp1Det','Spp2Det','Operation')

# Export file
#write.table(SP1,"Data/SIF/original/Wet.year1and2.SppPairs.csv",sep=",", row.names=F)
write.table(SP1,"Data/SIF/original/Dry.year1and2.SppPairs.csv",sep=",", row.names=F)

# I then removed all cameras with <10 days of operation during the study period. Only relevant for wet.

# copy into new table, removing records with camera operation <10 days
SP2 <- SP1[SP1$Operation >= 10, ] # subset
SP2$StudySite <- droplevels(SP2$StudySite) # drop study site levels that are now excluded
nlevels(SP2$StudySite) # number of cameras that leaves; it's 60 for dry, 56 for Wet.peak.4mo, and only 37 for Wet.peak.3mo


# Export these into a new folder, "fewer_cameras." Or here's the code for that:
# write.table(SP2,"Data/SIF/fewer_cameras/Dry.peak.SppPairs.csv",sep=",", row.names=F)
#write.table(SP2,"Data/SIF/fewer_cameras/Wet.year1and2.SppPairs.csv",sep=",", row.names=F)
write.table(SP2,"Data/SIF/fewer_cameras/Dry.year1and2.SppPairs.csv",sep=",", row.names=F)
# write.table(SP2,"Data/SIF/fewer_cameras/Wet.peak.4mo.SppPairs.csv",sep=",", row.names=F)
