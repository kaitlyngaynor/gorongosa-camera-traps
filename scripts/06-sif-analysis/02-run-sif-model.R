setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

library(R2jags)

# DRY

#Load the datafile, which includes the species codes for each pair 
#(SppCode1, SppCode2), the number of detections for each species (Spp1Det, Spp2Det) and 
#the total number of occasions (Operation) at each camera station.

data.dry <- read.csv("Data/SIF/fewer_cameras/Dry.year1and2.SppPairs.csv", header = TRUE)

ni <- 80000
nt <- 1
nb <- 10000
nc <- 3

#Specify the parameters to monitor
parameters <- c('SIF', 'psiA', 'psiBA', 'psiBa', 'psiB', 'pA', 'pB')

#Specify the index used to loop through all 768 species pairs
index.dry <- unique(data.dry[ , c("SppCode1", "SppCode2")])

#Create a matrix 'result' to store the results for all 703 species pairs
# KG: I'm not actually sure what the "35" is for. In the initial code that Lindsey ran, there were 37 species; in this code, there are 38. 
# I first tried to run by setting this to 38, but got an error "Error in result[j ,] <- outsav: number of items to replace is not a multiple of replacement length
# So I'm not sure what's up. I am now rerunning with 35 (10/31/18)
# 11/12/18 - seems like the 35 is the number of columns!
result.dry <- matrix(NA, nrow(index.dry), 35)

# DRY

#Loop through the matrix 'index' to run the independent analyses for all species pairs and #save the results to 'result'
for(j in 1:nrow(index.dry)){
  data1 <- list(yA = data.dry[data.dry$SppCode1 == index.dry$SppCode1[j] & data.dry$SppCode2 == index.dry$SppCode2[j], 'Spp1Det'], 
                yB = data.dry[data.dry$SppCode1 == index.dry$SppCode1[j] & data.dry$SppCode2 == index.dry$SppCode2[j], 'Spp2Det'], 
                N = data.dry[data.dry$SppCode1 == index.dry$SppCode1[j] & data.dry$SppCode2 == index.dry$SppCode2[j], 'Operation'], 
                n = length(unique(data.dry[data.dry$SppCode1 == index.dry$SppCode1[j] & data.dry$SppCode2 == index.dry$SppCode2[j],'StudySite'])))
  
  inits <-function() {list(zA = as.numeric(as.numeric(data1$yA>0)), zB = as.numeric(as.numeric(data1$yB>0)))}
  
  out <- jags(parameters.to.save = parameters, "Scripts/SIF/model2.txt", inits = inits, data = data1, n.chains = nc, n.burnin = nb, n.iter = ni, n.thin = nt)
  
  outsav <- t(c(out$BUGSoutput$summary['SIF',c(1,2,3,7,8)], out$BUGSoutput$summary['psiA',c(1,2,3,7,8)], out$BUGSoutput$summary['psiBA',c(1,2,3,7,8)], out$BUGSoutput$summary['psiBa',c(1,2,3,7,8)], out$BUGSoutput$summary['psiB',c(1,2,3,7,8)], out$BUGSoutput$summary['pA',c(1,2,3,7,8)], out$BUGSoutput$summary['pB',c(1,2,3,7,8)]))
  
  result.dry[j,]<- outsav
}

#Bind the 'result' with the respective row in 'index'
result.dry  <-  cbind(index.dry, result.dry)

#Rename columns
colnames(result.dry) <-c("SppCode1","SppCode2","SIF","sd","2.50%","97.50%", "psiA", "sd", "2.50%", "97.50%", "psiBA", "sd", 
  "2.50%", "97.50%", "psiBa", "sd", "2.50%", "97.50%", "psiB", "sd","2.50%", "97.50%", "pA", "sd", "2.50%", "97.50%", "pB", "sd", "2.50%", "97.50%")

write.table(result.dry,"Results/SIF/SIF_2018DryYear1and2.csv",sep=",", row.names=F) # perhaps a mistake to name thusly, analyses were 2018 but data was 2016


# Something strange happening here: There are 37 columns (35 parameters), but only 30 column names (28 for), so the last seven columns are NA
# there should be 7 parameters with 4 columns each (mean, sd, 2.5%, 97.5%)
# For each of the parameters, there are five columns saved to the results (see code above in outsave <-t(c(out...))), but I'm not sure what the fifth column is
# the mystery fifth column has no name, is very close to a value of 1.00 for all of the parameters
# just going to manually delete that fifth column and adjust names accordingly.


# WET

rm(list=ls())

data.wet <- read.csv("Data/SIF/fewer_cameras/Wet.year1and2.SppPairs.csv", header = TRUE)

ni <- 80000
nt <- 1
nb <- 10000
nc <- 3

parameters <- c('SIF', 'psiA', 'psiBA', 'psiBa', 'psiB', 'pA', 'pB')

index.wet <- unique(data.wet[ , c("SppCode1", "SppCode2")])

result.wet <- matrix(NA, nrow(index.wet), 35)

for(j in 1:nrow(index.wet)){
  data1 <- list(yA = data.wet[data.wet$SppCode1 == index.wet$SppCode1[j] & data.wet$SppCode2 == index.wet$SppCode2[j], 'Spp1Det'], 
                yB = data.wet[data.wet$SppCode1 == index.wet$SppCode1[j] & data.wet$SppCode2 == index.wet$SppCode2[j], 'Spp2Det'], 
                N = data.wet[data.wet$SppCode1 == index.wet$SppCode1[j] & data.wet$SppCode2 == index.wet$SppCode2[j], 'Operation'], 
                n = length(unique(data.wet[data.wet$SppCode1 == index.wet$SppCode1[j] & data.wet$SppCode2 == index.wet$SppCode2[j],'StudySite'])))
  
  inits <-function() {list(zA = as.numeric(as.numeric(data1$yA>0)), zB = as.numeric(as.numeric(data1$yB>0)))}
  
  out <- jags(parameters.to.save = parameters, "Scripts/SIF/model2.txt", inits = inits, data = data1, n.chains = nc, n.burnin = nb, n.iter = ni, n.thin = nt)
  
  outsav <- t(c(out$BUGSoutput$summary['SIF',c(1,2,3,7,8)], out$BUGSoutput$summary['psiA',c(1,2,3,7,8)], out$BUGSoutput$summary['psiBA',c(1,2,3,7,8)], out$BUGSoutput$summary['psiBa',c(1,2,3,7,8)], out$BUGSoutput$summary['psiB',c(1,2,3,7,8)], out$BUGSoutput$summary['pA',c(1,2,3,7,8)], out$BUGSoutput$summary['pB',c(1,2,3,7,8)]))
  
  result.wet[j,]<- outsav
}

#Bind the 'result' with the respective row in 'index'
result.wet  <-  cbind(index.wet, result.wet)

#Rename columns
colnames(result.wet) <-c("SppCode1","SppCode2","SIF","sd","2.50%","97.50%", "psiA", "sd", "2.50%", "97.50%", "psiBA", "sd", 
                         "2.50%", "97.50%", "psiBa", "sd", "2.50%", "97.50%", "psiB", "sd","2.50%", "97.50%", "pA", "sd", "2.50%", "97.50%", "pB", "sd", "2.50%", "97.50%")

write.table(result.wet,"Results/SIF/SIF_2018WetYear1and2.csv",sep=",", row.names=F) # perhaps a mistake to name thusly, analyses were 2018 but data was 2016


