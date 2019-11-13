rm(list=ls())

library(R2jags)
library(reshape)
library(reshape2)
library(plyr)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

zstd <- function (x)  (x-mean(x,na.rm = T))/sd(x,na.rm = T)  

#D <- read.csv("Data/Occupancy intermediates/D.dry.peak.csv")
#G <- read.csv("Data/Occupancy intermediates/G.dry.peak.csv")

 D <- read.csv("Data/Occupancy intermediates/D.wet4mo.peak.csv")
 G <- read.csv("Data/Occupancy intermediates/G.wet4mo.peak.csv")

# pull occupancy covariates from matrix of all covariates - JUST PICK ONE!
## X = as.matrix(D[,20])  # poaching.all
## X = as.matrix(D[,21]) # poaching.high
## X = as.matrix(D[,23]) # road.distance
## X = as.matrix(D[,24]) # road.major.distance
## X = as.matrix(D[,28]) # tree.100m
# X = as.matrix(D[,29]) # tree.250m
# X = as.matrix(D[,30]) # tree.500m
# X = as.matrix(D[,31]) # tree.1km
# X = as.matrix(D[,32]) # termites.100m
# X = as.matrix(D[,33]) # termites.250m
# X = as.matrix(D[,34]) # termites.500m
# X = as.matrix(D[,35]) # termites.1km
# X = as.matrix(D[,12]) # termite.count.100m
# X = as.matrix(D[,13]) # termite.count.50m
## X = as.matrix(D[,14]) # termite.large.count.100m
## X = as.matrix(D[,15]) # termite.large.count.50m
## X = as.matrix(D[,39]) # pans.100m - MODEL IS NOT RUNNING WITH THIS?
## X = as.matrix(D[,40]) # pans.250m
## X = as.matrix(D[,41]) # pans.500m
## X = as.matrix(D[,26]) # pan.distance

X <- apply(X,2,zstd) # standardize variables
head(X) # confirm these are the variables I want

## detection covariates - keep these the same
# here, I chose height.cm, angle, detect.obscured
dX = D[,8:10] # pull detection covariates from matrix of all covariates 
dX = apply(dX,2,zstd) # standardize variables
head(dX) # confirm these are the variables I want

## group covariates
XG = cbind(X*G[,1], X*G[,2], X*G[,3])

data <- list(D = D$Detections, # number of detections
             N = ceiling(D[,"Operation"]), # number of trap-nights for each camera
             Species = as.numeric(D$SppCode), # species
             n = nrow(D), 
             nspp = max(as.numeric(D$SppCode)),
             X = X, 
             XG = XG, 
             dX = dX)

# specify the initial values
inits = function() {list(Z = as.numeric(data$D>0))}

# specify the parameters to be monitored
params = c("rho","pbeta","spbeta","sigpbeta","mbeta","sigbeta","sbeta","gbeta","psi.mean","sigma.occ","p.mean","sigma.p","alpha","Z","P")

nc = 3       # number of chains
ni = 60000   # number of iterations
nb = 10000   # burn-in period
nthin = 50   # thinning rate

out3 <- jags(data = data, 
             inits = inits, 
             parameters.to.save = params, 
             model.file ="Scripts/AllMammals_pcov.txt", 
             n.chains = nc, 
             n.iter = ni,
             n.burnin = nb, 
             n.thin = nthin)

# extract community-level parameters for the covariate
mbeta <- out3$BUGSoutput$sims.list$mbeta

community.parameter <- data.frame(matrix(NA, 1, 4))
colnames(community.parameter) <- c("Mean", "SD", "LCI", "UCI")
community.parameter[1,1:4] <- c(mean(mbeta[,1]),sd(mbeta[,1]),quantile(mbeta[,1],c(0.025,0.975)))

# export
write.csv(community.parameter, "community_covariates_wet4mo_roadmajdist.csv")
