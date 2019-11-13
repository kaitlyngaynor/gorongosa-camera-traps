library(R2jags)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)

setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# import operation tables for each year and season
Camop.dry1 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_7_1_16_9_30_16.csv")
Camop.wet1 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_12_1_16_3_31_17.csv")
Camop.dry2 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_7_1_17_9_30_17.csv")
Camop.wet2 <- read.csv("Data/Cleaned_occupancy_records/Camoperation_12_1_17_3_31_18.csv")

# remove all columns with 0s from operation
Camop.dry1 <- Camop.dry1[Camop.dry1$Operation > 0,]
Camop.wet1 <- Camop.wet1[Camop.wet1$Operation > 0,]
Camop.dry2 <- Camop.dry2[Camop.dry2$Operation > 0,]
Camop.wet2 <- Camop.wet2[Camop.wet2$Operation > 0,]

# change names of cameras (paste characters)
Camop.dry1$StudySite <- paste(Camop.dry1$StudySite, "_D1", sep="")
Camop.wet1$StudySite <- paste(Camop.wet1$StudySite, "_W1", sep="")
Camop.dry2$StudySite <- paste(Camop.dry2$StudySite, "_D2", sep="")
Camop.wet2$StudySite <- paste(Camop.wet2$StudySite, "_W2", sep="")

# bring in detections 
D.dry1 <- read.csv("Data/Cleaned_occupancy_records/Detections_7_1_16_9_30_16.csv")
D.wet1 <- read.csv("Data/Cleaned_occupancy_records/Detections_12_1_16_3_31_17.csv")
D.dry2 <- read.csv("Data/Cleaned_occupancy_records/Detections_7_1_17_9_30_17.csv")
D.wet2 <- read.csv("Data/Cleaned_occupancy_records/Detections_12_1_17_3_31_18.csv")

# change names of cameras (paste characters)
D.dry1$StudySite <- paste(D.dry1$StudySite, "_D1", sep="")
D.wet1$StudySite <- paste(D.wet1$StudySite, "_W1", sep="")
D.dry2$StudySite <- paste(D.dry2$StudySite, "_D2", sep="")
D.wet2$StudySite <- paste(D.wet2$StudySite, "_W2", sep="")

# rbind all into one spreadsheet
D <- rbind(D.dry1, D.wet1, D.dry2, D.wet2)
Camop <- rbind(Camop.dry1, Camop.wet1, Camop.dry2, Camop.wet2)

# select only LMH species
D <- dplyr::select(D, StudySite, AEME, ALBU, CENA, COTA, HIAM, HINI, KOEL, OUOU, PHAF, 
                   POLA, RERE, SYGR, SYCA, TAOR, TRAN, TRST, TRSY)

# save species names, will need them later
sppnames.lmh <- c("SppCode", "AEME", "ALBU", "CENA", "COTA", "HIAM", "HINI", "KOEL", 
                  "OUOU", "PHAF", "POLA", "RERE", "SYGR", "SYCA", "TAOR", "TRAN", "TRST", "TRSY")
sppnames2.lmh <- c("AEME", "ALBU", "CENA", "COTA", "HIAM", "HINI", "KOEL", "OUOU", 
                   "PHAF", "POLA", "RERE", "SYGR", "SYCA", "TAOR", "TRAN", "TRST", "TRSY")

# melt detections
D <- melt(D,vars = c("StudySite"))
colnames(D)[2:3] = c("SppCode","Detections")

# inefficiently only take detections from cameras that were operating (they are just 0s now)
D <- left_join(Camop, D)
D <- D[,c(1,3,4)]

# bring in year/season covariates
Covariates <- read.csv("Data/cam_metadata_yearseasononly.csv")
Spp <- read.csv("Data/2018spp_yearseasoncombined.csv")

# join everything together
D <- left_join(D, Covariates, by = "StudySite")
D <- left_join(D, Camop, by = "StudySite")
D <- left_join(D, Spp, by = "SppCode")

# convert back to factors, they had been coerced to characters
D$SppCode <- as.factor(D$SppCode)
D$StudySite <- as.factor(D$StudySite)

# select occupancy covariates
X <- dplyr::select(D, Year, Season)

# select detection covariates
dX = dplyr::select(D, Season)

# group covariates
G <- cbind(as.numeric(D$Floodplain=="Yes"),as.numeric(D$Floodplain=="No"))
XG = cbind(X*G[,1], X*G[,2])

# define data and parameters
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


# run occupancy model (see Gorongosa_occupancy.Rmd for annotation)
out3 <- jags(data = data, 
             inits = inits, 
             parameters.to.save = params, 
             model.file ="Scripts/Occupancy/AllMammals_pcov.txt", 
             n.chains = nc, 
             n.iter = ni,
             n.burnin = nb, 
             n.thin = nthin)
out3.sum <- out3$BUGSoutput$summary

write.table(x=out3.sum,file="Results/Occupancy/Results_combinedyearseason.csv",sep=",") # save output

alpha <- out3$BUGSoutput$sims.list$alpha
p <- out3$BUGSoutput$sims.list$P

expit <- function(x)  1/(1+exp(-x))
logit.alpha <- expit(alpha)
logit.p <- expit(p)

psimeans <- colMeans(logit.alpha)
names(psimeans) <- sppnames2.lmh
psimeans <- as.data.frame(psimeans)
write.table(x = psimeans, file = "Results/Occupancy/Combinedyearseason_alphaspsi()p().csv", sep = ",")

pmeans <- colMeans(logit.p)
names(pmeans) <- sppnames2.lmh
write.table(x = pmeans, file = "Results/Occupancy/Combinedyearseason_detection.csv", sep=",")

apply(logit.alpha, 2, function(x) sort(x)[])
psiCI <- apply(logit.alpha, 2, function(x) quantile(x,probs = c(0.025,0.1,0.5,0.9,0.975)))
colnames(psiCI) <- sppnames2.lmh
write.table(x=psiCI, file="Results/Occupancy/Combinedyearseason_alphaCI.psi()p().csv",sep=",")

apply(logit.p, 2, function(x) sort(x)[])
pCI <- apply(logit.p, 2, function(x) quantile(x,probs = c(0.025,0.1,0.5,0.9,0.975)))
colnames(pCI) <- sppnames2.lmh
write.table(x = pCI, file="Results/Occupancy/Combinedyearseason_pCI.psi()p().csv", sep = ",")

mbeta <- out3$BUGSoutput$sims.list$mbeta
gbeta <- out3$BUGSoutput$sims.list$gbeta
sbeta <- out3$BUGSoutput$sims.list$sbeta

covs <- colnames(X) # define covariates
sizes <- c("Yes", "No")
group <- data.frame(expand.grid(covs, sizes), matrix(NA, length(covs) * length(sizes), 4)) # create data frame where number of rows is equal to the number of covariates * the number of groups
colnames(group) <- c("Factor", "Group", "Mean", "SD", "LCI", "UCI")

for (a in 1:length(covs)){
  group[a,3:6] <- c(mean(mbeta[,a]),sd(mbeta[,a]),quantile(mbeta[,a],c(0.025,0.975)))
}

for (a in 1:length(covs)){
  for (b in 1:(length(sizes)-1)){
    sims <- mbeta[,a] + gbeta[,((b-1)*ncol(X)+a)]
    group[(ncol(X)*(b)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,c(0.025,0.975)))
  }
}

write.table(x = group, file = "Results/Occupancy/Combinedyearseason_group.csv", sep = ",", row.names=F)


# Species level estimates
spec <- Spp[,1]
levels(Spp$Floodplain) <- levels(Spp$Floodplain)[c(1,2)]
gg <- as.numeric(Spp$Floodplain)

covs <- colnames(X)
sizes <- c("Yes", "No")

species <- data.frame(expand.grid(covs,spec), matrix(NA,length(covs)*length(spec),4))
colnames(species) <- c("Factor","Species","Mean","SD","LCI","UCI")

gbeta <- cbind(gbeta, matrix(0,nrow(gbeta),length(covs)))

# Create a loop that will estimate species-specific values for each of the covariates
for (a in 1:length(covs)){
  for (b in 1:length(spec)){
    sims <- mbeta[,a] + gbeta[,((gg[b]-1)*ncol(X)+a)] + sbeta[,b,a]
    species[(ncol(X)*(b-1)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,c(0.025,0.975)))
  }
}

write.table(x=species,file="Results/Occupancy/Combinedyearseason_species.csv",sep=",", row.names=F) 



########################################################################################
## GET THIS RUNNING, NOT SURE HOW
########################################################################################

# Increase memory size to avoid errors (Windows-specific, no need)
# memory.limit(size=10000)

# Define the z matrix
z = out3$BUGSoutput$sims.list$Z

#export to figure out later
write.table(x=z,file="Results/Combinedyearseason_Zmatrix.csv",sep=",")

# Sor the data frame based on species, study site, and diet category
#d <- sort_df(merge(data.frame(ID = 1:nrow(D),D[,1:2]),data.frame(SppCode = spec, Group = Spp$Diet)),"ID")[,c(1,3,4)]
d <- sort_df(merge(data.frame(ID = 1:nrow(D),D[,1:2]),data.frame(SppCode = spec, Group = Spp$Floodplain)),"ID")[,c(1,3,4)]

# Create a new data frame
dz <- data.frame(d,t(z))

# Melt the data frame for easy casting
m.dz <- melt(dz,id.vars = c("SppCode","StudySite","Group") )

# Aggregate the data by summing the values in the z matrix for each camera station during each iteration
z.all <- acast(m.dz,StudySite ~ variable, fun.aggregate = sum)

# Use the aggregated values to create probability distributions and estimate mean, sd, and 95% credible interval values for camera-station specific species richness
z.all2 <- t(apply(z.all,1,function(x) c(mean(x),sd(x),quantile(x,c(0.025,0.975)))))
names <- rownames(z.all2)
rownames(z.all2) <- NULL
z.all2 <- cbind(names,z.all2)
colnames(z.all2) = c("StudySite", "Mean","SD","LCI","UCI")

# Export estimates of species richness as a table
write.table(x=z.all2,file="Results/Occupancy/Combinedyearseason_spprich.csv",sep=",")

