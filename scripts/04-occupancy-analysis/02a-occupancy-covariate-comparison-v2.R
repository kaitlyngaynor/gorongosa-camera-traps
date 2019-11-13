setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

# import the community covariates for each of the single-predictor models
# change row name to the variable name
dry.pan250 <- read.csv("Results/Single variable occ models/community_covariates_dry_pan250m.csv")
dry.pan250$X <- "pan250m"
dry.pan500 <- read.csv("Results/Single variable occ models/community_covariates_dry_pan500m.csv")
dry.pan500$X <- "pan500m"
dry.pandistance <- read.csv("Results/Single variable occ models/community_covariates_dry_pandistance.csv")
dry.pandistance$X <- "pandistance"
dry.poachingall<- read.csv("Results/Single variable occ models/community_covariates_dry_poachingall.csv")
dry.poachingall$X <- "poachingall"
dry.poachinghigh<- read.csv("Results/Single variable occ models/community_covariates_dry_poachinghigh.csv")
dry.poachinghigh$X <- "poachinghigh"
dry.roaddistance <- read.csv("Results/Single variable occ models/community_covariates_dry_roaddistance.csv")
dry.roaddistance$X <- "roaddistance"
dry.roadmajordistance <- read.csv("Results/Single variable occ models/community_covariates_dry_roadmajordistance.csv")
dry.roadmajordistance$X <- "roadmajordistance"
dry.termitecount50m <- read.csv("Results/Single variable occ models/community_covariates_dry_termitecount50m.csv")
dry.termitecount50m$X <- "termitecount50m"
dry.termitecount100m <- read.csv("Results/Single variable occ models/community_covariates_dry_termitecount100m.csv")
dry.termitecount100m$X <- "termitecount100m"
dry.termitelargecount50m <- read.csv("Results/Single variable occ models/community_covariates_dry_termitelargecount50m.csv")
dry.termitelargecount50m$X <- "termitelargecount50m"
dry.termitelargecount100m <- read.csv("Results/Single variable occ models/community_covariates_dry_termitelargecount100m.csv")
dry.termitelargecount100m$X <- "termitelargecount100m"
dry.termitecount50m <- read.csv("Results/Single variable occ models/community_covariates_dry_termitecount50m.csv")
dry.termitecount50m$X <- "termitecount50m"
dry.termites100 <- read.csv("Results/Single variable occ models/community_covariates_dry_termites100m.csv")
dry.termites100$X <- "termites100m"
dry.termites250 <- read.csv("Results/Single variable occ models/community_covariates_dry_termites250m.csv")
dry.termites250$X <- "termites250m"
dry.termites500 <- read.csv("Results/Single variable occ models/community_covariates_dry_termites500m.csv")
dry.termites500$X <- "termites500m"
dry.termites1000 <- read.csv("Results/Single variable occ models/community_covariates_dry_termites1000m.csv")
dry.termites1000$X <- "termites1000m"
dry.tree100 <- read.csv("Results/Single variable occ models/community_covariates_dry_tree100m.csv")
dry.tree100$X <- "tree100m"
dry.tree250 <- read.csv("Results/Single variable occ models/community_covariates_dry_tree250m.csv")
dry.tree250$X <- "tree250m"
dry.tree500 <- read.csv("Results/Single variable occ models/community_covariates_dry_tree500m.csv")
dry.tree500$X <- "tree500m"
dry.tree1000 <- read.csv("Results/Single variable occ models/community_covariates_dry_tree1000m.csv")
dry.tree1000$X <- "tree1000m"

# combine the dry season dataframes
dry <- rbind (dry.pan250, dry.pan500, dry.pandistance, dry.poachingall, dry.poachinghigh,
              dry.roaddistance, dry.roadmajordistance, dry.termitecount100m, dry.termitecount50m,
              dry.termitelargecount100m, dry.termitelargecount50m, dry.termites100, dry.termites250,
              dry.termites500, dry.termites1000, dry.tree100, dry.tree250, dry.tree500, dry.tree1000)

# add column for season
dry$Season <- "dry"

# add column for category
dry$Category <- c("pan", "pan", "pan", "poaching", "poaching", "road", "road", "termite", "termite", "termite",
                     "termite", "termite", "termite", "termite", "termite", "tree", "tree", "tree", "tree")

# now do the same for wet

# import the community covariates for each of the single-predictor models
# change row name to the variable name
wet4mo.pan250 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_pan250m.csv")
wet4mo.pan250$X <- "pan250m"
wet4mo.pan500 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_pan500m.csv")
wet4mo.pan500$X <- "pan500m"
wet4mo.pandistance <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_pandistance.csv")
wet4mo.pandistance$X <- "pandistance"
wet4mo.poachingall<- read.csv("Results/Single variable occ models/community_covariates_wet4mo_poachingall.csv")
wet4mo.poachingall$X <- "poachingall"
wet4mo.poachinghigh<- read.csv("Results/Single variable occ models/community_covariates_wet4mo_poachinghigh.csv")
wet4mo.poachinghigh$X <- "poachinghigh"
wet4mo.roaddistance <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_roaddistance.csv")
wet4mo.roaddistance$X <- "roaddistance"
wet4mo.roadmajordistance <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_roadmajordistance.csv")
wet4mo.roadmajordistance$X <- "roadmajordistance"
wet4mo.termitecount50m <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termitecount50m.csv")
wet4mo.termitecount50m$X <- "termitecount50m"
wet4mo.termitecount100m <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termitecount100m.csv")
wet4mo.termitecount100m$X <- "termitecount100m"
wet4mo.termitelargecount50m <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termitelargecount50m.csv")
wet4mo.termitelargecount50m$X <- "termitelargecount50m"
wet4mo.termitelargecount100m <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termitelargecount100m.csv")
wet4mo.termitelargecount100m$X <- "termitelargecount100m"
wet4mo.termitecount50m <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termitecount50m.csv")
wet4mo.termitecount50m$X <- "termitecount50m"
wet4mo.termites100 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termites100m.csv")
wet4mo.termites100$X <- "termites100m"
wet4mo.termites250 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termites250m.csv")
wet4mo.termites250$X <- "termites250m"
wet4mo.termites500 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termites500m.csv")
wet4mo.termites500$X <- "termites500m"
wet4mo.termites1000 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_termites1000m.csv")
wet4mo.termites1000$X <- "termites1000m"
wet4mo.tree100 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_tree100m.csv")
wet4mo.tree100$X <- "tree100m"
wet4mo.tree250 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_tree250m.csv")
wet4mo.tree250$X <- "tree250m"
wet4mo.tree500 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_tree500m.csv")
wet4mo.tree500$X <- "tree500m"
wet4mo.tree1000 <- read.csv("Results/Single variable occ models/community_covariates_wet4mo_tree1000m.csv")
wet4mo.tree1000$X <- "tree1000m"

# combine the wet season dataframes
wet4mo <- rbind (wet4mo.pan250, wet4mo.pan500, wet4mo.pandistance, wet4mo.poachingall, wet4mo.poachinghigh,
              wet4mo.roaddistance, wet4mo.roadmajordistance, wet4mo.termitecount100m, wet4mo.termitecount50m,
              wet4mo.termitelargecount100m, wet4mo.termitelargecount50m, wet4mo.termites100, wet4mo.termites250,
              wet4mo.termites500, wet4mo.termites1000, wet4mo.tree100, wet4mo.tree250, wet4mo.tree500, wet4mo.tree1000)

# add column for season
wet4mo$Season <- "wet4mo"

# add column for category
wet4mo$Category <- c("pan", "pan", "pan", "poaching", "poaching", "road", "road", "termite", "termite", "termite",
                     "termite", "termite", "termite", "termite", "termite", "tree", "tree", "tree", "tree")

# combine dry and wet
all <- rbind(dry,wet4mo)

# fix column names (so X is "Covariate")
colnames(all) <- c("Covariate", "Mean", "SD", "LCI", "UCI", "Season", "Category")

# take a look at the spreadsheet
View(all)

# export csv
write.csv(all, "Results/covariate_comparison.csv", row.names=F)
