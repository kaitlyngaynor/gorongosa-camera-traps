setwd("~/Dropbox/projects/GORONGOSA2/Camera Trap Grid/R")

library(Hmisc)

# import metadata file
metadata <- read.csv("Data/cam_metadata_master.csv")
names(metadata)

# subset to include continuous covariates
metadata2 <- metadata[, c(9:14, 17:42)]

# determine Pearson's r coefficients and p-values for relationships between each of covariates
res <- rcorr(as.matrix(metadata2))

# define function for flattening the matrix (where each row is a pairwise comparison of covariates)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# flatten using above function
correlations <- flattenCorrMatrix(res$r, res$P) 
nrow(correlations) # 496 total pairs

# subset to include only significant relationships
correlations.sig <- correlations[correlations$p < 0.05,]
nrow(correlations.sig) # 141 of the pairs are significantly correlated
View(correlations.sig)

# Hm. seems like lots of these are correlated, and not only the ones that we'd think