library(tidyverse)
library(ape)
library(PVR)

# read in phylogenetic tree
tree <- read.tree("data/lenoir_phylogeny.tre")

# calculate phylogenetic eigenvectors
pvr <- PVRdecomp(tree)@Eigen

# save phylogenetic eigenvectors
saveRDS(pvr, "data/pvr.rds")
