library(tidyverse)
library(ape)
library(PVR)


d <- read.csv("data/lenoir_data.csv")
tree <- read.tree("data/lenoir_phylogeny.tre") 

pvr <- PVRdecomp(tree)@Eigen

saveRDS(pvr, "data/pvr.rds")
