library(tidyverse)
library(vegan)
library(distances)


# load distances
trait_dist <- readRDS("data/trait_dist.rds")
phylo_dist <- readRDS("data/phylo_dist.rds")

test <- mantel(trait_dist, phylo_dist, permutations = 999, parallel = 15)

saveRDS(mantel_tests, "results/trait_phylo_mantel_test.rds")

