library(V.PhyloMaker)
library(tidyverse)
library(WorldFlora)


species_list <- read.csv("data/lenoir_data.csv")$species_acc
sp_match <- WFO.match(species_list, "data/wfo_list.txt")
sp_list <- sp_match %>%
  WFO.one() %>%
  mutate(species_new = paste(genus, specificEpithet)) %>%
  select(spec.name.ORIG, species_new, genus, family) %>%
  as_tibble() %>%
  select(species_new, genus, family) %>%
  rename(species = species_new) %>%
  distinct()


# generate the phylogeny presented in Figure 1a
tree <- phylo.maker(sp.list = sp_list, tree = GBOTB.extended, nodes = nodes.info.1, scenarios="S3")

write.tree(tree$scenario.3, "data/lenoir_phylogeny.tre")





