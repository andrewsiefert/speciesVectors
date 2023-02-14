library(V.PhyloMaker)
library(tidyverse)
library(WorldFlora)
library(doParallel)


species_list <- read.csv("data/sPlot_cooccur_species_list.csv")$species

wfo <- read.delim("data/wfo_list.txt") %>%
  filter(infraspecificEpithet == "")

spp_list <- split(species_list, rep(1:20, each = 2000))

n <- length(spp_list)
cl <- makeCluster(n)
registerDoParallel(cl)

spp_match <- foreach(i = 1:n, .errorhandling = "pass", .packages = "WorldFlora") %dopar% {
  WFO.match(spp_list[[i]], WFO.data = wfo)
}

stopCluster(cl)

sp_list <- spp_match %>%
  map(WFO.one) %>%
  bind_rows() %>%
  mutate(species_new = paste(genus, specificEpithet)) %>%
  select(spec.name.ORIG, species_new, genus, family) %>%
  as_tibble()

phylo_list <- sp_list %>%
  select(species_new, genus, family) %>%
  rename(species = species_new) %>%
  filter(str_length(species) > 1) %>%
  distinct()


# generate the phylogeny presented in Figure 1a
tree <- phylo.maker(sp.list = phylo_list, tree = GBOTB.extended, nodes = nodes.info.1, scenarios="S3")

write.tree(tree$scenario.3, "data/sPlotOpen_phylogeny.tre")
