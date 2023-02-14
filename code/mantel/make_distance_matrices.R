library(tidyverse)
library(distances)
library(ape)


lookup <- read.csv("data/species_lookup.csv") %>% as_tibble()
emb_spp <- read.csv("data/sPlot_cooccur_species_list.csv") %>%
  as_tibble() %>%
  rename(emb_ind = id)

# read in species embeddings
paths <- list.files("vectors/global", full.names = T) %>%
  grep("embedding", ., value = T)
embeddings <- map(paths, read.csv) %>% map(~rename_all(.x, ~paste0("d", 1:length(.x))))
names(embeddings) <- str_extract(paths, "model[0-9]+")

# read in traits
traits <- read.csv("data/trait/TRY_sPlotOpen.csv") %>%
  janitor::clean_names() %>%
  select(-n, -contains("_sd")) %>%
  rename_all(~str_remove(., "_mean")) %>%
  rename(species = 1) %>%
  na.omit() %>%
  as_tibble() %>%
  mutate(trait_ind = 1:n())

# read in phylogeny
phylo <- read.tree("data/sPlotOpen_phylogeny.tre")


trait_h <- inner_join(lookup, traits, by = c("species_new"="species"))
emb_h <- inner_join(lookup, emb_spp, by = c("spec.name.ORIG"="species"))
phylo_h <- tibble(phylo_ind = 1:length(phylo$tip.label),
                  species_new = phylo$tip.label) %>%
  mutate(species_new = str_replace(species_new, "_", " "))

species <- inner_join(trait_h, emb_h) %>%
  inner_join(phylo_h) %>%
  select(species_new, contains("ind")) %>%
  group_by(species_new) %>%
  slice(1L) %>%
  ungroup()

write.csv(species, "data/species_cross_reference.csv")

emb_ind <- species$emb_ind
trait_ind <- species$trait_ind
phylo_ind <- species$phylo_ind

# trait distance
trait_m <- traits[trait_ind,] %>%
  select(-species, -trait_ind) %>%
  mutate_all(scale) %>%
  as.matrix()

trait_dist <- distances(as.matrix(trait_m)) %>% distance_matrix()
saveRDS(trait_dist, "data/trait_dist.rds")

# phylogenetic distance
tree <- keep.tip(phylo, phylo_ind)
pdist <- cophenetic.phylo(tree) %>% as.dist()
phylo_lab <- attr(pdist, "Labels")
po <- order(phylo_lab)
phylo_dist <- as.matrix(pdist)[po,po] %>% as.dist()

saveRDS(phylo_dist, "data/phylo_dist.rds")


