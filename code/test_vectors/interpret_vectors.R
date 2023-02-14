library(tidyverse)


# Prepare data ------------------------------------------------------------

bs <- read.csv("data/lenoir_data.csv") %>%
  rename(species = 2) %>%
  select(species, shift)

traits <- read.csv("data/trait/TRY_sPlotOpen.csv") %>%
  janitor::clean_names() %>%
  select(-n, -contains("_sd")) %>%
  rename_all(~str_remove(., "_mean")) %>%
  rename(species = 1) %>%
  na.omit() %>%
  semi_join(bs)

tree <- ape::read.tree("data/lenoir_phylogeny.tre")
pvr <- readRDS("data/pvr.rds")
eig <- pvr$vectors %>% as.data.frame()
eig$species <- str_replace(tree$tip.label, "_", " ")


species <- read.csv("data/sPlot_cooccur_species_list.csv")

emb <- read.csv("vectors/global/model6_embedding.csv")

dim <- (length(emb)-2)/2
b1 <- emb[,1]
b2 <- emb[,2]
b <- (b1+b2)/2
w <- emb[,-(1:2)]
w1 <- w[,1:dim]
w2 <- w[,(dim+1):length(w)]
w <- (w1 + w2)/2

emb <- bind_cols(species, w) %>% cbind(b)
d <- inner_join(emb, bs) %>% inner_join(traits) %>% inner_join(eig) %>% as_tibble()
X <- d %>% select(contains("X"), b) %>% mutate_all(scale) %>% as.matrix()
y <- d$shift


# Get tuning parameters ----------------------------------------------------------

m <- readRDS("results/glove_global_vector_test_results_w_bias.rds")$model6[[1]]
alpha <- m$bestTune$alpha
lambda <- m$bestTune$lambda

fit <- glmnet::glmnet(X, y, alpha = alpha, lambda = lambda)




# Plot vector space -------------------------------------------------------

ggplot(d, aes(x = X2, y = X9, color = shift)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(x = "Dimension 1", y = "Dimension 8", color = "Range shift (m)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1)

ggsave("results/figures/lenoir_vector_space.png", height = 5, width = 6.5)


# Trait correlations ------------------------------------------------------

trait_cor1 <- apply(d %>% select(leaf_area:wood_vessel_length), 2, function(i) cor(i, d$X2))
trait_cor8 <- apply(d %>% select(leaf_area:wood_vessel_length), 2, function(i) cor(i, d$X9))

trait_p1 <- apply(d %>% select(leaf_area:wood_vessel_length), 2, function(i) cor.test(i, d$X2)$p.value)
trait_p8 <- apply(d %>% select(leaf_area:wood_vessel_length), 2, function(i) cor.test(i, d$X9)$p.value)

trait_cor <- tibble(variable = names(trait_cor1),
                  r1 = trait_cor1,
                  r8 = trait_cor8,
                  p1 = trait_p1,
                  p8 = trait_p8)

write_csv(trait_cor, "results/vector_trait_correlations.csv")


# Biogeography ------------------------------------------------------------

splot <- read_tsv("data/sPlotOpen.txt") %>% janitor::clean_names()
site <- read_tsv("data/sPlotOpen_header.txt") %>% janitor::clean_names()

splot <- splot %>%
  filter(species %in% d$species) %>%
  inner_join(site)

biome <- model.matrix(latitude ~ biome, data = splot) %>%
  as_tibble() %>% janitor::clean_names()


geo <- splot %>%
  bind_cols(biome) %>%
  select(-biome) %>%
  group_by(species) %>%
  summarize_at(vars(latitude, longitude, elevation, is_forest, forest, shrubland, grassland, wetland,
                    contains("biome"), soil_clim_pc1, soil_clim_pc2),
               mean, na.rm = T) %>%
  ungroup()


d2 <- inner_join(d, geo)

geo_cor1 <- apply(d2 %>% select(latitude:soil_clim_pc2), 2, function(i) cor(i, d2$X2))
geo_cor8 <- apply(d2 %>% select(latitude:soil_clim_pc2), 2, function(i) cor(i, d2$X9))

geo_p1 <- apply(d2 %>% select(latitude:soil_clim_pc2), 2, function(i) cor.test(i, d2$X2)$p.value)
geo_p8 <- apply(d2 %>% select(latitude:soil_clim_pc2), 2, function(i) cor.test(i, d2$X9)$p.value)

geo_cor <- tibble(variable = names(geo_cor1),
                  r1 = geo_cor1,
                  r8 = geo_cor8,
                  p1 = geo_p1,
                  p8 = geo_p8)

write_csv(geo_cor, "results/vector_geo_correlations.csv")
