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

vectors <- d %>% select(X2:X17)
names(vectors) <- paste0("dim", 1:16)

traits <- d %>% select(leaf_area:wood_vessel_length)

trait_cor <- apply(vectors, 2, function(i) apply(traits, 2, function(j) round(cor(i, j), 2)))
trait_p <- apply(vectors, 2, function(i) apply(traits, 2, function(j) round(cor.test(i, j)$p.value, 3)))

write_csv(as.data.frame(trait_cor), "results/vector_trait_correlations.csv")
write_csv(as.data.frame(trait_p), "results/vector_trait_correlation_p_values.csv")


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

vectors <- d2 %>% select(X2:X17)
names(vectors) <- paste0("dim", 1:16)

geo <- d2 %>% select(latitude:soil_clim_pc2, -is_forest)

geo_cor <- apply(vectors, 2, function(i) apply(geo, 2, function(j) round(cor(i, j), 2)))
geo_p <- apply(vectors, 2, function(i) apply(geo, 2, function(j) round(cor.test(i, j)$p.value, 3)))

write_csv(as.data.frame(geo_cor), "results/vector_geo_correlations.csv")
write_csv(as.data.frame(geo_p), "results/vector_geo_correlation_p_values.csv")


