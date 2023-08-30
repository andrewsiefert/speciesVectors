library(tidyverse)
library(caret)
library(doParallel)


# prepare data ------------------------------------------------------------

# read in Lenoir range shifts data
bs <- read.csv("data/lenoir_data.csv") %>%
  rename(species = 2) %>%
  select(species, shift)

# read in species list
species <- read.csv("data/lenoir_cooccur_species_list.csv")

# read in GloVe species vectors
paths <- list.files("vectors/local", full.names = T) %>%
  grep("embedding", ., value = T)
embeddings <- map(paths, read.csv) %>% map(~rename_all(.x, ~paste0("d", 1:length(.x))))
names(embeddings) <- str_extract(paths, "model[0-9]+")

# read in and clean trait data
traits <- read.csv("data/trait/TRY_sPlotOpen.csv") %>%
  janitor::clean_names() %>%
  select(-n, -contains("_sd")) %>%
  rename_all(~str_remove(., "_mean")) %>%
  rename(species = 1) %>%
  na.omit() %>%
  semi_join(bs)

# read in phylogenetic eigenvectors
tree <- ape::read.tree("data/lenoir_phylogeny.tre")
pvr <- readRDS("data/pvr.rds")
eig <- pvr$vectors %>% as.data.frame()
eig$species <- str_replace(tree$tip.label, "_", " ")

# combine range shift, trait, and eigenvector data
d <- inner_join(species, bs) %>% inner_join(traits) %>% inner_join(eig)
y <- d$shift


# fit species vector models --------------------------------------------------------------

# set up repeated 10-fold CV
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

# create hyperparameter grid for elastic net
grid <- expand.grid(lambda = exp(-4:5),
                    alpha = seq(0, 1, 0.25))


n <- length(embeddings) # number of species vector sets

# loop over sets of species vectors
cl <- makeCluster(n)
registerDoParallel(cl)

tests <- foreach(i = 1:n, .packages = c("caret", "dplyr")) %dopar% {

  # extract species vectors
  # (each model produces two vectors for each species that differ only due to
  #  random initialization, so need to take average to get one vector per species)
  emb <- embeddings[[i]]
  dim <- (length(emb)-2)/2     # vector dimension
  b1 <- emb[,1]                # bias term 1
  b2 <- emb[,2]                # bias term 2
  b <- (b1+b2)/2               # average bias terms
  w <- emb[,-(1:2)]            # select vectors
  w1 <- w[,1:dim]              # extract first set of vectors
  w2 <- w[,(dim+1):length(w)]  # extract second set of vectors
  w <- (w1 + w2)/2             # take average to get one vector per species

  # create predictor matrices
  emb <- bind_cols(species, w) %>% cbind(b)
  d <- inner_join(emb, bs) %>% inner_join(traits) %>% inner_join(eig)                              # join vectors, trains, phylo eigenvectors
  X_emb <- d %>% select(starts_with("d"), b, -disp_unit_leng) %>% as_data_frame()                  # select vectors only
  X_et <- d %>% select(starts_with("d"), b, leaf_area:wood_vessel_length) %>% as_data_frame()      # select vectors and traits
  X_ep <- d %>% select(starts_with("d"), b, -disp_unit_leng, starts_with("c")) %>% as_data_frame() # select vectors and phylogenetic eigenvectors
  X_etp <- d %>% select(-id, -species, -shift) %>% as_data_frame()                                 # select all three

  # fit elastic net, species vectors only
  enet_emb <- train(x = X_emb, y = y,
                method = "glmnet",
                tuneGrid = grid,
                trControl = fitControl)

  # fit elastic net, species vectors and traits
  enet_emb_trait <- train(x = X_et, y = y,
                    method = "glmnet",
                    tuneGrid = grid,
                    trControl = fitControl)

  # fit elastic net, species vectors and phylogenetic eigenvectors
  enet_emb_phylo <- train(x = X_ep, y = y,
                          method = "glmnet",
                          tuneGrid = grid,
                          trControl = fitControl)

  # fit elastic net, all three
  enet_emb_trait_phylo <- train(x = X_etp, y = y,
                          method = "glmnet",
                          tuneGrid = grid,
                          trControl = fitControl)

  return(list(enet_emb, enet_emb_trait, enet_emb_phylo, enet_emb_trait_phylo))
}

stopCluster(cl)

# save results
names(tests) <- names(embeddings)
saveRDS(tests, "results/glove_local_vector_test_results_w_bias.rds")


# organize output ---------------------------------------------------------


# read in grid of species vector parameters
grid <- read.csv("code/train_vectors/local/grid.csv") %>% as_tibble()

# extract and combine model performance results in a single data frame
se <- function(x) sd(x)/sqrt(length(x))

out <- lapply(tests, function(m) {
  map_df(m, ~data.frame(rmse = mean(.$resample$RMSE),
                        rmse_se = se(.$resample$RMSE),
                        r2 = mean(.$resample$Rsquared),
                        r2_se = se(.$resample$Rsquared),
                        mae = mean(.$resample$MAE),
                        mae_se = se(.$resample$MAE)))
}) %>%
  bind_rows() %>%
  mutate(model = rep(names(tests), each = 4) %>% str_extract("[0-9]+") %>% as.numeric(),
         test = rep(c("emb", "emb_trait", "emb_phylo", "all"), n)) %>%
  as_tibble() %>%
  left_join(grid) %>%
  arrange(model) %>%
  mutate(model = paste0("emb", model))


# plot results
out %>%
  filter(str_detect(model, "emb")) %>%
  ggplot(aes(x = dim, y = r2, color = factor(xmax))) +
  geom_point() +
  geom_path() +
  #geom_errorbar(aes(ymin = r2-r2_se, ymax = r2+r2_se)) +
  facet_wrap(~test)

out %>%
  filter(str_detect(model, "emb")) %>%
  ggplot(aes(x = dim, y = rmse, color = factor(xmax))) +
  geom_point() +
  geom_path() +
  #geom_errorbar(aes(ymin = rmse-rmse_se, ymax = rmse+rmse_se)) +
  facet_wrap(~test)

out %>%
  filter(str_detect(model, "emb")) %>%
  ggplot(aes(x = dim, y = mae, color = factor(xmax))) +
  geom_point() +
  geom_path() +
  facet_wrap(~test)


# save results
write_csv(out, "results/glove_local_test_results_w_bias.csv")
