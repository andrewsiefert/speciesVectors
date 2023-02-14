library(tidyverse)
library(caret)
library(doParallel)


# prepare data ------------------------------------------------------------

bs <- read.csv("data/lenoir_data.csv") %>%
  rename(species = 2) %>%
  select(species, shift)

species <- read.csv("data/sPlot_cooccur_species_list.csv")

paths <- list.files("vectors/pca", full.names = T)
pca <- map(paths, readRDS)
loadings <- map(pca, ~.x$loadings)
names(loadings) <- paste0("pca", str_extract(paths, "[0-9]+"))

traits <- read.csv("data/trait/TRY_sPlotOpen.csv") %>%
  janitor::clean_names() %>%
  select(-n, -contains("_sd")) %>%
  rename_all(~str_remove(., "_mean")) %>%
  rename(species = 1) %>%
  na.omit() %>%
  semi_join(bs)

d <- inner_join(species, bs) %>% inner_join(traits)
y <- d$shift

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

# fit embedding models --------------------------------------------------------------

n <- length(loadings)

cl <- makeCluster(n)
registerDoParallel(cl)

tests <- foreach(i = 1:n, .packages = c("caret", "dplyr")) %dopar% {

  emb <- bind_cols(species, as_tibble(loadings[[i]]))

  d <- inner_join(emb, bs) %>% inner_join(traits)
  X <- d %>% select(contains("V"), -wood_vessel_length) %>% as_tibble()

  # elastic net
  enet <- train(x = X, y = y, method = "enet", tuneLength = 5,
                trControl = fitControl)

  return(enet)
}

stopCluster(cl)

names(tests) <- names(loadings)

saveRDS(tests, "results/pca_test_results.rds")

# organize output ---------------------------------------------------------

se <- function(x) sd(x)/sqrt(length(x))

out <- lapply(tests, function(i) {
  tibble(rmse = mean(i$resample$RMSE),
         rmse_se = se(i$resample$RMSE),
         r2 = mean(i$resample$Rsquared),
         r2_se = se(i$resample$Rsquared),
         mae = mean(i$resample$MAE),
         mae_se = se(i$resample$MAE))
}) %>%
  bind_rows() %>%
  mutate(dim = str_extract(names(tests), "[0-9]+") %>% as.numeric()) %>%
  select(dim, everything()) %>%
  arrange(dim)

write_csv(out, "results/pca_test_results.csv")
