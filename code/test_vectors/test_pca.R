library(tidyverse)
library(caret)
library(doParallel)


# prepare data ------------------------------------------------------------

# read in Lenoir range shifts data
bs <- read.csv("data/lenoir_data.csv") %>%
  rename(species = 2) %>%
  select(species, shift)

# read in sPlotOpen species list
species <- read.csv("data/sPlot_cooccur_species_list.csv")

# read in PCA loadings
paths <- list.files("vectors/pca", full.names = T)
pca <- map(paths, readRDS)
loadings <- map(pca, ~.x$loadings)
names(loadings) <- paste0("pca", str_extract(paths, "[0-9]+"))

# read in and clean trait data
traits <- read.csv("data/trait/TRY_sPlotOpen.csv") %>%
  janitor::clean_names() %>%
  select(-n, -contains("_sd")) %>%
  rename_all(~str_remove(., "_mean")) %>%
  rename(species = 1) %>%
  na.omit() %>%
  semi_join(bs)

# combine range shift and trait data
d <- inner_join(species, bs) %>% inner_join(traits)
y <- d$shift



# fit range shift models --------------------------------------------------------------

# set up repeated 10-fold CV
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

n <- length(loadings)   # number of sets of PCA loadings

# loop over sets of loadings
cl <- makeCluster(n)
registerDoParallel(cl)

tests <- foreach(i = 1:n, .packages = c("caret", "dplyr")) %dopar% {

  # extract loadings
  emb <- bind_cols(species, as_tibble(loadings[[i]]))

  # join with range shift data
  d <- inner_join(emb, bs) %>% inner_join(traits)
  X <- d %>% select(contains("V"), -wood_vessel_length) %>% as_tibble()

  # fit elastic net
  enet <- train(x = X, y = y, method = "enet", tuneLength = 5,
                trControl = fitControl)

  return(enet)
}

stopCluster(cl)

# save results
names(tests) <- names(loadings)
saveRDS(tests, "results/pca_test_results.rds")


# organize output ---------------------------------------------------------

se <- function(x) sd(x)/sqrt(length(x))    # function to calculate standard error

# extract and combine model performance results in a single data frame
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

# save results
write_csv(out, "results/pca_test_results.csv")
