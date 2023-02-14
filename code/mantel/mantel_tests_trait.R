library(tidyverse)
library(vegan)
library(distances)


# load embeddings
paths <- list.files("vectors/global", full.names = T) %>%
  grep("embedding", ., value = T)
embeddings <- map(paths, read.csv) %>% map(~rename_all(.x, ~paste0("d", 1:length(.x))))
names(embeddings) <- str_extract(paths, "model[0-9]+")

xw <- read.csv("data/species_cross_reference.csv")
emb_ind <- xw$emb_ind

# load trait distances
trait_dist <- readRDS("data/trait_dist.rds")


mantel_tests <- list()
for(i in 1:length(embeddings)) {

  emb <- embeddings[[i]][emb_ind,]
  dim <- (length(emb)-2)/2
  #b1 <- emb[,1]
  #b2 <- emb[,2]
  #b <- (b1+b2)/2
  w <- emb[,-(1:2)]
  w1 <- w[,1:dim]
  w2 <- w[,(dim+1):length(w)]
  w <- (w1 + w2)/2

  emb_dist <- distances(w) %>% distance_matrix()

  mantel_tests[[i]] <- mantel(trait_dist, emb_dist, permutations = 999, parallel = 15)

}


saveRDS(mantel_tests, "results/trait_mantel_tests.rds")

