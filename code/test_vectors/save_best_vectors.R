library(tidyverse)


# read in sPlotOpen species list
species <- read_csv("data/sPlot_cooccur_species_list.csv") %>% select(species)

# read in GloVe species vectors
vectors <- read_csv("vectors/global/model6_embedding.csv")

dim <- (length(vectors)-2)/2     # vector dimension
b1 <- vectors[,1]                # bias term 1
b2 <- vectors[,2]                # bias term 2
b <- (b1+b2)/2                   # average bias terms
w <- vectors[,-(1:2)]            # select vectors
w1 <- w[,1:dim]                  # extract first set of vectors
w2 <- w[,(dim+1):length(w)]      # extract second set of vectors
w <- (w1 + w2)/2                 # take average to get one vector per species
colnames(w) <- paste0("d", 1:ncol(w))

# combine species names, bias terms, vectors
out <- bind_cols(species, b, w) %>%
  rename(bias = 2)

# save vectors
write_csv(out, "vectors/species_vectors_16dim_w_bias.csv")
