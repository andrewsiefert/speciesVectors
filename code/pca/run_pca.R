library(flashpcaR)

setwd("~")
X <- readRDS("projects/speciesVectors/data/sPlot_site_species_matrix_hellinger.rds")
X <- as.matrix(X)

dim <- c(4, 8, 16, 32, 64)

for(d in dim) {
  pca <- flashpca(X, ndim = d, stand = "none", divisor = "none",
                  do_loadings = T, seed = 42)
  saveRDS(pca, paste0("../../gscratch/asiefer1/projects/speciesVectors/vectors/pca_", d, "dim.rds"))
}
