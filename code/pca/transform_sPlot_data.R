# read in sPlot site-species matrix
X <- readRDS("data/sPlot_site_species_matrix.rds")

## Hellinger transformation

# calculate number of species in each site
totals <- rowSums(X)

# calculate Hellinger-transformed presence for each site
x_trans <- sqrt(1/totals)

# replace presences with Hellinger-transformed presences
X_trans <- X
row_ind <- X@i + 1
X_trans@x <- x_trans[row_ind]

# save transformed matrix
saveRDS(X_trans, "data/sPlot_site_species_matrix_hellinger.rds")

