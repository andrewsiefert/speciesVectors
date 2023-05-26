library(tidyverse)
library(Matrix)


# read in sPlot data
d <- read_tsv("data/sPlotOpen.txt") %>% distinct(PlotObservationID, Species)

# drop bad species and plots with only one species
keep_sp <- d %>%
  count(Species) %>%
  filter(str_detect(Species, " "))

d2 <- d %>%
  semi_join(keep_sp) %>%
  add_count(PlotObservationID) %>%
  filter(n > 1)

nsp <- n_distinct(d2$Species)

# create site by species occurrence matrix
site_f <- factor(d2$PlotObservationID)
species_f <- factor(d2$Species)

s <- sparseMatrix(
  as.numeric(site_f),
  as.numeric(species_f),
  x = 1)

saveRDS(s, "data/sPlot_site_species_matrix.rds")


# calculate co-occurrence matrix
co <- as(t(s) %*% s, "TsparseMatrix")

# store co-occurrences as data frame
counts <- tibble(sp1 = co@i+1,
                 sp2 = co@j+1,
                 co = co@x) %>%
  filter(sp1 != sp2)

# create species lookup table
species <- tibble(id = 1:nrow(co),
                  species = levels(species_f))

# save data
write_csv(counts, "data/sPlot_cooccur_counts.csv")
write_csv(species, "data/sPlot_cooccur_species_list.csv")

