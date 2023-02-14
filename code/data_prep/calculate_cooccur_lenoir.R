library(tidyverse)
library(Matrix)


# read in sPlot data
d <- read_tsv("data/sPlotOpen.txt") %>% distinct(PlotObservationID, Species) %>% na.omit()

# read in Lenoir data
bs <- read_csv("data/lenoir_data.csv")

keep_plots <- d %>%
  group_by(PlotObservationID) %>%
  summarize(keep = any(Species %in% bs$species_acc)) %>%
  ungroup() %>%
  filter(keep)

d2 <- d %>%
  inner_join(keep_plots) %>%
  add_count(PlotObservationID) %>%
  filter(n > 1)

# create site by species occurrence matrix
site_f <- factor(d2$PlotObservationID)
species_f <- factor(d2$Species)

s <- sparseMatrix(
  as.numeric(site_f),
  as.numeric(species_f),
  x = 1)

freq <- colSums(s)

# calculate co-occurrence matrix
co <- as(t(s) %*% s, "TsparseMatrix")
co2 <- as(co/freq, "TsparseMatrix")

# store co-occurrences as data frame
counts <- tibble(sp1 = co@i+1,
                 sp2 = co@j+1,
                 co = co@x) %>%
  filter(sp1 != sp2)

species <- tibble(id = 1:nrow(co),
                  species = levels(species_f))

write_csv(counts, "data/lenoir_cooccur_counts.csv")
write_csv(species, "data/lenoir_cooccur_species_list.csv")
