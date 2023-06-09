# Install TensorFlow (skip if already installed) --------------------------

install.packages("keras")

## Configure R with a Python installation it can use ----

# If you don't already have Python 3 installed, you can install it using the
# reticulate package...
library(reticulate)
path_to_python <- install_python()   # install Python and save path to interpreter

#... or download and install it directly from https://www.python.org/downloads/

# Create a virtual environment for running Tensorflow
virtualenv_create("r-reticulate", python = path_to_python)

# If you installed Python directly (not using reticulate), replace "path_to_python"
# with the path to your Python interpreter.
# To find the path in Windows:
   # 1) Type ‘Python’ in the Windows Search Bar
   # 2) Right-click on the Python App, and then select “Open file location“
   # 3) Right-click on the Python shortcut, and then select Properties
   # 4) Copy the path found in the "Target" box
# To find the path on a Mac, open Python in the Applications folder and copy
# the path found in the"Interpreter" box

## Install TensorFlow
library(tensorflow)
install_tensorflow(envname = "r-reticulate", version = "2.12")

# Restart R and confirm that installation succeeded
reticulate::use_virtualenv("r-reticulate")
library(tensorflow)
tf$constant("Hello Tensorflow!")


# Prepare co-occurrence data ----------------------------------------------
library(tidyverse)
library(Matrix)

## Read in species occurrences from USA VegBank dataset (subset of sPlotOpen) ----
d <- readRDS("demo/USA_VegBank.rds")


## Convert to site-by-species matrix ----

nsp <- n_distinct(d$Species)

site_f <- factor(d$PlotObservationID)  # convert site to factor
species_f <- factor(d$Species)         # convert species to factor

# create site-by-species matrix
s <- sparseMatrix(
  as.numeric(site_f),
  as.numeric(species_f),
  x = 1)


## Calculate co-occurrences ----

# convert site-by-species matrix to species co-occurrence matrix
co <- as(t(s) %*% s, "TsparseMatrix")

# store co-occurrences as data frame
counts <- tibble(sp1 = co@i,
                 sp2 = co@j,
                 co = co@x) %>%
  filter(sp1 != sp2)

# species list
species <- tibble(id = 1:nrow(co),
                  species = levels(species_f))



# Train GloVe model -------------------------------------------------------

library(tensorflow)
library(keras)

set_random_seed(seed = 123, disable_gpu = TRUE)


## Set hyperparameters ----

dim <- 8         # dimension of species vectors
xmax <- 100      # weighting function threshold
alpha <- 0.75    # weighting function power
lr <- 0.001      # learning rate


## Define GloVe loss function ----
glove_loss <- function(y_true, y_pred) {
  K <- backend()
  y_true <- tf$cast(y_true, tf$float32)
  diff <- tf$pow(tf$subtract(y_pred, tf$math$log(y_true)), 2)  #squared difference
  weight <- tf$where(y_true >= xmax, tf$pow(y_true, 0), tf$pow(y_true/xmax, alpha))
  loss <- K$sum(tf$multiply(weight, diff))
  return(loss)
}


## Specify model ----

# inputs (species ID's)
sp1_input <- layer_input(shape = c(1), dtype = 'int32', name = 'sp1_input')
sp2_input = layer_input(shape = c(1), dtype = 'int32', name = 'sp2_input')

# embedding layers (species vectors)
sp_vec1 <- layer_embedding(input_dim = nsp, output_dim = dim, name = "species_vectors1", input_length = 1)
sp_vec2 <- layer_embedding(input_dim = nsp, output_dim = dim, name = "species_vectors2", input_length = 1)

# bias terms
bias1 <- layer_embedding(input_dim = nsp, output_dim = 1, name = "bias1", input_length = 1)
bias2 <- layer_embedding(input_dim = nsp, output_dim = 1, name = "bias2", input_length = 1)

# calculate species vectors
sp1_vec <- sp_vec1(sp1_input)
sp2_vec <- sp_vec2(sp2_input)

# calculate bias terms
sp1_bias <- bias1(sp1_input)
sp2_bias <- bias2(sp2_input)

# take dot product of vectors and add bias
dp <- layer_dot(inputs = list(sp1_vec, sp2_vec), axes = 2, name='output')
output <- layer_add(inputs = list(dp, sp1_bias, sp2_bias))

# define model inputs and output
model <- keras_model(inputs = list(sp1_input, sp2_input), outputs = output)

# print model summary
model


## Compile model ----
model %>% compile(optimizer = optimizer_adam(learning_rate = lr), loss = glove_loss)


## Fit model ----

# fit model
model %>%
  fit(list(counts$sp1, counts$sp2), counts$co,
      epochs = 100,
      batch_size = 256,
      validation_split = 0.1,
      callbacks = callback_early_stopping(monitor = "val_loss",
                                          patience = 3,
                                          mode = 'min',
                                          restore_best_weights = T),
      verbose = 2)


## Extract species vectors

vec1 <- get_weights(model$layers[[3]])[[1]]    # first set of vectors
vec2 <- get_weights(model$layers[[4]])[[1]]    # second set of vectors

vectors <- (vec1 + vec2)/2                     # take elementwise average

colnames(vectors) <- paste0("d", 1:dim)        # add dimension names

out <- bind_cols(species, vectors)             # attach species names



# Explore nearest neighbors in vector space ------------------------------

# calculate distance matrix
dists <- out %>% select(-id, -species) %>% dist() %>% as.matrix()

sp <- "Abies lasiocarpa"    # pick focal species

# sort species list by distance to focal species in vector space
out %>% mutate(dist = dists[,which(species == sp)]) %>%
  arrange(dist) %>%
  select(id, species, dist, everything())



