# Learning vector representations of species from co-occurrence data

This repository contains data and code to reproduce the analysis presented in: 

Siefert, A., Laughlin, D.C., Sabatini, F.M. 2023. You shall know a species by the company it keeps: leveraging co-occurrence data to improve ecological prediction. doi: https://doi.org/10.1101/2023.02.15.528518

In this paper, we propose using ordination to encode the information contained in large species co-occurrence datasets in low-dimensional vectors that can be used to represent species in ecological prediction. We present an efficient method to derive species vectors from co-occurrence data using GloVe, an unsupervised learning algorithm originally designed for language modeling.

To demonstrate the method, we used GloVe to generate vectors for nearly 40,000 plant species using co-occurrence statistics derived from sPlotOpen, an environmentally-balanced global dataset of vegetation plots, and we tested the ability of the vectors to predict elevational range shifts in European montane plant species.

We implemented GloVe using TensorFlow in Python. The wrapper function for training GloVe and extracting species vectors is found in `code/train_glove.py`. We also provide a demonstration of fitting GloVe to a smaller dataset in R: `demo/glove_demo.R`.

Pre-trained vectors (16 dimensions plus bias terms) for 39,905 plant species are available in `vectors/species_vectors_16dim_w_bias.csv`. We encourage you to try these out in your own ecological prediction tasks!
