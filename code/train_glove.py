from tensorflow.keras.layers import Input, Embedding, Dot, Add
from tensorflow.keras.models import Model
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.optimizers import Adam
from tensorflow.python.ops import math_ops
import numpy as np
import tensorflow.keras.backend as K
import tensorflow as tf
import pandas as pd



def train_glove(d, dim, xmax, epochs=100, alpha=0.75, lr = 0.001):
    """
    train_glove trains the GloVe algorithm on species co-occurrence data 

    :param d: dataframe of species co-occurrence counts in long form
                -1st column ('sp1'), species ID's for species 1 (must be consecutive integers starting at 1)
                -2rd column ('sp2'), species ID's for species 2 (must be consecutive integers starting at 1)
                -3rd column ('co'), co-occurrence counts for species 1 and 2 (must be >0) 
    :param dim: number of dimensions
    :param xmax: weighting function threshold 
    :param epochs: number of training epochs
    :param alpha: weighting function power
    :param lr: learning rate
    """

    # convert species ID's to zero indexing
    d['sp1'] = d['sp1'] - 1
    d['sp2'] = d['sp2'] - 1

    # get number of species
    n_species = d.sp1.value_counts().shape[0]


    # function to calculate glove loss
    def glove_loss(y_true, y_pred):
        y_true = tf.cast(y_true, tf.float32)
        diff = math_ops.squared_difference(y_pred, tf.math.log(y_true))  #squared difference
        weight = tf.where(y_true >= xmax, tf.pow(y_true, 0), tf.pow(y_true/xmax, alpha))
        loss = K.sum(tf.multiply(weight, diff))
        return loss

    # specify model
    dim = int(dim)

    # inputs (species ID's)
    sp1_input = Input(shape=(1,), dtype='int32', name='sp1_input')
    sp2_input = Input(shape=(1,), dtype='int32', name='sp2_input')

    # initiate embedding layers (species vectors)
    embedding1 = Embedding(input_dim=n_species, output_dim=dim, name="species_embedding1", input_length=1)
    embedding2 = Embedding(input_dim=n_species, output_dim=dim, name="species_embedding2", input_length=1)

    # initiate bias terms
    bias1 = Embedding(input_dim=n_species, output_dim=1, name="bias1", input_length=1)
    bias2 = Embedding(input_dim=n_species, output_dim=1, name="bias2", input_length=1)

    # calculate species vectors
    sp1_emb = embedding1(sp1_input)
    sp2_emb = embedding2(sp2_input)

    # calculate bias terms
    sp1_bias = bias1(sp1_input)
    sp2_bias = bias2(sp2_input)

    # take dot product of vectors and add bias
    dp = Dot(axes=2, name='output')([sp1_emb, sp2_emb])
    output = Add()([dp, sp1_bias, sp2_bias])

    # define model inputs and output
    m1 = Model(inputs=[sp1_input, sp2_input], outputs=[output])
    
    # print model summary
    m1.summary()

    # compile
    m1.compile(optimizer=Adam(learning_rate=lr), loss=glove_loss)

    # fit model
    h1 = m1.fit([d.sp1.to_numpy(), d.sp2.to_numpy()], [d.co.to_numpy()],
                epochs=epochs, batch_size=256, validation_split=0.1,
                callbacks=[EarlyStopping(monitor='val_loss', patience=3, mode='min', restore_best_weights=True)],
                verbose=2)

    # print minimum validation loss
    np.min(h1.history['val_loss'])


    # save embeddings
    emb1 = m1.layers[2].get_weights()[0]
    emb2 = m1.layers[3].get_weights()[0]
    bias1 = m1.layers[5].get_weights()[0]
    bias2 = m1.layers[6].get_weights()[0]
    emb = pd.DataFrame(np.concatenate((bias1, bias2, emb1, emb2), axis=1))

    out = {"model": m1, "history": pd.DataFrame(h1.history), "embedding": emb}
    return out
