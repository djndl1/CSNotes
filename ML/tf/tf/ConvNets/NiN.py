# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing as skpreprocessing, metrics as skmetrics
from keras.applications import imagenet_utils

# %% Loading Caltech 256
caltech256_data = '/home/djn/Datasets/caltech256/256_ObjectCategories'

def caltech_preprocessing(x):
    return imagenet_utils.preprocess_input(x, mode='tf')

picgen = keras.preprocessing.image.ImageDataGenerator(
    horizontal_flip=True,
    fill_mode='nearest',
    width_shift_range=0.2,
    height_shift_range=0.2,
    validation_split=0.15,
    preprocessing_function=caltech_preprocessing
)

train_gen = picgen.flow_from_directory(caltech256_data,
                                       target_size=(64, 64),
                                       batch_size=32,
                                       subset='training'
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(64, 64),
                                     batch_size=32,
                                     subset='validation')
# %% See what we have
np_rng = np.random.RandomState(1)

k = np_rng.randint(0, 32)
pic = train_gen[0][0][k].astype(np.uint8)

#%%
input_shape = pic.shape
n_classes = len(train_gen.class_indices)
# %% 
def mlpconv2d(x, filters, kernel_size, strides=(1, 1), padding='valid', 
            kernel_regularizers=None, bias_regularizers=None):
    y = keras.layers.Conv2D(filters[0], kernel_size, strides=strides, padding=padding, activation='relu',
                            kernel_regularizer=kernel_regularizers,
                            bias_regularizer=bias_regularizers)(x)
    y = keras.layers.BatchNormalization(axis=1)(y)
    for filter in filters[1:]:
        y = keras.layers.Conv2D(filter, (1, 1), strides=(1, 1), activation='relu', 
                                kernel_regularizer=kernel_regularizers,
                                bias_regularizer=bias_regularizers)(y)
        y = keras.layers.BatchNormalization(axis=1)(y)

    return y

#%%
def build_NiN():


    input = keras.layers.Input(shape=input_shape)

    conved = mlpconv2d(input, [192, 160, 96], (5, 5), 1, padding='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)
    conved = keras.layers.Dropout(0.7)(conved)

    conved = mlpconv2d(conved, [192, 192, 192], (5, 5), 1, padding='same',
                                kernel_regularizers=keras.regularizers.l2(0.0005),
                                bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.AveragePooling2D((5, 5), strides=2)(conved)
    conved = keras.layers.Dropout(0.7)(conved)
    
    conved = mlpconv2d(conved, [192, 192, n_classes], (3, 3), 1, padding='same',
                                kernel_regularizers=keras.regularizers.l2(0.0005),
                                bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)
    
    
    avg = keras.layers.GlobalAveragePooling2D()(conved)
    scores = keras.layers.Activation('softmax')(avg)
    
    NiN = keras.Model(inputs=input, outputs=scores)
    
    NiN.summary()
    
    
    # %%
    sgd = keras.optimizers.Adam(lr=0.002)
    NiN.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])

    return NiN
    
#%%
chkpt = keras.callbacks.ModelCheckpoint('NiN.h5', period=2, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=3)
    
callbacks = [chkpt, lrreduce]
# %%
import os
if os.path.isfile('NiN.h5'):
    NiN = keras.models.load_model('NiN.h5')
else:
    NiN = build_NiN()

hist = NiN.fit_generator(train_gen, epochs=3, validation_data=val_gen, validation_steps=len(val_gen), callbacks=callbacks)

# %%
import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open('NiN_hist.csv', mode='w') as f:
                hist_df.to_csv(f)

