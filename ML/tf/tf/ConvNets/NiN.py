# -*- coding: utf-8 -*-

#%% 
from tensorflow import keras
import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing as skpreprocessing, metrics as skmetrics
from tensorflow.keras.applications import imagenet_utils

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
                                       target_size=(96, 96),
                                       batch_size=32,
                                       subset='training'
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(96, 96),
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
    y = keras.layers.Conv2D(filters[0], kernel_size, strides, padding, activation='relu',
                            kernel_regularizer=kernel_regularizers,
                            bias_regularizer=bias_regularizers)(x)
    for filter in filters[1:]:
        y = keras.layers.Conv2D(filter, (1, 1), strides=(1, 1), activation='relu', 
                                kernel_regularizer=kernel_regularizers,
                                bias_regularizer=bias_regularizers)(y)

    return y

#%%
def build_NiN():


    input = keras.layers.Input(shape=input_shape)

    conved = mlpconv2d(input, [64, 64, 64], (5, 5), 1, padding='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)
    conved = keras.layers.Dropout(0.5)(conved)

    conved = mlpconv2d(conved, [128, 128, 128], (3, 3), 1, padding='same',
                                kernel_regularizers=keras.regularizers.l2(0.0005),
                                bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)
    conved = keras.layers.Dropout(0.5)(conved)
    
    conved = mlpconv2d(conved, [256, 256, n_classes], (3, 3), 1, padding='same',
                                kernel_regularizers=keras.regularizers.l2(0.0005),
                                bias_regularizers=keras.regularizers.l2(0.0005))
    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)
    
    
    avg = keras.layers.GlobalAveragePooling2D()(conved)
    scores = keras.layers.Activation('softmax')(avg)
    
    NiN = keras.Model(inputs=input, outputs=scores)
    
    NiN.summary()
    
    
    # %%
    sgd = keras.optimizers.SGD(lr=0.01, momentum=0.9)
    NiN.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])

    return NiN
    
#%%
chkpt = keras.callbacks.ModelCheckpoint('NiN.h5', period=5, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=5)
tfbord = keras.callbacks.TensorBoard(log_dir='NiN_logs')
    
callbacks = [chkpt, lrreduce, tfbord]
# %%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

config.gpu_options.per_process_gpu_memory_fraction=0.7
set_session(tf.Session(config=config))

import os
if os.path.isfile('NiN.h5'):
    NiN = keras.models.load_model('NiN.h5')
else:
    NiN = build_NiN()

NiN.fit(x=train_gen, epochs=30, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
