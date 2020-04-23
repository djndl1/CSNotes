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
                                       target_size=(128, 128),
                                       batch_size=32,
                                       subset='training'
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(128, 128),
                                     batch_size=32,
                                     subset='validation')
# %% See what we have

np_rng = np.random.RandomState(1)
pic = train_gen[0][0][0].astype(np.uint8)

input_shape = pic.shape
n_classes = len(train_gen.class_indices)

# %%

def wide_residual_block(filters, kernels, strides, regularizer, width_factor=2, cross=False):
    def resblock(x):
        res_out = x

        res_out = keras.layers.BatchNormalization()(x)
        res_out = keras.layers.Activation('relu')(res_out)
        res_out = keras.layers.Conv2D(filters[0] * width_factor, kernels[0], strides, padding='same',
                                      kernel_regularizer=regularizer, bias_regularizer=regularizer)(res_out)

        res_out = keras.layers.Dropout(0.4)(res_out)

        res_out = keras.layers.BatchNormalization()(res_out)
        res_out = keras.layers.Activation('relu')(res_out)
        res_out = keras.layers.Conv2D(filters[1] * width_factor, kernels[1], strides=(1, 1), padding='same',
                                      kernel_regularizer=regularizer, bias_regularizer=regularizer)(res_out)

        if cross:
            shortcut = keras.layers.BatchNormalization()(x)
            shortcut = keras.layers.Conv2D(filters[1] * width_factor, (1, 1), strides=strides, padding='same',
                                           kernel_regularizer=regularizer, bias_regularizer=regularizer)(shortcut)
        else:
            shortcut = x

        out = keras.layers.Add()([res_out, shortcut])
        out = keras.layers.Activation('relu')(out)
        out = keras.layers.Dropout(0.4)(out)

        return out

    return resblock

#%%

def build_WRN():

    input = keras.layers.Input(shape=input_shape)
    conv = keras.layers.Conv2D(16, (3, 3), strides=(1, 1), padding='same')(input)
    conv = keras.layers.BatchNormalization()(conv)

    conv = wide_residual_block([16, 16], [(3, 3), (3, 3)], (1, 1), width_factor=6, cross=True,
                          regularizer=keras.regularizers.l2(0.0001))(conv)
    conv = wide_residual_block([16, 16], [(3, 3), (3, 3)], (1, 1), width_factor=6,
                          regularizer=keras.regularizers.l2(0.0001))(conv)

    conv = wide_residual_block([32, 32], [(3, 3), (3, 3)], (2, 2), cross=True, width_factor=6,
                          regularizer=keras.regularizers.l2(0.0001))(conv)
    conv = wide_residual_block([32, 32], [(3, 3), (3, 3)], (1, 1), width_factor=6,
                          regularizer=keras.regularizers.l2(0.0001))(conv)


    conv = wide_residual_block([64, 64], [(3, 3), (3, 3)], (2, 2), cross=True, width_factor=6,
                          regularizer=keras.regularizers.l2(0.0001))(conv)
    conv = wide_residual_block([64, 64], [(3, 3), (3, 3)], (1, 1), width_factor=6,
                          regularizer=keras.regularizers.l2(0.0001))(conv)

    flat = keras.layers.GlobalAveragePooling2D()(conv)
    scores = keras.layers.Dense(n_classes, activation='softmax',
                                kernel_regularizer=keras.regularizers.l2(0.0001),
                                bias_regularizer=keras.regularizers.l2(0.0001))(flat)

    WRN = keras.Model(inputs=input, outputs=scores)

    WRN.summary()

    
    sgd = keras.optimizers.SGD(lr=0.1, momentum=0.9, nesterov=True)
    WRN.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])

    return WRN
#%%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

config.gpu_options.per_process_gpu_memory_fraction=0.85
set_session(tf.Session(config=config))

import os
if os.path.isfile('./WRN.h5'):
    WRN = keras.models.load_model('WRN.h5')
else:
    WRN = build_WRN()

chkpt = keras.callbacks.ModelCheckpoint('WRN.h5', period=2, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=5)
tfbord = keras.callbacks.TensorBoard(log_dir='WRN_logs')

callbacks = [chkpt, lrreduce, tfbord]

WRN.fit(x=train_gen, epochs=70, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
