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

def residual_block(filters, kernels, strides, regularizer, cross=False):
    def resblock(x):
        res_out = x

        res_out = keras.layers.Conv2D(filters[0], kernels[0], strides, padding='same',
                                          kernel_regularizer=regularizer, bias_regularizer=regularizer)(x)
        res_out = keras.layers.BatchNormalization()(res_out)
        res_out = keras.layers.Activation('relu')(res_out)

        res_out = keras.layers.Conv2D(filters[1], kernels[1], strides=(1, 1), padding='same',
                                          kernel_regularizer=regularizer, bias_regularizer=regularizer)(res_out)
        res_out = keras.layers.BatchNormalization()(res_out)
        res_out = keras.layers.Activation('relu')(res_out)

        if cross:
            shortcut = keras.layers.Conv2D(filters[2], (1, 1), strides=strides, padding='same',
                                      kernel_regularizer=regularizer, bias_regularizer=regularizer)(x)
            shortcut = keras.layers.BatchNormalization()(shortcut)
        else:
            shortcut = x

        out = keras.layers.Add()([res_out, shortcut])
        out = keras.layers.Activation('relu')(out)

        return out

    return resblock

#%%

input = keras.layers.Input(shape=input_shape)

conv = keras.layers.Conv2D(64, (7, 7), strides=(2, 2), padding='same')(input)
conv = keras.layers.BatchNormalization()(conv)
conv = keras.layers.Activation('relu')(conv)
conv = keras.layers.MaxPool2D((3, 3), strides=2, padding='same')(conv)

conv = residual_block([64, 64], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([64, 64], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

conv = residual_block([128, 128, 128], [(3, 3), (3, 3)], (2, 2), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([128, 128], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([128, 128], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([128, 128], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

conv = residual_block([256, 256, 256], [(3, 3), (3, 3)], (2, 2), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

conv = residual_block([512, 512, 512], [(3, 3), (3, 3)], (2, 2), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([512, 512], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
conv = residual_block([512, 512], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

flat = keras.layers.GlobalAveragePooling2D()(conv)
scores = keras.layers.Dense(n_classes, activation='softmax',
                            kernel_regularizer=keras.regularizers.l2(0.0001),
                            bias_regularizer=keras.regularizers.l2(0.0001))(flat)

ResNet34 = keras.Model(inputs=input, outputs=scores)

ResNet34.summary()


# %%
chkpt = keras.callbacks.ModelCheckpoint('ResNet34.h5', period=2, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=5)
tfbord = keras.callbacks.TensorBoard(log_dir='ResNet34_logs')

callbacks = [chkpt, lrreduce, tfbord]
sgd = keras.optimizers.SGD(lr=0.1, momentum=0.9)
ResNet34.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])

#%%
import os
if os.path.isfile('./ResNet34.h5'):
    ResNet34 = keras.models.load_model('ResNet34.h5')
# %%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

config.gpu_options.per_process_gpu_memory_fraction=0.8
set_session(tf.Session(config=config))

ResNet34.fit(x=train_gen, epochs=70, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
