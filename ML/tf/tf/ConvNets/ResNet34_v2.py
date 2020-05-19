# -*- coding: utf-8 -*-

#%%
import keras
import numpy as np
import matplotlib.pyplot as plt
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

def residual_block_v2(filters, kernels, strides, regularizer, cross=False):

    canary = 3 if keras.backend.image_data_format() == 'channels_last' else 1
    def resblock(x):
        preact = x
        preact = keras.layers.BatchNormalization(axis=canary)(preact)
        preact = keras.layers.Activation('relu')(preact)

        res_out = keras.layers.Conv2D(filters[0], kernels[0], strides=strides, padding='same',
                                          kernel_regularizer=regularizer, bias_regularizer=regularizer)(preact)

        res_out = keras.layers.BatchNormalization(axis=canary)(res_out)
        res_out = keras.layers.Activation('relu')(res_out)
        res_out = keras.layers.Conv2D(filters[1], kernels[1], strides=(1, 1), padding='same',
                                          kernel_regularizer=regularizer, bias_regularizer=regularizer)(res_out)

        if cross:
            shortcut = keras.layers.Conv2D(filters[1], (1, 1), strides=strides, padding='same')(preact)
            shortcut = keras.layers.BatchNormalization(axis=canary)(shortcut)
        else:
            shortcut = x

        out = keras.layers.Add()([res_out, shortcut])

        return out

    return resblock

#%%

def build_resnet_v2():
    canary = 3 if keras.backend.image_data_format() == 'channels_last' else 1

    input = keras.layers.Input(shape=input_shape)

    conv = keras.layers.Conv2D(64, (7, 7), strides=(2, 2), padding='same')(input)

    conv = residual_block_v2([64, 64], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)
    conv = residual_block_v2([64, 64], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

    conv = residual_block_v2([128, 128], [(3, 3), (3, 3)], (2, 2), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
    for _ in range(2):
        conv = residual_block_v2([128, 128], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

    conv = residual_block_v2([256, 256], [(3, 3), (3, 3)], (2, 2), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
    for _ in range(3):
        conv = residual_block_v2([256, 256], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

    conv = residual_block_v2([512, 512], [(3, 3), (3, 3)], (1, 1), cross=True,
                      regularizer=keras.regularizers.l2(0.0001))(conv)
    for _ in range(1):
        conv = residual_block_v2([512, 512], [(3, 3), (3, 3)], (1, 1),
                      regularizer=keras.regularizers.l2(0.0001))(conv)

    conv = keras.layers.BatchNormalization(axis=canary)(conv)
    conv = keras.layers.Activation('relu')(conv)

    flat = keras.layers.GlobalAveragePooling2D()(conv)
    scores = keras.layers.Dense(n_classes, activation='softmax',
                            kernel_regularizer=keras.regularizers.l2(0.0001),
                            bias_regularizer=keras.regularizers.l2(0.0001))(flat)

    ResNet34 = keras.Model(inputs=input, outputs=scores)

    sgd = keras.optimizers.SGD(lr=0.1, momentum=0.9)
    ResNet34.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])
    return ResNet34


resv2_name = 'ResNet32_v2'
# %%

#%%
import os
if os.path.isfile(resv2_name + '.h5'):
    ResNet34_v2 = keras.models.load_model(resv2_name + '.h5')
#else:
#    ResNet34_v2 = build_resnet_v2()
# %%
ResNet34_v2.summary()
# %%
chkpt = keras.callbacks.ModelCheckpoint(resv2_name + '.h5', period=1, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=3)

callbacks = [chkpt, lrreduce]

hist = ResNet34_v2.fit_generator(train_gen, epochs=20, validation_data=val_gen, validation_steps=len(val_gen), callbacks=callbacks)

# %%
import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open(resv2_name + '.csv', mode='w') as f:
            hist_df.to_csv(f)

