# -*- coding: utf-8 -*-

#%% 
from tensorflow import keras
import numpy as np
from keras.applications import imagenet_utils
import tensorflow as tf
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
pic = train_gen[0][0][0].astype(np.uint8)

input_shape = pic.shape
n_classes = len(train_gen.class_indices)

# %% 
def BN_Inception_block(filters, reductions, strides=(1, 1), regularizer=None):
    assert len(filters) == len(reductions)
    assert len(filters) == 3

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3
    def inception(x):
        path_1 = keras.layers.Conv2D(filters[0], (1, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_1 = keras.layers.BatchNormalization(axis=canary)(path_1)
        path_1 = keras.layers.Activation('relu')(path_1)

        path_2 = keras.layers.Conv2D(reductions[0], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)                                                                          
        path_2 = keras.layers.Conv2D(filters[1], (3, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)                                     

        path_3 = keras.layers.Conv2D(reductions[1], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)                                                                          
        path_3 = keras.layers.Conv2D(filters[2], (3, 3), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)                                                                                                               
        path_3 = keras.layers.Conv2D(filters[2], (3, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)

        path_4 = keras.layers.MaxPool2D((3, 3), strides=strides, padding='same')(x)
        path_4 = keras.layers.Conv2D(reductions[2], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_4)
        path_4 = keras.layers.BatchNormalization(axis=canary)(path_4)
        path_4 = keras.layers.Activation('relu')(path_4)                                     

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_3, path_4])

        return out

    return inception


#%%

def build_inception():
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    input = keras.layers.Input(shape=input_shape)

    conved = keras.layers.Conv2D(64, (7, 7), strides=(2, 2), padding='same', activation='relu')(input)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)
    conved = keras.layers.MaxPool2D((3, 3), strides=2, padding='same')(conved)

    conved = keras.layers.Conv2D(64, (3, 3), strides=(1, 1), padding='same', activation='relu')(conved)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)
    conved = keras.layers.MaxPool2D((3, 3), strides=2, padding='same')(conved)

    conved = BN_Inception_block([64, 64, 96], [64, 64, 32])(conved)
    conved = BN_Inception_block([64, 96, 96], [64, 64, 64])(conved)

    conved = BN_Inception_block([224, 96, 128], [64, 96, 128])(conved)
    conved = BN_Inception_block([192, 128, 128], [96, 96, 128])(conved)
    conved = BN_Inception_block([160, 160, 160], [128, 128, 128])(conved)
    conved = BN_Inception_block([96, 192, 192], [128, 160, 128])(conved)
    conved = BN_Inception_block([256, 192, 192], [128, 192, 128], strides=(2, 2))(conved)

    conved = BN_Inception_block([352, 320, 224], [192, 160, 128])(conved)
    conved = BN_Inception_block([352, 320, 224], [192, 192, 128])(conved)
    avg = keras.layers.GlobalAveragePooling2D()(conved)

    #avg = keras.layers.Dropout(0.4)(avg)
    avg = keras.layers.Dense(n_classes)(avg)

    scores = keras.layers.Activation('softmax')(avg)

    bn_incept = keras.Model(inputs=input, outputs=scores)

    sgd = keras.optimizers.SGD(lr=0.1, momentum=0.9)
    bn_incept.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])

    return bn_incept


# %%
config = tf.ConfigProto()
config.gpu_options.allow_growth = True
config.gpu_options.per_process_gpu_memory_fraction=0.8

keras.backend.set_session(tf.Session(config=config))

import os
if os.path.isfile('BN_inception.h5'):
    bnincept = keras.models.load_model('BN_inception.h5')
else:
    bnincept = build_inception()

bnincept.summary()

keras.utils.plot_model(bnincept, to_file='BN_inception.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint('BN_inception.h5', period=1, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau(patience=3)
tfbord = keras.callbacks.TensorBoard(log_dir='BN_inception_logs')

callbacks = [chkpt, lrreduce, tfbord]
hist = bnincept.fit_generator(train_gen, epochs=40, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

import pickle
with open('bn_hist', 'wb') as f:
    pickle.dump(hist, f)

# %%
