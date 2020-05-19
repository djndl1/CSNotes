# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
from keras.applications import imagenet_utils
import mxnet as mx
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
def Normal_Inception_block(filters, reductions, strides=(1, 1), regularizer=None):
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

        path_4 = keras.layers.AveragePooling2D((3, 3), strides=strides, padding='same')(x)
        path_4 = keras.layers.Conv2D(reductions[2], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_4)
        path_4 = keras.layers.BatchNormalization(axis=canary)(path_4)
        path_4 = keras.layers.Activation('relu')(path_4)

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_3, path_4])

        return out

    return inception

def Strided_Inception_block(filters, reductions, strides=(2, 2), regularizer=None):
    assert len(filters) == 2

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3
    def inception(x):

        path_2 = keras.layers.Conv2D(filters[0], (3, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)

        path_3 = keras.layers.Conv2D(reductions[0], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)
        path_3 = keras.layers.Conv2D(filters[1], (3, 3), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)
        path_3 = keras.layers.Conv2D(filters[1], (3, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)

        path_4 = keras.layers.MaxPool2D((3, 3), strides=strides, padding='same')(x)

        out = keras.layers.Concatenate(canary)([path_2, path_3, path_4])

        return out

    return inception

def Factorized_Inception_block(filters, reductions, n=5, strides=1, regularizer=None):
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

        path_2 = keras.layers.Conv2D(reductions[1], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2 = keras.layers.Conv2D(filters[2], (1, n), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2 = keras.layers.Conv2D(filters[2], (n, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2 = keras.layers.Conv2D(filters[2], (1, n), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2 = keras.layers.Conv2D(filters[2], (n, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)

        path_3 = keras.layers.Conv2D(reductions[0], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)
        path_3 = keras.layers.Conv2D(filters[1], (1, n), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)
        path_3 = keras.layers.Conv2D(filters[1], (n, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)

        path_4 = keras.layers.AveragePooling2D((3, 3), strides=strides, padding='same')(x)
        path_4 = keras.layers.Conv2D(reductions[2], (1, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_4)
        path_4 = keras.layers.BatchNormalization(axis=canary)(path_4)
        path_4 = keras.layers.Activation('relu')(path_4)

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_3, path_4])

        return out

    return inception


def Expanded_Inception_block(filters, reductions, strides=1, regularizer=None):
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

        path_2 = keras.layers.Conv2D(reductions[1], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2 = keras.layers.Conv2D(filters[2], (3, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2 = keras.layers.BatchNormalization(axis=canary)(path_2)
        path_2 = keras.layers.Activation('relu')(path_2)
        path_2_1 = keras.layers.Conv2D(filters[2], (3, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2_1 = keras.layers.BatchNormalization(axis=canary)(path_2_1)
        path_2_1 = keras.layers.Activation('relu')(path_2_1)
        path_2_2 = keras.layers.Conv2D(filters[2], (1, 3), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_2)
        path_2_2 = keras.layers.BatchNormalization(axis=canary)(path_2_2)
        path_2_2 = keras.layers.Activation('relu')(path_2_2)

        path_3 = keras.layers.Conv2D(reductions[0], (1, 1), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(x)
        path_3 = keras.layers.BatchNormalization(axis=canary)(path_3)
        path_3 = keras.layers.Activation('relu')(path_3)
        path_3_1 = keras.layers.Conv2D(filters[1], (1, 3), padding='same',
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3_1 = keras.layers.BatchNormalization(axis=canary)(path_3_1)
        path_3_1 = keras.layers.Activation('relu')(path_3_1)
        path_3_2 = keras.layers.Conv2D(filters[1], (3, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_3)
        path_3_2 = keras.layers.BatchNormalization(axis=canary)(path_3_2)
        path_3_2 = keras.layers.Activation('relu')(path_3_2)

        path_4 = keras.layers.AveragePooling2D((3, 3), strides=strides, padding='same')(x)
        path_4 = keras.layers.Conv2D(reductions[2], (1, 1), padding='same', strides=strides,
                                     kernel_regularizer=regularizer,
                                     bias_regularizer=regularizer)(path_4)
        path_4 = keras.layers.BatchNormalization(axis=canary)(path_4)
        path_4 = keras.layers.Activation('relu')(path_4)

        out = keras.layers.Concatenate(canary)([path_1, path_2_1, path_2_2, path_3_1, path_3_2, path_4])

        return out

    return inception

def auxiliary_classifier(x):
    out = x
    out = keras.layers.AveragePooling2D((5, 5), strides=3, padding='same')(out)
    out = keras.layers.Conv2D(128, (1, 1), padding='same')(out)
    out = keras.layers.BatchNormalization(axis=1)(out)
    out = keras.layers.Activation('relu')(out)
    out = keras.layers.Flatten()(out)
    out = keras.layers.Dense(512)(out)
#    out = keras.layers.BatchNormalization(axis=1)(out)
    out = keras.layers.Activation('relu')(out)
    out = keras.layers.Dense(n_classes, activation='softmax')(out)
    return out
#%%

def uniform_label_smoothing(onehot_label, weight):
    smoothed = onehot_label
    n_classes = smoothed.shape[1]
    smoothed *= 1.0 - weight
    smoothed += 1.0 / n_classes * weight

    return smoothed

def multiple_outputs(generator):
    while True:
        gnext = generator.next()
        smoothed = uniform_label_smoothing(gnext[1], 0.1)
        yield gnext[0], [smoothed, smoothed, smoothed]
#%%

def build_inception():
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    input = keras.layers.Input(shape=input_shape)

    conved = keras.layers.Conv2D(32, (3, 3), strides=(2, 2))(input)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)
    conved = keras.layers.Conv2D(32, (3, 3), strides=(1, 1),)(conved)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)
    conved = keras.layers.Conv2D(64, (3, 3), strides=(1, 1), padding='same')(conved)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)

    conved = keras.layers.MaxPool2D((3, 3), strides=2)(conved)

    conved = keras.layers.Conv2D(80, (3, 3), strides=(1, 1))(conved)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)
    conved = keras.layers.Conv2D(192, (3, 3), strides=(2, 2))(conved)
    conved = keras.layers.BatchNormalization(axis=canary)(conved)
    conved = keras.layers.Activation('relu')(conved)

    conved = Normal_Inception_block([64, 64, 96], [48, 64, 32])(conved)
    conved = Normal_Inception_block([64, 64, 96], [48, 64, 64])(conved)
    conved = Normal_Inception_block([64, 64, 96], [48, 64, 64])(conved)


    y1 = auxiliary_classifier(conved)

    conved = Factorized_Inception_block([192, 192, 192], [128, 128, 192])(conved)
    conved = Factorized_Inception_block([192, 192, 192], [160, 160, 192])(conved)
    conved = Factorized_Inception_block([192, 192, 192], [160, 160, 192])(conved)
#    conved = Factorized_Inception_block([192, 192, 192], [192, 192, 192])(conved)
    #conved = Strided_Inception_block([384, 96], [64])(conved)

    y2 = auxiliary_classifier(conved)

    conved = Expanded_Inception_block([320, 384, 384], [384, 448, 192])(conved)
    avg = keras.layers.GlobalAveragePooling2D()(conved)

    avg = keras.layers.Dense(n_classes)(avg)

    scores = keras.layers.Activation('softmax')(avg)

    model = keras.Model(inputs=input, outputs=[scores, y1, y2])

    sgd = keras.optimizers.SGD(lr=0.0075, momentum=0.9)
    model.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])

    return model


# %%
import os
if os.path.isfile('inceptionv3.h5'):
    inceptv3 = keras.models.load_model('inceptionv3.h5')
#else:
#inceptv3 = build_inception()

inceptv3.summary()

keras.utils.plot_model(inceptv3, to_file='inceptionv3.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint('inceptionv3.h5', period=1, save_best_only=True)
lrschedule = keras.callbacks.LearningRateScheduler(lambda epoch, lr: lr * 0.94 if (epoch+1) % 2 == 0 else lr, verbose=1)

callbacks = [chkpt, lrschedule]
hist = inceptv3.fit_generator(multiple_outputs(train_gen), epochs=30, steps_per_epoch=len(train_gen),
validation_data=multiple_outputs(val_gen), validation_steps=len(val_gen), callbacks=callbacks)

import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open('inception_v3_hist.csv', mode='w') as f:
            hist_df.to_csv(f)
