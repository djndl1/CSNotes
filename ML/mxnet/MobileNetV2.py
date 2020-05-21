# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
from keras.applications import imagenet_utils
import keras.backend as K
import mxnet as mx
# %% Loading Caltech 256
caltech256_data = '/home/djn/Datasets/caltech256/256_ObjectCategories'

def caltech_preprocessing(x, y):
    return imagenet_utils.preprocess_input(x, mode='tf'), y

picgen = keras.preprocessing.image.ImageDataGenerator(
    horizontal_flip=True,
    fill_mode='nearest',
    width_shift_range=0.2,
    height_shift_range=0.2,
    validation_split=0.15,
    preprocessing_function=caltech_preprocessing
)

train_gen = picgen.flow_from_directory(caltech256_data,
                                       target_size=(256, 256),
                                       batch_size=32,
                                       subset='training',
                                        save_to_dir="/home/djn/Datasets/caltech256/train/"
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(256, 256),
                                     batch_size=32,
                                     subset='validation',
                                     save_to_dir="/home/djn/Datasets/caltech256/val/")
# %% See what we have

np_rng = np.random.RandomState(1)
pic = train_gen[0][0][0].astype(np.uint8)

input_shape = pic.shape
n_classes = len(train_gen.class_indices)

# %%
def ConvBNBlock(filters, kernel_size, strides=(1, 1), regularizer=None, padding='same', activation='relu', bn=True):

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def convbn(x):
        out = x
        out = keras.layers.Conv2D(filters, kernel_size, strides=strides, 
                                padding=padding, kernel_regularizer=regularizer, bias_regularizer=regularizer)(out)
        if bn:
            out = keras.layers.BatchNormalization(axis=canary)(out)
        out = keras.layers.ReLU(max_value=6)(out)

        return out

    return convbn

def DepthConvBNBlock(kernel_size, strides=(1, 1), depth_multiplier=1, 
                     regularizer=None, padding='same', activation='relu', bn=True):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def convbn(x):
        out = x
        out = keras.layers.DepthwiseConv2D(kernel_size, strides=strides, padding=padding,
                depth_multiplier=depth_multiplier, kernel_regularizer=regularizer, bias_regularizer=regularizer)(out)
        if bn:
            out = keras.layers.BatchNormalization(axis=canary)(out)
        out = keras.layers.ReLU(max_value=6.0)(out)

        return out

    return convbn

def inverted_bottleneck(filters, strides=1, expansion_factor=6, regularizer=keras.regularizer.l2(4e-5)):
    canary = 1 if keras.backend.image_data_format() == 'channels_first' else 3

    def bottleneck(x):
        input_channels = K.input_shape(x)[canary]
        
        res = ConvBNBlock(input_channels * expansion_factor, (1, 1), regularizer=regularizer)(x)
        res = DepthConvBNBlock((3, 3), strides=strides, padding='same', regularizer=regularizer)(res)
        res = keras.layers.Conv2D(filters, (1, 1), padding='same', 
                                  kernel_regularizer=regularizer, bias_regularizer=regularizer)(res)
        res = keras.layers.BatchNormalization(axis=canary)(res)

        if strides == 1:
            shortcut = x
            res = keras.layers.Add()[res, shortcut]

        return res

    return bottleneck

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
        yield gnext[0], [smoothed]
#%%

def build_mobilenet(width_multipler=1):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    input = keras.layers.Input(shape=input_shape)

    conved = ConvBNBlock(32, (3, 3), strides=(2, 2))(input)

    for _ in range(1):
        conved = inverted_bottleneck(16, expansion_factor=1)(conved)

    for _ in range(2):
        conved = inverted_bottleneck(24)(conved, strides=2)

    for _ in range(3):
        conved = inverted_bottleneck(32, strides=2)(conved)

    for _ in range(4):
        conved = inverted_bottleneck(64)(conved)

    for _ in range(3):
        conved = inverted_bottleneck(96)(conved)

    for _ in range(3):
        conved = inverted_bottleneck(160, strides=2)(conved)

    for _ in range(1):
        conved = inverted_bottleneck(320)(conved)

    conved = ConvBNBlock(1280, (1, 1))(conved)

    avg = keras.layers.GlobalAveragePooling2D()(conved)
    avg = keras.layers.Dense(n_classes)(conved)
    scores = keras.layers.Activation('softmax')(avg)

    model = keras.Model(inputs=input, outputs=[scores])

    return model


# %%
import os
#if os.path.isfile('inceptionv4.h5'):
#    inceptv4 = keras.models.load_model('inceptionv4.h5')
#else:
mobilenet = build_mobilenet(width_multipler=1)

mobnet_name = 'MobileNetV2'

sgd = keras.optimizers.RMSprop(lr=0.045)
mobilenet.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])
mobilenet.summary()

keras.utils.plot_model(mobilenet, to_file=mobnet_name + '.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint(mobnet_name + '.h5', period=1, save_best_only=True)
lrschedule = keras.callbacks.LearningRateScheduler(lambda epoch, lr: lr * 0.98 if (epoch+1) % 2 == 0 else lr, verbose=1)

callbacks = [chkpt, lrschedule]
hist = mobilenet.fit_generator(multiple_outputs(train_gen), epochs=100, steps_per_epoch=len(train_gen),
validation_data=multiple_outputs(val_gen), validation_steps=len(val_gen), callbacks=callbacks)

import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open(mobnet_name + '_hist.csv', mode='w') as f:
            hist_df.to_csv(f)
