# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
from keras.applications import imagenet_utils
# %% Loading Caltech 256
caltech256_train = '/home/djn/Datasets/caltech256/train/'
caltech256_test = '/home/djn/Datasets/caltech256/test/'

def caltech_preprocessing(x):
    return imagenet_utils.preprocess_input(x, mode='tf')

picgen = keras.preprocessing.image.ImageDataGenerator(
    horizontal_flip=True,
    fill_mode='nearest',
    width_shift_range=0.2,
    height_shift_range=0.2,
    preprocessing_function=caltech_preprocessing
)

train_gen = picgen.flow_from_directory(caltech256_train,
                                       target_size=(64, 64),
                                       batch_size=32
                                       )
val_gen = picgen.flow_from_directory(caltech256_test,
                                     target_size=(64, 64),
                                     batch_size=32)
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
        out = keras.layers.Activation(activation)(out)

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
        out = keras.layers.Activation(activation)(out)

        return out

    return convbn

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

    conved = DepthConvBNBlock((3, 3))(conved)
    conved = ConvBNBlock(64, (1, 1))(conved)

    conved = DepthConvBNBlock((3, 3))(conved)
    conved = ConvBNBlock(128, (1, 1))(conved)

    conved = DepthConvBNBlock((3, 3))(conved)
    conved = ConvBNBlock(128, (1, 1))(conved)

    conved = DepthConvBNBlock((3, 3), strides=(2, 2))(conved)
    conved = ConvBNBlock(256, (1, 1))(conved)

    conved = DepthConvBNBlock((3, 3))(conved)
    conved = ConvBNBlock(256, (1, 1))(conved)

    conved = DepthConvBNBlock((3, 3), strides=(2, 2))(conved)
    conved = ConvBNBlock(512, (1, 1))(conved)

    for _ in range(5):
        conved = DepthConvBNBlock((3, 3))(conved)
        conved = ConvBNBlock(512, (1, 1))(conved)


    conved = DepthConvBNBlock((3, 3), strides=(2, 2))(conved)
    conved = ConvBNBlock(1024, (1, 1))(conved)


    conved = DepthConvBNBlock((3, 3))(conved)
    conved = ConvBNBlock(1024, (1, 1))(conved)

    avg = keras.layers.GlobalAveragePooling2D()(conved)

    avg = keras.layers.Dense(n_classes)(avg)
    avg = keras.layers.Dropout(0.5)(avg)
    scores = keras.layers.Activation('softmax')(avg)

    model = keras.Model(inputs=input, outputs=[scores])

    sgd = keras.optimizers.SGD(lr=0.01, momentum=0.9)
    model.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])
    return model


mobnet_name = 'MobileNet'
# %%
import os
if os.path.isfile(mobnet_name + '.h5'):
    mobilenet = keras.models.load_model(mobnet_name + '.h5')
else:
    mobilenet = build_mobilenet(width_multipler=1)


mobilenet.summary()

keras.utils.plot_model(mobilenet, to_file=mobnet_name + '.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint(mobnet_name + '.h5', period=1, save_best_only=True)
lrschedule = keras.callbacks.LearningRateScheduler(lambda epoch, lr: lr * 0.9 if (epoch+1) % 2 == 0 else lr, verbose=1)

callbacks = [chkpt, lrschedule]
hist = mobilenet.fit_generator(multiple_outputs(train_gen), epochs=40, steps_per_epoch=len(train_gen),
validation_data=multiple_outputs(val_gen), validation_steps=len(val_gen), callbacks=callbacks)

import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open(mobnet_name + '_hist.csv', mode='w') as f:
            hist_df.to_csv(f)
