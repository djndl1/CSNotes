# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
from keras.applications import imagenet_utils
#import mxnet as mx
# %% Loading Caltech 256
caltech256_train = '/home/djn/Datasets/caltech256/train'
caltech256_test = '/home/djn/Datasets/caltech256/test'

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
        out = keras.layers.Conv2D(filters, kernel_size, strides=strides, padding=padding, kernel_regularizer=regularizer, bias_regularizer=regularizer)(out)
        if bn:
            out = keras.layers.BatchNormalization(axis=canary)(out)
        out = keras.layers.Activation(activation)(out)

        return out

    return convbn

def FireBlock(s1_filters, e1_filters, e3_filters):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3
    def fire(x):
        squeezed = ConvBNBlock(s1_filters, (1, 1))(x)

        expand_1 = ConvBNBlock(e1_filters, (1, 1))(squeezed)
        expand_3 = ConvBNBlock(e1_filters, (3, 3))(squeezed)

        out = keras.layers.Concatenate(axis=canary)([expand_1, expand_3])

        return out

    return fire


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

def build_squeezenet():
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    input = keras.layers.Input(shape=input_shape)

    conved = ConvBNBlock(96, (3, 3), strides=(2, 2))(input)
    conved = keras.layers.MaxPool2D((3, 3), strides=(2, 2))(conved)

    conved = FireBlock(16, 64, 64)(conved)
    conved = FireBlock(16, 64, 64)(conved)
    conved = keras.layers.MaxPool2D((3, 3), strides=(2, 2))(conved)

    conved = FireBlock(32, 128, 128)(conved)
    conved = FireBlock(32, 128, 128)(conved)
    conved = keras.layers.MaxPool2D((3, 3), strides=(2, 2))(conved)

    conved = FireBlock(48, 192, 192)(conved)
    conved = FireBlock(48, 192, 192)(conved)
    conved = FireBlock(64, 256, 256)(conved)
    conved = FireBlock(64, 256, 256)(conved)

    conved = keras.layers.Dropout(0.5)(conved)
    conved = ConvBNBlock(n_classes, (1, 1))(conved)
    avg = keras.layers.GlobalAveragePooling2D()(conved)
    scores = keras.layers.Activation('softmax')(avg)

    model = keras.Model(inputs=input, outputs=[scores])

    sgd = keras.optimizers.SGD(lr=0.04, momentum=0.9)
    model.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])
    return model


# %%
import os
sqnet_name = 'SqueezeNet'
if os.path.isfile(sqnet_name + '.h5'):
    squeezenet = keras.models.load_model(sqnet_name + '.h5')
#else:
#squeezenet = build_squeezenet()


squeezenet.summary()

keras.utils.plot_model(squeezenet, to_file=sqnet_name + '.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint(sqnet_name + '.h5', period=1, save_best_only=True)
lrschedule = keras.callbacks.LearningRateScheduler(lambda epoch, lr: lr * 0.94 if (epoch+1) % 2 == 0 else lr, verbose=1)

callbacks = [chkpt, lrschedule]
hist = squeezenet.fit_generator(multiple_outputs(train_gen), epochs=100, steps_per_epoch=len(train_gen),
validation_data=multiple_outputs(val_gen), validation_steps=len(val_gen), callbacks=callbacks)

import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open(sqnet_name + '_hist.csv', mode='w') as f:
            hist_df.to_csv(f)
