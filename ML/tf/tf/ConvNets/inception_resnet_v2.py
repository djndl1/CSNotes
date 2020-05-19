# -*- coding: utf-8 -*-

#%% 
import keras
import numpy as np
import keras.backend as K
from keras.applications import imagenet_utils
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
                                       target_size=(72, 72),
                                       batch_size=36,
                                       subset='training'
                                       )

valgen = keras.preprocessing.image.ImageDataGenerator(
    preprocessing_function=caltech_preprocessing
)

val_gen = valgen.flow_from_directory(caltech256_test,
                                     target_size=(72, 72),
                                     batch_size=32)
# %% See what we have

np_rng = np.random.RandomState(1)
pic = train_gen[0][0][0].astype(np.uint8)

input_shape = pic.shape
n_classes = len(train_gen.class_indices)

# %%
def ConvBNBlock(filters, kernel_size, strides=(1, 1), regularizer=None, padding='same', activation='relu'):

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3
    def convbn(x):
        out = x
        out = keras.layers.Conv2D(filters, kernel_size, strides=strides, padding=padding, 
                                  kernel_regularizer=regularizer, bias_regularizer=regularizer)(out)
        out = keras.layers.BatchNormalization(axis=canary)(out)
        out = keras.layers.Activation(activation)(out)

        return out
    return convbn


def Inception_ResNet_A(scale=0.2, regularizer=None):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def inception(x):
        path_1 = ConvBNBlock(32, (1, 1))(x)

        path_2 = ConvBNBlock(32, (1, 1))(x)
        path_2 = ConvBNBlock(32, (3, 3))(path_2)

        path_3 = ConvBNBlock(32, (1, 1))(x)
        path_3 = ConvBNBlock(48, (3, 3))(path_3)
        path_3 = ConvBNBlock(64, (3, 3))(path_3)

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_3])

        return out

    def resnet(x):
        shortcut = x

        x_shape = K.int_shape(shortcut)
        out = inception(x)
        out = ConvBNBlock(x_shape[canary], (1, 1))(out)
        out = keras.layers.Lambda(lambda i: i * scale)(out)

        ret = keras.layers.Add()([shortcut, out])
        ret = keras.layers.Activation('relu')(ret)

        return ret

    return resnet

def Inception_ResNet_B(n=7, scale=0.2, regularizer=None):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def inception(x):
        path_1 = ConvBNBlock(192, (1, 1))(x)

        path_2 = ConvBNBlock(128, (1, 1))(x)
        path_2 = ConvBNBlock(160, (1, n))(path_2)
        path_2 = ConvBNBlock(192, (n, 1))(path_2)

        out = keras.layers.Concatenate(axis=canary)([path_1, path_2])

        return out

    def resnet(x):
        shortcut = x
        x_shape = K.int_shape(shortcut)

        out = inception(x)
        out = ConvBNBlock(x_shape[canary], (1, 1))(out)
        out = keras.layers.Lambda(lambda i: i * scale)(out)

        ret = keras.layers.Add()([shortcut, out])
        ret = keras.layers.Activation('relu')(ret)

        return ret

    return resnet

def Inception_ResNet_C(scale=0.2, regularizer=None):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def inception(x):
        path_1 = ConvBNBlock(192, (1, 1))(x)

        path_2 = ConvBNBlock(192, (1, 1))(x)
        path_2 = ConvBNBlock(224, (1, 3))(path_2)
        path_2 = ConvBNBlock(256, (3, 1))(path_2)

        out = keras.layers.Concatenate(canary)([path_1, path_2])

        return out

    def resnet(x):
        shortcut = x 
        x_shape = K.int_shape(shortcut)

        out = inception(x)
        out = ConvBNBlock(x_shape[canary], (1, 1))(out)
        out = keras.layers.Lambda(lambda i: i * scale)(out)

        ret = keras.layers.Add()([shortcut, out])
        ret = keras.layers.Activation('relu')(ret)

        return ret

    return resnet



def Reduction_A(filters, regularizer=None):
    assert len(filters) == 4

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def inception(x):
        path_1 = ConvBNBlock(filters[3], (3, 3), strides=2, padding='valid')(x)

        path_2 = ConvBNBlock(filters[0], (1, 1))(x)
        path_2 = ConvBNBlock(filters[1], (3, 3))(path_2)
        path_2 = ConvBNBlock(filters[2], (3, 3), strides=2, padding='valid')(path_2)

        path_pool = keras.layers.MaxPooling2D((3, 3), strides=2, padding='valid')(x)

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_pool])

        return out

    return inception

def Reduction_B(filters=[384, 288, 320], reductions=[256, 256, 256], n=7, regularizer=None):
    assert len(filters) == 3 

    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    def inception(x):
        path_1 = ConvBNBlock(reductions[0], (1, 1))(x)
        path_1 = ConvBNBlock(filters[0], (3, 3), strides=2, padding='valid')(path_1)

        path_2 = ConvBNBlock(reductions[1], (1, 1))(x)
        path_2 = ConvBNBlock(filters[1], (3, 3), strides=2, padding='valid')(path_2)

        path_3 = ConvBNBlock(reductions[2], (1, 1))(x)
        path_3 = ConvBNBlock(288, (3, 3))(path_3)
        path_3 = ConvBNBlock(filters[2], (3, 3), strides=2, padding='valid')(path_3)

        path_pool = keras.layers.MaxPooling2D((3, 3), strides=2, padding='valid')(x)

        out = keras.layers.Concatenate(canary)([path_1, path_2, path_3, path_pool])

        return out

    return inception

def inception_resnet_stem(x, n=7, standard=False):
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    if standard:
        out = ConvBNBlock(32, (3, 3), strides=2, padding='valid')(x)
        out = ConvBNBlock(32, (3, 3), padding='valid')(out)
    else:
        out = ConvBNBlock(32, (3, 3))(x)
        out = ConvBNBlock(32, (3, 3))(out)

    out = ConvBNBlock(64, (3, 3))(out)

    r1_1 = keras.layers.MaxPooling2D((3, 3), strides=2, padding='same')(out)
    r1_2 = ConvBNBlock(96, (2, 2), strides=2, padding='valid')(out)
    out = keras.layers.Concatenate(canary)([r1_1, r1_2])

    b1 = ConvBNBlock(64, (1, 1))(out)
    b1 = ConvBNBlock(96, (3, 3), padding='valid')(b1)
    b2 = ConvBNBlock(64, (1, 1))(out)
    b2 = ConvBNBlock(64, (n, 1))(b2)
    b2 = ConvBNBlock(64, (1, n))(b2)
    b2 = ConvBNBlock(96, (3, 3), padding='valid')(b2)
    out = keras.layers.Concatenate(canary)([b1, b2])

    r1_1 = keras.layers.MaxPooling2D((3, 3), strides=2, padding='same')(out)
    r1_2 = ConvBNBlock(192, (2, 2), strides=2, padding='valid')(out)
    out = keras.layers.Concatenate(canary)([r1_1, r1_2])

    return out


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

def build_inception_resnet_v2():
    channel_axis = keras.backend.image_data_format()
    if channel_axis == 'channels_first':
        canary = 1
    else:
        canary = 3

    inpt = keras.layers.Input(shape=input_shape)

    conved = inception_resnet_stem(inpt, n=5)

    y1 = auxiliary_classifier(conved)
    for _ in range(2):
        conved = Inception_ResNet_A()(conved)

    conved = Reduction_A([256, 256, 384, 384])(conved)

    y2 = auxiliary_classifier(conved)
    for _ in range(3):
        conved = Inception_ResNet_B(n=3)(conved)

    #conved = Reduction_B(n=3)(conved)

    #for _ in range(2):
    conved = Inception_ResNet_C()(conved)

    avg = keras.layers.GlobalAveragePooling2D()(conved)
    avg = keras.layers.Dropout(0.2)(avg)
    avg = keras.layers.Dense(n_classes)(avg)
    scores = keras.layers.Activation('softmax')(avg)

    model = keras.Model(inputs=inpt, outputs=[scores, y1, y2])
    sgd = keras.optimizers.SGD(lr=0.045, momentum=0.9)
    model.compile(loss='categorical_crossentropy',
                    optimizer=sgd,
                    metrics=['acc'])
    #model.summary()

    return model

# %%
import os
if os.path.isfile('inception_resnet_v2.h5'):
    incept_res_v2 = keras.models.load_model('inception_resnet_v2.h5')
#else:
#    incept_res_v2 = build_inception_resnet_v2()

incept_res_v2.summary()

keras.utils.plot_model(incept_res_v2, to_file='inception_resnet_v2.png',
        show_shapes=True, show_layer_names=True)

#%%

chkpt = keras.callbacks.ModelCheckpoint('inception_resnet_v2.h5', period=1, save_best_only=True)
lrschedule = keras.callbacks.LearningRateScheduler(lambda epoch, lr: lr * 0.8 if (epoch+1) % 2 == 0 else lr, verbose=1)

callbacks = [chkpt, lrschedule]
hist = incept_res_v2.fit_generator(multiple_outputs(train_gen), epochs=5, steps_per_epoch=len(train_gen),
validation_data=multiple_outputs(val_gen), validation_steps=len(val_gen), callbacks=callbacks)

import pandas as pd

hist_df = pd.DataFrame(hist.history)
with open('inception_resnet_v2_hist.csv', mode='w') as f:
            hist_df.to_csv(f)
