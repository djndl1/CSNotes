#-*- coding: utf-8 -*-

#%%
import tensorflow as tf
from tensorflow import keras
import numpy as np
from tensorflow.keras import layers, models, optimizers, metrics, utils

# %%
def mlpconv2d(x, filters, kernel_size, strides=(1, 1), padding='valid'):
    y = layers.Conv2D(filters[0], kernel_size, strides, padding, activation='relu')(x)
    for filter in filters[1:]:
        y = layers.Conv2D(filter, (1, 1), strides=(1, 1), activation='relu')(y)

    return y

#%%
(x_train, y_train), (x_test, y_test) = keras.datasets.mnist.load_data()

y_train = keras.utils.to_categorical(y_train)
y_test = keras.utils.to_categorical(y_test)

x_train = (x_train.astype(np.float32) - 127) /255.0
x_test = (x_test.astype(np.float32) - 127) /255.0

x_train = x_train[:, :, :, np.newaxis]
x_test = x_test[:, :, :, np.newaxis]
#%%
nin_input = layers.Input(shape=x_train.shape[1:])
inbetween = mlpconv2d(nin_input, filters=[32, 32, 32], kernel_size=(5, 5),
                    padding='same')
inbetween = layers.MaxPooling2D()(inbetween)
inbetween = mlpconv2d(inbetween, filters=[64, 64, 64], 
                    kernel_size=(3, 3), padding='same')
inbetween = layers.MaxPooling2D()(inbetween)
inbetween = mlpconv2d(inbetween, filters=[128, 64, 10],
                    kernel_size=(3, 3), padding='same')
inbetween = layers.GlobalAveragePooling2D()(inbetween)
nin_output = layers.Activation('softmax')(inbetween)
nin_model = keras.Model(inputs=[nin_input], outputs=[nin_output])

# %%
nin_model.summary()

# %%
nin_model.compile(loss='categorical_crossentropy',
                 optimizer=optimizers.RMSprop(lr=0.005),
                 metrics=['acc'])

# %%
hist = nin_model.fit(x_train, y_train, batch_size=120, epochs=50,
            validation_data=[x_test, y_test])

# %%
