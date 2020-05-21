#-*- coding: utf-8 -*-

#%%
import  tensorflow as tf
import tensorflow
from tensorflow import keras
import pandas as pd, numpy as np, matplotlib.pyplot as plt
from tensorflow.keras import layers, models, utils, optimizers, losses, metrics

# %%
(X_train, y_train), (X_test, y_test) = keras.datasets.mnist.load_data()

# %%
y_train = keras.utils.to_categorical(y_train)
y_test = keras.utils.to_categorical(y_test)

n_classes = y_train.shape[1]
# %%
model = keras.Sequential()
model.add(layers.SimpleRNN(units=16, activation='relu', input_shape=(28, 28)))
model.add(layers.Dense(n_classes, activation='softmax'))

model.compile(loss='categorical_crossentropy',
              optimizer=optimizers.RMSprop(lr=0.01),
              metrics=['acc'])
model.summary()              

# %%
hist = model.fit(X_train, y_train, batch_size=100, epochs=20)

# %%
score = model.evaluate(X_test, y_test)

# %%
