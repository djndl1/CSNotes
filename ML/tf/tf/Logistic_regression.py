#-*- coding: utf-8 -*-

# %%
import tensorflow as tf
from tensorflow import keras
import numpy as np
import matplotlib.pyplot as plt

# %%

(x_train, y_train), (x_test, y_test) = keras.datasets.mnist.load_data()

# %%
num_input = np.prod(x_train[0].shape)
num_output = np.prod(y_train[0])


# %%
x = tf.placeholder(dtype=tf.float32, )