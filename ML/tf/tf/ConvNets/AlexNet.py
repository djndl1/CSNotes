# -*- coding: utf-8 -*-

#%% 
from tensorflow import keras
import tensorflow as tf
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing as skpreprocessing, metrics as skmetrics

# %% Loading Caltech 256
caltech256_data = '/home/djn/Datasets/caltech256/256_ObjectCategories'

picgen = keras.preprocessing.image.ImageDataGenerator(
    horizontal_flip=True,
    fill_mode='nearest',
    width_shift_range=0.2,
    height_shift_range=0.2,
    validation_split=0.2
)

train_gen = picgen.flow_from_directory(caltech256_data,
                                       target_size=(227, 227),
                                       batch_size=32,
                                       subset='training'
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(227, 227),
                                     batch_size=32,
                                     subset='validation')
# %% See what we have
label2text = dict()
for k, v in train_gen.class_indices.items():
    label2text[v] = k

plt.figure(figsize=(10, 20))
np_rng = np.random.RandomState(1)

for i in range(32):
        plt.subplot(8, 4, i+1)
        plt.xticks([])
        plt.yticks([])
        k = np_rng.randint(0, 32)
        pic = train_gen[0][0][k].astype(np.uint8)
        plt.imshow(pic)
        titulus = label2text[train_gen[0][1][k].argmax()]
        plt.title(titulus)

plt.show()

#%%
input_shape = pic.shape
n_classes = len(train_gen.class_indices)
# %% Local Response Normalization is replaced by Batch Normalization
alexnet = keras.Sequential()
alexnet.add(keras.layers.Conv2D(96, (11, 11), strides=4, 
                                input_shape=input_shape, activation='relu'))
alexnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))
alexnet.add(keras.layers.BatchNormalization())

alexnet.add(keras.layers.Conv2D(256, (5, 5), strides=4, activation='relu', padding='same'))
alexnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))
alexnet.add(keras.layers.BatchNormalization())

alexnet.add(keras.layers.Conv2D(384, (3, 3), strides=1, activation='relu', padding='same'))
alexnet.add(keras.layers.Conv2D(384, (3, 3), strides=1, activation='relu', padding='same'))
alexnet.add(keras.layers.Conv2D(256, (3, 3), strides=1, activation='relu', padding='same'))
alexnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))

alexnet.add(keras.layers.Flatten())
alexnet.add(keras.layers.Dense(4096, activation='relu'))
alexnet.add(keras.layers.Dropout(0.5))
alexnet.add(keras.layers.Dense(4096, activation='relu'))
alexnet.add(keras.layers.Dropout(0.5))
alexnet.add(keras.layers.Dense(n_classes, activation='softmax'))

alexnet.summary()


# %%
chkpt = keras.callbacks.ModelCheckpoint('alexnet.ckp', period=5, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau()
tfbord = keras.callbacks.TensorBoard()

callbacks = [chkpt, lrreduce, tfbord]
sgd = keras.optimizers.SGD(lr=0.01, momentum=0.9)
alexnet.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])

# %%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

#config.gpu_options.per_process_gpu_memory_fraction=0.9
set_session(tf.Session(config=config))

alexnet.fit(x=train_gen, epochs=50, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
