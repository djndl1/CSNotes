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
    validation_split=0.15
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
zfnet = keras.Sequential()
zfnet.add(keras.layers.Conv2D(96, (7, 7), strides=2,
                                kernel_regularizer=keras.regularizers.l2(0.0005),
                                bias_regularizer=keras.regularizers.l2(0.0005),
                                input_shape=input_shape, activation='relu'))
zfnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))
zfnet.add(keras.layers.BatchNormalization())

zfnet.add(keras.layers.Conv2D(256, (5, 5), 
                                kernel_regularizer=keras.regularizers.l2(0.0005),
                                bias_regularizer=keras.regularizers.l2(0.0005),
                                strides=4, activation='relu', padding='same'))
zfnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))
zfnet.add(keras.layers.BatchNormalization())

zfnet.add(keras.layers.Conv2D(384, (3, 3), strides=1, 
                                kernel_regularizer=keras.regularizers.l2(0.0005),
                                bias_regularizer=keras.regularizers.l2(0.0005),
                                activation='relu', padding='same'))
zfnet.add(keras.layers.Conv2D(384, (3, 3), 
                                kernel_regularizer=keras.regularizers.l2(0.0005),
                                bias_regularizer=keras.regularizers.l2(0.0005),
                                strides=1, activation='relu', padding='same'))
zfnet.add(keras.layers.Conv2D(256, (3, 3), 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            strides=1, activation='relu', padding='same'))
zfnet.add(keras.layers.MaxPooling2D((3, 3), strides=2))

zfnet.add(keras.layers.Flatten())
zfnet.add(keras.layers.Dense(4096, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='relu'))
zfnet.add(keras.layers.Dropout(0.5))
zfnet.add(keras.layers.Dense(4096, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='relu'))
zfnet.add(keras.layers.Dropout(0.5))
zfnet.add(keras.layers.Dense(n_classes, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='softmax'))

zfnet.summary()


# %%
chkpt = keras.callbacks.ModelCheckpoint('zfnet.h5', period=5, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau()
tfbord = keras.callbacks.TensorBoard(log_dir='zfnet_logs')

callbacks = [chkpt, lrreduce, tfbord]
sgd = keras.optimizers.SGD(lr=0.01, momentum=0.9)
zfnet.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])

# %%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

#config.gpu_options.per_process_gpu_memory_fraction=0.9
set_session(tf.Session(config=config))

zfnet.fit(x=train_gen, epochs=80, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
