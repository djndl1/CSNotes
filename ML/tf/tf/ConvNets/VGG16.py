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
                                       target_size=(224, 224),
                                       batch_size=32,
                                       subset='training'
                                       )
val_gen = picgen.flow_from_directory(caltech256_data,
                                     target_size=(224, 224),
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
# %% 
def generic_conv_block(x, filters, kernels, strides, activations='relu', paddings='valid',
                        kernel_regularizers=None, bias_regularizers=None):
    depth = len(filters) if isinstance(filters, tuple) or isinstance(filters, list) else 1
    if len(kernels) != depth:
        raise Exception("Wrong kernel list!")
    if isinstance(strides, int):
        strides = [strides]
        strides = strides * depth
    if len(strides) != depth:
        raise Exception("Wrong stride list!")
    if isinstance(activations, str):
        activations = [activations]
        activations *= depth
    if len(activations) != depth:
        raise Exception("Wrong activation list")
    if isinstance(paddings, str):
        paddings = [paddings]
        paddings *= depth
    if len(paddings) != depth:
        raise Exception("Wrong padding list")
    if not (isinstance(kernel_regularizers, list) and isinstance(kernel_regularizers, tuple)):
        kernel_regularizers = [kernel_regularizers]
        kernel_regularizers *= depth
    if not (isinstance(bias_regularizers, list) and isinstance(bias_regularizers, tuple)):
        bias_regularizers = [bias_regularizers]
        bias_regularizers *= depth

    out = x
    for i in range(depth):
        out = keras.layers.Conv2D(filters[i], kernels[i], padding=paddings[i],
                                strides=strides[i], activation=activations[i],
                                kernel_regularizer=kernel_regularizers[i],
                                bias_regularizer=bias_regularizers[i])(out)

    return out

#%%

input = keras.layers.Input(shape=input_shape)

conved = generic_conv_block(input, [64, 64], [(3, 3), (3, 3)], 1, paddings='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))

conved = keras.layers.MaxPool2D((2, 2), strides=2)(conved)

conved = generic_conv_block(conved, [128, 128], [(3, 3), (3, 3)], 1, paddings='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))

conved = keras.layers.MaxPool2D((2, 2), strides=2)(conved)

conved = generic_conv_block(conved, [256, 256, 256], [(3, 3), (3, 3), (3, 3)], 1, paddings='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))

conved = keras.layers.MaxPool2D((2, 2), strides=2)(conved)

conved = generic_conv_block(conved, [512, 512, 512], [(3, 3), (3, 3), (3, 3)], 1, paddings='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))                            

conved = keras.layers.MaxPool2D((2, 2), strides=2)(conved)

conved = generic_conv_block(conved, [512, 512, 512], [(3, 3), (3, 3), (3, 3)], 1, paddings='same',
                            kernel_regularizers=keras.regularizers.l2(0.0005),
                            bias_regularizers=keras.regularizers.l2(0.0005))                            

conved = keras.layers.MaxPool2D((2, 2), strides=2)(conved)                            

flattend = keras.layers.Flatten()(conved)
fc1 = keras.layers.Dense(4096, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='relu')(flattend)
fc1 = keras.layers.Dropout(0.5)(fc1)
fc2 = keras.layers.Dense(4096, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='relu')(fc1)
fc2 = keras.layers.Dropout(0.5)(fc2)
scores = keras.layers.Dense(n_classes, 
                            kernel_regularizer=keras.regularizers.l2(0.0005),
                            bias_regularizer=keras.regularizers.l2(0.0005),
                            activation='softmax')(fc2)

vgg16 = keras.Model(inputs=input, outputs=scores)

vgg16.summary()


# %%
chkpt = keras.callbacks.ModelCheckpoint('vggnet16.h5', period=5, save_best_only=True)
lrreduce = keras.callbacks.ReduceLROnPlateau()
tfbord = keras.callbacks.TensorBoard(log_dir='vggnet_logs')

callbacks = [chkpt, lrreduce, tfbord]
sgd = keras.optimizers.SGD(lr=0.01, momentum=0.9)
vgg16.compile(loss='categorical_crossentropy',
                optimizer=sgd,
                metrics=['acc'])

# %%
from keras.backend.tensorflow_backend import set_session
config = tf.ConfigProto()
config.gpu_options.allow_growth = True

#config.gpu_options.per_process_gpu_memory_fraction=0.9
set_session(tf.Session(config=config))

vgg16.fit(x=train_gen, epochs=70, validation_data=val_gen, validation_steps=1, callbacks=callbacks)

# %%
