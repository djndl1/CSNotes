#-*- coding: utf-8 -*-

# %% 
import tensorflow as tf
from tensorflow import keras
import numpy as np, matplotlib.pyplot as plt
from tensorflow.keras import layers, models, optimizers, losses, metrics, utils
from sklearn import datasets, preprocessing, model_selection, metrics

# %%
(X_train, y_train), (X_test, y_test) = keras.datasets.mnist.load_data()

X_train = (X_train.astype(np.float32) -127) / 255
X_test = (X_test.astype(np.float32) -127) / 255

X_train = X_train.reshape(-1, np.prod(X_train.shape[1:]))
X_test = X_test.reshape(-1, np.prod(X_test.shape[1:]))

y_train = keras.utils.to_categorical(y_train)
y_test = keras.utils.to_categorical(y_test)

X_train, X_val, y_train, y_val = model_selection.train_test_split(X_train, y_train)

# %%
num_input = X_train.shape[1]
num_output = y_train.shape[1]

# %%
class MLPTF:
    def __init__(self, num_input, num_output, num_neurons):
        self.num_input = num_input
        self.num_output = num_output
        self.num_neurons = num_neurons
        self.num_layers = len(num_neurons)

    def __del__(self):
        self.sess.close()

    def build(self, lr=0.1):
        w = []
        b = []
        for i in range(self.num_layers):
            w.append(tf.Variable(tf.truncated_normal([
                self.num_input if i == 0 else self.num_neurons[i-1], self.num_neurons[i]
            ]), dtype=tf.float32, name='w_{0:04d}'.format(i)))

            b.append(tf.Variable(tf.truncated_normal([
                self.num_neurons[i]
            ]), dtype=tf.float32, name='b_{0:04d}'.format(i)))

        w.append(tf.Variable(tf.truncated_normal([self.num_neurons[-1], self.num_output]),
                dtype=tf.float32, name='w_out'))
        b.append(tf.Variable(tf.truncated_normal([self.num_output]), 
                dtype=tf.float32, name='b_out'))

        self.weights = w
        self.biases = b

        self.x = tf.placeholder(dtype=tf.float32, name='x', shape=[None, self.num_input])
        self.t = tf.placeholder(dtype=tf.float32, name='t', shape=[None, self.num_output])

        layer = self.x
        for i in range(self.num_layers):
            layer = tf.nn.relu(tf.matmul(layer, self.weights[i]) + self.biases[i])
        self.out = tf.matmul(layer, self.weights[-1]) + self.biases[-1]
        self.loss = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=self.out, labels=self.t))
        self.optimizer = tf.train.GradientDescentOptimizer(lr).minimize(self.loss)

        self.prediction = tf.argmax(self.out, axis=1)
        self.accuracy = tf.reduce_mean(
            tf.cast(tf.equal(self.prediction, tf.argmax(self.t, axis=1)), tf.float32))

        self.sess = tf.Session()
        self.built = True

    def fit(self, x, y, batch_size, epochs, validation_data=None):
        if not self.built:
            raise Exception("Model not built")

        num_batches = x.shape[0] // batch_size
        self.batch_size = batch_size

        self.sess.run(tf.global_variables_initializer())
        self.losses = []
        self.accuracies = []
        for epoch in range(epochs):
            for batch in range(num_batches):
                train_loss, train_acc, _ = self.sess.run([self.loss, self.accuracy, self.optimizer],feed_dict={
                    self.x: x[batch*batch_size: (batch+1)*batch_size],
                    self.t: y[batch*batch_size: (batch+1)*batch_size]
                })
                self.losses.append(train_loss)
                self.accuracies.append(train_acc)

            if validation_data:
                num_val_batches = validation_data[0].shape[0] // batch_size
                val_x, val_y = validation_data[0], validation_data[1]
                val_accs = []
                val_losses = []
                
                for val_batch in range(num_val_batches):
                    val_loss, val_acc = self.sess.run([self.loss, self.accuracy], feed_dict={
                        self.x: val_x[val_batch*batch_size: (val_batch+1)*batch_size],
                        self.t: val_y[val_batch*batch_size: (val_batch+1)*batch_size]
                    })
                    val_losses.append(val_loss)
                    val_accs.append(val_acc)
                val_loss_mean = np.asarray(val_losses).mean()
                val_acc_mean = np.asarray(val_accs).mean()
                print("Validation Loss {0:.6f}, validation accuracy {1:.6f}".format(val_loss_mean, val_acc_mean))


    def predict(self, x):
        pred = self.sess.run([self.prediction], feed_dict={
            self.x: x
        })

        return pred[0]

    def evaluate(self, x, y):
        acc, loss = self.sess.run([self.accuracy, self.loss], feed_dict={
            self.x: x,
            self.t: y
        })

        return acc, loss

        
        

# %%
mlp = MLPTF(num_input, num_output, [256, 256])
mlp.build(lr=0.01)

# %%
mlp.fit(X_train, y_train, batch_size=60, epochs=100, validation_data=[X_val, y_val])

# %%

pred_test = mlp.predict(X_test)

# %%
mlp.evaluate(X_test, y_test)

# %%
import pandas as pd

# %%
df = pd.read_csv('international-airline-passengers.csv', usecols=[1], header=0)
dataset = df.values
dataset = dataset.astype('float32')
# %%
plt.figure()
plt.plot(dataset)

# %%
train, test = model_selection.train_test_split(dataset, shuffle=False, test_size=0.3)

# %%
import ml_utils
train_data = ml_utils.series_to_supervised(train, 2, 1)
test_data = ml_utils.series_to_supervised(test, 2, 1)
x_train = train_data.iloc[:, :2].values
y_train = train_data.iloc[:, 2].values

x_test = test_data.iloc[:, :2].values
y_test = test_data.iloc[:, 2].values
# %%
ts_model = keras.Sequential()
ts_model.add(layers.Dense(8, activation='relu', input_shape=x_train.shape[1:]))
ts_model.add(layers.Dense(8, activation='relu'))
ts_model.add(layers.Dense(1))

ts_model.summary()

# %%
ts_model.compile(optimizer=optimizers.Adam(lr=0.001),
                 loss='mse')

# %%
ts_model.fit(x_train, y_train, batch_size=20, epochs=100)

# %%
test_pred = ts_model.predict(x_test)
train_pred = ts_model.predict(x_train)
plt.plot(dataset, 'b')
plt.plot(np.arange(len(train_pred) + 2, len(train_pred) + len(test_pred) + 2), test_pred, 'r')
plt.plot(train_pred, 'g')

# %%
all_ts = ml_utils.series_to_supervised(dataset, n_in=2, n_out=1)
all_pred = ts_model.predict(all_ts.iloc[:, :2])
plt.plot(all_pred, 'r')
plt.plot(all_ts.iloc[:, 2], 'b')

# %%
