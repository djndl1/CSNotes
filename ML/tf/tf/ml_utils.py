#!/usr/bin/env python3


import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import pandas as pd
from tensorflow import keras

def standardize(x, axis=-1):
    means = np.mean(x, axis, dtype=x.dtype)
    x -= np.expand_dims(means, axis)
    stds = np.atleast_1d(np.std(x, axis))
    stds[stds==0] = 1
    return x / np.expand_dims(stds, axis)


def train_validate_test_split(X, y, validate_size=0.1, test_size=0.2, 
                             shuffle=True, stratify=False):
    from sklearn.model_selection import train_test_split

    strats = y if stratify else None
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, 
                                                        shuffle=shuffle, stratify=strats)
    val_size = validate_size / (1 - test_size)
    strats = y_train if stratify else None
    X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=val_size,
                                                      shuffle=shuffle, stratify=strats)

    return [(X_train, y_train), (X_val, y_val), (X_test, y_test)]


def plot_confusion_matrix(y_true, y_pred, classes, cmap=plt.cm.Blues):    
    """Plot a confusion matrix using ground truth and predictions."""

    import itertools
    from sklearn.metrics import accuracy_score
    from sklearn.metrics import classification_report
    from sklearn.metrics import confusion_matrix

    # Confusion matrix
    cm = confusion_matrix(y_true, y_pred)
    cm_norm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]

    #  Figure
    fig = plt.figure()
    ax = fig.add_subplot(111)
    cax = ax.matshow(cm, cmap=plt.cm.Blues)
    fig.colorbar(cax)

    # Axis
    plt.title("Confusion matrix")
    plt.ylabel("True label")
    plt.xlabel("Predicted label")
    ax.set_xticklabels([''] + classes)
    ax.set_yticklabels([''] + classes)
    ax.xaxis.set_label_position('bottom') 
    ax.xaxis.tick_bottom()

    # Values
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, f"{cm[i, j]:d} ({cm_norm[i, j]*100:.1f}%)",
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    # Display
    plt.show()

def plot_multiclass_decision_boundary(model, X, y, savefig_fp=None):
    """Plot the multiclass decision boundary for a model that accepts 2D inputs.

    Arguments:
        model {function} -- trained model with function model.predict(x_in).
        X {numpy.ndarray} -- 2D inputs with shape (N, 2).
        y {numpy.ndarray} -- 1D outputs with shape (N,).
    """
    import matplotlib.pyplot as plt

    # Axis boundaries
    x_min, x_max = X[:, 0].min() - 0.1, X[:, 0].max() + 0.1
    y_min, y_max = X[:, 1].min() - 0.1, X[:, 1].max() + 0.1

    xx, yy = np.meshgrid(np.linspace(x_min, x_max, 101),
                         np.linspace(y_min, y_max, 101))

    # Create predictions
    x_in = np.c_[xx.ravel(), yy.ravel()]
    y_pred = model.predict(x_in)
    y_pred = np.argmax(y_pred, axis=1).reshape(xx.shape)

    # Plot decision boundary
    plt.contourf(xx, yy, y_pred, cmap=plt.cm.Spectral, alpha=0.8)
    plt.scatter(X[:, 0], X[:, 1], c=y, s=40, cmap=plt.cm.RdYlBu)
    plt.xlim(xx.min(), xx.max())
    plt.ylim(yy.min(), yy.max())

    # Plot
    if savefig_fp:
        plt.savefig(savefig_fp, format='png')


class model:
    def __init__(self, num_input, num_output):
        self.num_input = num_input
        self.num_output = num_output
        
    def __del__(self):
        self.sess.close()

    def build(self, lr=0.1):
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

def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
	"""
	Frame a time series as a supervised learning dataset.
	Arguments:
		data: Sequence of observations as a list or NumPy array.
		n_in: Number of lag observations as input (X).
		n_out: Number of observations as output (y).
		dropnan: Boolean whether or not to drop rows with NaN values.
	Returns:
		Pandas DataFrame of series framed for supervised learning.
	"""


	n_vars = 1 if type(data) is list else data.shape[1]
	df = pd.DataFrame(data)
	cols, names = list(), list()
	# input sequence (t-n, ... t-1)
	for i in range(n_in, 0, -1):
		cols.append(df.shift(i))
		names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
	# forecast sequence (t, t+1, ... t+n)
	for i in range(0, n_out):
		cols.append(df.shift(-i))
		if i == 0:
			names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
		else:
			names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
	# put it all together
	agg = pd.concat(cols, axis=1)
	agg.columns = names
	# drop rows with NaN values
	if dropnan:
		agg.dropna(inplace=True)
	return agg


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