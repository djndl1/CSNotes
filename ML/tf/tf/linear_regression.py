# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
import tensorflow as tf
from tensorflow import keras
import numpy as np
import sklearn
import sklearn.datasets as skds
import sklearn.model_selection as skms
import sklearn.preprocessing as skpp


# %%
boston = skds.load_boston()


# %%
X, y = boston.data.astype(np.float32), boston.target.astype(np.float32)


# %%
if y.ndim == 1:
    y = y.reshape(y.shape[0], -1)


# %%
X_train, X_test, y_train, y_test = skms.train_test_split(X, y, test_size=0.2)


# %%
x_scaler = skpp.StandardScaler()
X_train = x_scaler.fit_transform(X_train)
X_test = x_scaler.transform(X_test)


# %%
X_train[0]


# %%
num_outputs = y_train.shape[1]
num_inputs = X_train.shape[1]


# %%
x_tensor = tf.placeholder(dtype=tf.float32, shape=[None, num_inputs], name='x')
y_tensor = tf.placeholder(dtype=tf.float32, shape=[None, num_outputs], name='y')


# %%
w = tf.Variable(tf.zeros([num_inputs, num_outputs]), dtype=tf.float32, name='w')
b = tf.Variable(tf.zeros([num_outputs]), dtype=tf.float32, name='b')


# %%
model = tf.matmul(x_tensor, w) + b


# %%
loss = tf.reduce_mean(tf.square(model - y_tensor))
mse = tf.reduce_mean(tf.square(model - y_tensor))
y_mean = tf.reduce_mean(y_tensor)


# %%
total_error = tf.reduce_sum(tf.square(y_tensor - y_mean))
unexplained_error = tf.reduce_sum(tf.square(y_tensor - model))
rs = 1 - tf.div(unexplained_error, total_error)


# %%
lr = 1e-4
optimizer = tf.train.GradientDescentOptimizer(lr).minimize(loss)


# %%
num_epochs = 1500
loss_epochs = []
mse_epochs = []
rs_epochs = []


# %%
mse_score = 0
rs_score = 0


# %%
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())

    for epoch in range(num_epochs):
        feed_dict = {x_tensor: X_train, y_tensor: y_train}
        loss_val, _ = sess.run([loss, optimizer], feed_dict=feed_dict)
        loss_epochs.append(loss_val)

        feed_dict = {x_tensor: X_test, y_tensor: y_test}
        mse_score, rs_score = sess.run([mse, rs], feed_dict=feed_dict)
        mse_epochs.append(mse_score)
        rs_epochs.append(rs_score)

    print("For test data: MSE = {0:.8f}, R2 = {1:.8f}".format(mse_score, rs_score))


# %%


