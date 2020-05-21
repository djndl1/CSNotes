A. Krizhevsky, I. Sutskever, and G. E. Hinton, “ImageNet Classification with Deep Convolutional Neural Networks.” .

https://towardsdatascience.com/deciding-optimal-filter-size-for-cnns-d6f7b56f9363

https://www.sicara.ai/blog/2019-10-31-convolutional-layer-convolution-kernel

https://engmrk.com/alexnet-implementation-using-keras/

# Background

CNNs have attractive qualities: much fewer connections and parameters, easier to train, great performance. However, they are expensive to apply in large scale to high-resolution images.

# Architecture

- Eight layers: five Conv. layers and three FC layers

- input: $224\times 224 \times 3$ images

- output: $1000$-way softmax FC layer

- objective: maximizing the multinomial logistic regression 

- optimizer: SGD, batch size $128$, momentum $0.9$, weight decay $0.0005$

$$
v_{i+1}=0.9v_{i}-0.0005\epsilon w_{i}-\epsilon\left\langle \frac{\partial L}{\partial w}|_{w_{i}}\right\rangle _{D_{i}}
$$

where $\left\langle \frac{\partial L}{\partial w}|_{w_{i}}\right\rangle _{D_{i}}$ is the average over the $i$th batch $D_i$ of the derivative of the objective w.r.t $w$, evaluated at $w_i$.

$$
w_{i+1}=w_{i}+v_{i+1}
$$

|Input $224 \times 224 \times 3$ |
:------------:
| Conv $11\times11 \times3$, $96$ kernels, stride $4$, pad $0$ |
| MaxPooling, overlapping with size $3$ and stride $2$     |
| LRN                                                      |
| Conv $5\times 5 \times 48$, $256$ kernels, stride $1$, pad $2$ |
| MaxPooling                                               |
| LRN                                                      |
| Conv $3\times 3 \times 256$, $384$ kernels, stride $1$, pad $1$ |
| Conv $3\times 3 \times 192$, $384$ kernels, stride $1$, pad $1$ |
| Conv $3\times 3 \times 192$, $256$ kernels, stride $1$, pad $1$ |
| MaxPooling                                               |
| FC-4096                                                  |
| FC-4096                                                  |
| FC-1000, softmax                                         |

The second, fourth and fifth conv. layers have strange kernel channel numbers because they are computed across two GPUs. the kernels of the third conv layer are connected to all kernel maps in the second layer. The neurons in FC layers are connected to all neurons in the previous layer.

## Pooling

Pooling layers in CNNs summarize the outputs of neighboring groups of neurons in the same kernel map. Overlapping pooling is used, that is, a grid of pooling units spaced $s$ pixels apart, each summarizing a neighborhood of size $z \times z$ centered at the location of the pooling unit, where $s < z$ (Here $s = 2, z = 3$).

## ReLU (Rectified Linear Units) Nonlinearity

$$
f(x) = \max(0, x)
$$

Deep convolutional neural networks with ReLUs train several times faster than their equivalents with `tanh` units. Before AlexNet, $\tanh$ was used.

## Multiple GPUs

Using 2 GPUs is due to memory problem, NOT for speeding up the training process, since layers are distributed among GPUs.

## Local Response Normalization

Normalization helps to speed up convergence.

ReLUs have the desirable property that they do not require input normalization to prevent them from saturating. if at least some training examples produce a positive input to a ReLU, learning will happen in that neuron.

$$
b_{x,y}^{i}=a_{x,y}^{i}/\left(k+\alpha\sum_{j=\max\left(0,i-n/2\right)}^{\min\left(N-1,i+n/2\right)}\left(a_{x,y}^{j}\right)^{2}\right)^{\beta}
$$

- $a_{x,y}^i$: the activity of a neuron computed by applying kernel $i$ at position $(x,y)$ and then applying the ReLU nonlinearity.

- $N$: the total number of kernels

- $k,n,\alpha, \beta$: hyperparameters

The sum runs over $n$ adjacent kernel maps at the same spatial position.

# Reducing Overfitting

## Data Augmentation

1. Generating images translations and horizontal reflections dan extracting random $224 \times 224$ patches from the $256 \times 256$. At test time, the network makes a prediction by extracting five $224 \times 224$ patches (four corner patches and the center patch) as well as their horizontal reflections and averaging the predictions made by the network's softmax layer on the ten patches.

2. Altering the intensities of the RGB channels in training images. Performing PCA on the set of RGB pixel values throughout the training set. TODO

## Dropout

Setting to zero the output of each hidden neuron with a certain probability ($0.5$). The neurons which are dropped out in this way do not contribute to the forward and do not participate in backpropagation. So every time an input is presented, the neural network samples a different architecture, but all these architecture share weights. This technique reduces complex co-adaptations of neurons, since a neuron cannot rely on the presence of particular other neurons. It is therefore forced to learn more robust features that are useful in conjunction with many different random subsets of the other neurons.

At test time, all neurons are used but multiply their outputs by $0.5$.

# Training

Each layer is initialized from a zero-mean Gaussian distribution with standard deviation $0.01$. All neuron biases in the second, fourth, and fifth conv layers as well as in the hidden FC layers are are $1$. The biases of the remaining layers are $0$.

Learning rates are equal for all layers. The learning rate is divided by $10$ when the validation error stopped improving with the current learning rate. The learning rate was initialized at $0.01$ and reduced three times prior to termination.

The whole training took five to six days on two Nvidia GTX 580 3GB GPUs.

# Implementation

```python
import keras
from keras import Sequential
from keras import models
from keras.layers import Conv2D
from keras.layers import MaxPool2D
from keras.layers import Dropout
from keras.optimizers import SGD
from keras.layers import Dense
from keras.layers import Flatten

input_shape = (227, 227, 3)

alexnet_model = Sequential([
    Conv2D(96, (11,11), strides=4, input_shape=input_shape, activation='relu'),
    MaxPool2D((3,3), 2),
    Conv2D(256, (5, 5),  activation='relu', padding='same'),
    MaxPool2D((3,3), 2),
    Conv2D(384, (3, 3),  activation='relu', padding='same'),
    Conv2D(384, (3, 3),  activation='relu', padding='same'),
    Conv2D(256, (3, 3),  activation='relu', padding='same'),
    MaxPool2D((3,3), 2),
    Flatten(),
    Dense(4096, activation='relu'),
    Dropout(0.5),
    Dense(4096, activation='relu'),
    Dropout(0.5),
    Dense(1000, activation='softmax')
])

alexnet_model.summary()
alexnet_model.compile(optimizer='SGD', loss=keras.losses.categorical_crossentropy, metrics=['accuracy'])
```
