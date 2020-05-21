[K. Simonyan and A. Zisserman, “Very Deep Convolutional Networks for Large-Scale Image Recognition,” Sep. 2014.](https://www.robots.ox.ac.uk/~vgg/research/very_deep/)

Also see [VGG16 Implementation Using Keras](https://engmrk.com/vgg16-implementation-using-keras/)

- [VGGNet-Review](https://medium.com/coinmonks/paper-review-of-vggnet-1st-runner-up-of-ilsvlc-2014-image-classification-d02355543a11)

http://qingkaikong.blogspot.com/2017/12/what-is-ablation-study-in-machine.html

https://stats.stackexchange.com/questions/380040/what-is-an-ablation-study-and-is-there-a-systematic-way-to-perform-it

https://hackernoon.com/learning-keras-by-implementing-vgg16-from-scratch-d036733f2d5

https://neurohive.io/en/popular-networks/vgg16/

https://github.com/pytorch/vision/blob/master/torchvision/models/vgg.py

https://github.com/machrisaa/tensorflow-vgg

This paper addresses the issue of depth. It increases the depth of the network by adding more convolutional layer, which is feasible due to the use of _very small_ ($3 \times 3$) convolution filters in all layers. It was demonstrated that the representation depth is beneficial for the classfication accuracy.

The major drawbacks are being slow to train and its large weights.

# ConvNet Configuration

## Generic Architecture

- Input: $224 \times 224$ RGB image, subtracted by the mean RGB value computed on the training set from each pixel.

- Convolution stride: $1$; receptive field $3 \times 3$ or $1 \times 1$; Spatial padding: same

- Five MaxPooling over $2 \times 2$ window with stride $2$ following some of the conv layers.

- All hidden layers use ReLU.

- No Local Response Normalization (LRN) since it does not increase performance.

A stack of convolutional layers followed by three fully-connected (FC) layers. The final layer is softmax layer.

A stack of three $3 \times 3$ has an effective receptive field of $5 \times 5$ (of two, $7 \times 7$). We can see the weight parameters are much less: $3 \times (3^2 C^2)$ versus $7^2 C^2$ (assuming both the input and the output has $C$ channels).

Increased depth with small-size filters led to better performance.

A 16 weight-layer example, 130+ million parameters


| input (224, 224) RGB image |
:-----------:
| conv3-64  |
| conv3-64  |
| maxpool   |
| conv3-128 |
| conv3-128 |
| maxpool   |
| conv3-256 |
| conv3-256 |
| conv3-256 |
| maxpool   |
| conv3-512 |
| conv3-512 |
| conv3-512 |
| maxpool   |
| conv3-512 |
| conv3-512 |
| conv3-512 |
| maxpool   |
| FC-4096   |
| FC-4096   |
| FC-1000   |
| softmax   |

# Training and Testing

- Four Nvidia Titan Black GPUs, _2-3 weeks_ a single net;

## Training

- batch size: $256$, momentum: $0.9$;

- regularized by weight decay ($L_2$ penalty multiplier set to $5 \cdot 10^{-4}$);

- dropout regularization for the first two FC layers (dropout ratio $0.5$);

- Learning rate set to $10^{-2}$ initially, and then decreased by a factor of $10$ when the validation set accuracy stopped improving.

- weight initialization is done by first pretraining a smaller model and stracking more layers on top of it to obtain a deeper model.

- Images are cropped into $224 \times 224$ pixels from rescaled training images. The crops underwent random horizontal flipping and random RGB color shift.

- The proper image size can be found by (1) training a model with a smaller input size and training a model with a larger input size, initialized with the smaller one or (2) multi-scale training([Multiscale Methods and Machine Learning](https://www.kdnuggets.com/2018/03/multiscale-methods-machine-learning.html)) by random sampling $S$ from a certain range $[S_{min}, S_{max}]$ so that a single model is trained to recognize objects over a wide range of scales.

## Testing

An image is rescaled to a predefined smallest image side $Q$, which is not necessarily equal to the triaining scale $S$. Fully connected layers are converted by conv layers (exactly how?).

| Convolutionalized |
:----------:
| Conv7-4096 |
| Conv1-4096 |
| Conv1-1000 |
| Softmax    |

The result is a class score map with the number of channels equal to the number of classes, the class score map is spatially averaged (sum-pooled). Horizontal flipping of the images are and the original are averaged to obtain the final scores for the image.

# Evaluation

## Single-scale Evaluation

Local response normalization (LRN) does not improve than without any normalization layers. The classfication error decreases with the increased ConvNet depth. Scale jittering at training time leads to significantly better results than training on images with fixed smallest value. A deep net with small filters outperforms a shallow net with larger filters.

## Multi-scale Evaluation

A model runs over several rescaled versions of a test image, followed by averaging the resulting class posteriors. The multiscale versions perform better than their single-scale versions.

## Multi-crop Evaluation

Since the fully-convolutonal network is applied over the whole image, there is no need to sample multiple crops at test time. However, suing a large set of crops, can lead to improved accuracy. Using multiple crops performs slightly better than dense (fully-convolutional) evaluation and they can be complementary.

## ConvNet Fusion

Combining the outputs of several models by averaging their softmax class posteriors.

# keras implementation example

```python
import keras
from keras.models import Sequential
from keras.layers import Conv2D
from keras.layers import MaxPooling2D
from keras.layers import Dense, Activation, Dropout, Flatten

input_shape = (224, 224, 3)
vgg_16_model = Sequential([
     Conv2D(64, (3, 3), input_shape=input_shape, padding='same', activation='relu'),
     Conv2D(64, (3, 3), padding='same', activation='relu'),
     MaxPooling2D(pool_size=(2, 2), strides=2, padding='same'),
     Conv2D(128, (3, 3), padding='same', activation='relu'),
     Conv2D(128, (3, 3), padding='same', activation='relu'),
     MaxPooling2D(pool_size=(2, 2), strides=2, padding='same'),
     Conv2D(256, (3, 3), padding='same', activation='relu'),
     Conv2D(256, (3, 3), padding='same', activation='relu'),
     MaxPooling2D(pool_size=(2, 2), strides=2, padding='same'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     MaxPooling2D(pool_size=(2, 2), strides=2, padding='same'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     Conv2D(512, (3, 3), padding='same', activation='relu'),
     MaxPooling2D(pool_size=(2, 2), strides=2, padding='same'),
     Flatten(),
     Dense(4096, activation='relu'),
     Dropout(rate=0.5)
     Dense(4096, activation='relu'),
     Dropout(rate=0.5)
     Dense(1000, activation='softmax')
])
vgg_16_model.summary()

from keras.optimizers import SGD

sgd_optimizer = SGD(decay=5e-4, momentum=0.9, lr=0.01)
vgg_16_model.compile(loss=keras.losses.categorical_crossentropy, optimizer=sgd_optimizer, metrics=['accuracy'])
```

# [Localization](https://www.kaggle.com/c/imagenet-object-localization-challenge)

- https://medium.com/machine-learning-bites/deeplearning-series-objection-detection-and-localization-yolo-algorithm-r-cnn-71d4dfd07d5f

A special case of object detection, where a single object bouding box should be predicted.

The last fully connected layer predicts the bounding box location, represented by a $4-D$ vector storing its center coordinates, width, and height.

The main difference in training is that the logistic regression objective is replaced with a Euclidean loss, which penalizes the deviation of the predicted bounding box parameters from the ground-truth. Training was initialized with the corresponding classification models. The last fully-connected layer was initialized randomly and trained from scratch.

For testing, one protocol is comparing different network modifications on the validation set and considers only the bounding box prediction for the ground truth class by applying the net only to thecentral crop of the image . The other is based on the dense application of the localization ConvNet to the whole image: spatially close predictions are merged, and rated based on the class scores, obtained from the classification ConvNet (greedy merging). See [OverFeat](https://arxiv.org/abs/1312.6229)

Per-class regression (PCR) outperforms the class-agnostic single-class regression (SCR). Testing at several scales (multiscale testing) and combining the predictions of multiple networks (model fusion) improves the performance.

# Generalization of Very Deep Features

Deep image representation learnt on ILSVRC generalize well on other datasets. The last FC layer is removed, and the penultimate layer is used as image features, which are _aggregated_ (???) across multiple locations and scales. The resulting image descriptor is $L_{2}-normalized$ and combined with a linear SVM classifier trained on the target dataset. $Q$ goes through the network and prodices a $4096-D$ image descriptor. The descriptor is then averaged with the descriptor of a horizontal flipped image.

On VOC, aggregating image descriptors, computed by multiple scales by averaging performs similarly to the aggregation by stacking. On Caltech datasets, the stacking of descriptors, computed over multiple scales, performs better than averaging or max-pooling, and stacking allows a classifier to exploit such scale-specific representations.
