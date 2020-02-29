C. Szegedy et al., “Going Deeper with Convolutions,” Sep. 2014.

[GoogLeNet (Inception v1) Review](https://medium.com/coinmonks/paper-review-of-googlenet-inception-v1-winner-of-ilsvlc-2014-image-classification-c2b3565a64e7)

[Understandign Inception Network From Scratch](https://www.analyticsvidhya.com/blog/2018/10/understanding-inception-network-from-scratch/)

# Background

The recent trend in CNN has been to increase the number of layers and layer size while using dropout to address the problem of overfitting. Max-pooling has been successfully employed. Network-in-Network is an approach to increase to the representational power of neural networks and can be easily integrated into the current CNN pipelines. The current leading approach for object detection is R-CNN.

# Ideas

The straightforward way of increasing the depth and the width of neural networks is prone to overfitting with limited data. Another drawback is the increased use of computational resources.

The fundamental way of solving both issues is to move from fully connected to sparsely connected architectures even inside the convolutions. Today's computing infrastructures are very inefficient when it comes to numerical calculation on non-uniform sparse data structures.

The main idea of the Inception architecture is based on finding out how an optimal local sparse structure in a convolutional vision network can be approximated and covered by readily available dense components. All we need is to find the optimal local construction and to repeat it spatially. Each unit form the earlier layer corresponds to some region of the input image and these units are grouped into filter banks. ??? TODO

Judiciously applying dimension reductions and projections wherever the computational requirements would increase too much otherwise.???

For technical reasons, it seemed beneficial to start using Inception modules only at higher layers while keeping the lower layers in traditional convolutional fashion. ??

# Architecture

## GooLeNet

| type     | patch size/stride | output size                 | depth   | $1\times 1$ | $3 \times 3$ reduce | $3 \times 3$ | $5 \times 5$ reduce | $5 \times 5$ | pool proj | params  | ops     |
| :------: | :------:          | :------:                    | :-----: | :-----:     | :-----:             | :-----:      | :-----:             | :-----:      | :-----:   | :-----: | :-----: |
| conv     | $7 \times 7 / 2$  | $112 \times 112 \times 64 $ | $1 $    |             |                     |              |                     |              |           | $2.7K$  | $34M$   |
| max pool | $3 \times 3 / 2$  | $56 \times 56\times 64$     | 0       |             |                     |              |                     |              |           |         |         |
| conv     | $3 \times 3 / 1$  | $56 \times 56 \times 192$   | $2$     |             | $64$                | $192$        |                     |              |           | $112K$  | $360M$  |
