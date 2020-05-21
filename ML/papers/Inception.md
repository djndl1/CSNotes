C. Szegedy et al., “Going Deeper with Convolutions,” Sep. 2014.

[GoogLeNet (Inception v1) Review](https://medium.com/coinmonks/paper-review-of-googlenet-inception-v1-winner-of-ilsvlc-2014-image-classification-c2b3565a64e7)

[Understandign Inception Network From Scratch](https://www.analyticsvidhya.com/blog/2018/10/understanding-inception-network-from-scratch/)

# Ideas

The fundamental way is to move from full connected to sparsely connected architectures, even inside the convolutions. 
The Inception architecture intends to approximate a sparse structure. The main idea of the Inception architecture is based on finding out how an optimal local sparse structure in a convolutional vision network can be approximated and covered by readily available dense components.

Judiciously applying dimension reduc- tions and projections wherever the computational requirements would increase too much otherwise.
$1 \times 1$ convolutions are used to compute reductions before the expensive $3 \times3$ and $5 \times 5$ convolutions.

An Inception network is a network consisting of modules of the above type stacked upon each other, with occasional max-pooling layers with stride 2 to halve the resolution of the grid.

It aligns with the intuition that visual information should be processed at various scales and then aggregated so that the next stage can abstract features from different scales simultaneously.

# Architecture

The influence of the exact architectural parameters is relatively minor.

- Input_shape: (224, 224) taking RGB color channels with mean subtraction

- Use Global Average Pooling but an FC layer is also used after that.

- To train this deep network, auxilliary classifiers are connected to the intermediate layers. Their loss gets added to the total loss 
    of the network with a discount weight. At inference time they are discarded.

# Training

- SGD: momentum 0.9, fixed learning rate decreased by 4% every 8 epochs

# Testing

Aggressive image cropping is used, resulting in 144 samples for each image. The scores are then averaged over multiple crops and over all the individual classifiers to obtain the final prediction.
