Instead of linear filters followed by a nonlinear activation function, micro neural networks with more complex structures, here a MLP, to abstract the data within the receptive field are used. The GLM is replaced with a micro-network structure which is a general nonlinear function approximator.

The mlpconv maps the input local patch to the output feature vector with a MLP consisting of multiple FC??? layers with nonlinear activation functions.

The linear convolution is sufficient for abstraction when the instances of the latent concepts are linearly separable. However, representations that achieve good abstraction are generally highly nonlinear functions of the input data. Conventional CNNs use over-complete filters to cover all variations of the latent concepts.

# NIN

## MLP ConvLayer

Radial basis network and multlayer perceptron are two well-known universal function approximators. MLP is trained with back-prop, compatible with CNN and can itself form a deep model. 

$$
f_{i,j,k_{d}}^{d}=\max\left(w_{k_{d}}^{dT}f_{i,j}^{d-1}+b_{k_{d}},0\right)
$$

ReLU is used as the activation function in the MLP.

It is equivalent to cascaded cross channel parametric pooling on a normal convlayer. ???

## Global Average Pooling

Classical CNNs treats the conv layers as feature extractors, and the resulting feature is classified in a traditional way. However, FC layers are prone to overfitting.

The idea of global average pooling is to generate one feature map for each corresponding category of the classification task in the last mlpconv layer. Each feature map is taken its average and the resulting vector is fed directly into the softmax layer. It is more native to the convolution structure by enforcing correspondences between feature maps and categories. The feature maps can be easily interpreted as categories confidence maps. There is no feature maps to optimize thus overfitting is avoided at this layer.

Global average pooling as a structural regularizer explicitly enforces feature maps to be confidence maps of concepts (categories).

## Architecture

Three stacked mlpconv layers, each of which is followed by a spatial max pooling layer. Dropout is applied on the outputs of all but the last mlpconv layers. The final layer is global average pooling. Weight decay is used.
