A New Class of CNNs: deep convolutional generative adversarial networks, a strong candidate for unsupervised learning.

# Background and Aim

Learning reusable feature representations from large unlabeled data sets has been an area of active research. A set of constraints on the architectural topology of convolutional GAN that make them stable to train in most settings.

# Architecture 

1. All convolution nets replace deterministic spatial pooling functions with strided convolutions.

2. _Batch normalization_ stabilizes learning.

3. ReLU in the generator except for the output layer which uses the $tanh$ function.

4. The first layer of the GAN which takes a uniform noise distribution $Z$ as input, could be called fully connected as it is just a matrix multiplication, but the result is reshaped into a 4-dimensional tensors and used as the start of the convolution stack.

> Architecture guidelines for stable Deep Convolutional GANs
> • Replace any pooling layers with strided convolutions (discriminator) and fractional-strided convolutions (generator).
> • Use batchnorm in both the generator and the discriminator. 
> • Remove fully connected hidden layers for deeper architectures. 
> • Use ReLU activation in generator for all layers except for the output, which uses Tanh. 
> • Use LeakyReLU activation in the discriminator for all layers.

# Training

- No preprocessing was applied to training images besides scaling to the range of the tanh activation $[-1, 1]$.

- All models were trained with minibatch SGD with a minibatch size of 128.

- All weights were initialized from a zero-centered Normal distribution with standard deviation $0.02$.

- The slope of the leak in the LeakyReLU was set to 0.2.

- Adam optimizer with tuned hpyerparameters

- Learning rate 0.0002, momentum $\beta_{1}$ 0.5.


