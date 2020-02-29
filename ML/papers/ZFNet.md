M. D. Zeiler and R. Fergus, “Visualizing and Understanding Convolutional Networks,” Nov. 2013.

This paper introduce a novel visualization technique that gives insight into the function of intermediate feature layers and the operation of the classifier. These visualizations allow us to find model architectures that perform better. The visualization technique proposed uses a multi-layered Deconvolutional Network (deconvnet) to project the feature activations back to the input pixel space.

# Approach

## Deconvnet

A novel way to map the activities back to the input pixel space. A deconvnet can be thought of as a convnet model that uses the same components (filtering, pooling) but in reverse. A deconvnet is attached to each layer of a convnet.

To examine a given convnet activation, set all other activations in the layer to zero and pass the feature maps as input to the attacked deconvnet layer.

-> unpool -> rectify -> filter repeated until input pixel space is reached

- unpooling: recording the location of the maximum and place the maxval back into appropriate locations.

- rectification: passing the reconstructed signal through a relu non-linearity

- filtering: transposed versions of the same filters (Wiener deconvolution in reality condition), applied to the rectified maps

The reconstruction obtained from a single activation thus resembles a small piece of the original input image, with structures according to their contribution toward to the feature activation. They implicitly show which parts of the input image are dicriminative.

# ConvNet Architecture

| input $224 \times 224$                      |
| :-----:                                     |
| conv $7\times 7$, $96$, stride = $2$, relu  |
| max pool $3\times 3$ stride=2               |
| normalization                               |
| conv $5\times 5$, $256$, stride = $2$, relu |
| max pool $3\times 3$ stride=2               |
| constrast normalization                     |
| conv $3\times 3$, $384$, stride = $1$, relu |
| max pool $3\times 3$ stride=2               |
| constrast normalization                     |
| conv $3\times 3$, $384$, stride = $1$, relu |
| conv $3\times 3$, $256$, stride = $1$, relu |
| max pool $3\times 3$ stride = $2$           |
| FC 4096, dropout = 0.5                      |
| FC 4096, dropout = 0.5                        |
| C class softmax                             |

# Training Details

- Each image is resized to $256$, cropping the center $256 \times 256$ region, subtracting the per-pixel mean andthen using 10 different sub-crops of size $224 \times 224$.

- batch size = $128$;

- learning rate = $10^{-2}$; momentum = $0.9$; annealed manully when the validation error plateau

- dropout rate = $0.5$

- all weights initialized to $10^{-2}$ and biases set to $0$

Multiple different crops and flips of each training example are produced to boost training set size.

# Visualization

Images patch can have greater variation than the produced activation maps.

- Layer 2 responds to corners and other edge/color conjunctions; 

- Layer 3 has more complex invariances, capturing similar textures;

- Layer 4 shows significant variation but is more class-specific;

- layer 5 shows entire objects with significant pose variation;

The upper layers only develop and stabilize after a considerable number of epochs (40-50). 

Small transformations have a dramatic effect in the first layer of the model, but a lesser impact at the top feature layer. The network output is stable to translations and scalings but not invariant to rotation except for object with rotational symmetry.

The first layer filters are a mix of extremely high and low frequency information. with little coverage of the mid-frequencies. The second layer visualization shows aliasing artifacts caused by teh large stride 4 used in the 1st layer convolutions. To remedy this, the original AlexNet was modified into the architecture above. This improves the classification performance significantly.

By occluding different portions of the input image with a grey square, it is shown that the model is localizing the objects within the scene, as the probability of the correct class drops significantly when the object is occluded.

By occluding the same part of a dog face and computing a Hamming distance, it is shown that the model does establish some degree of correspondence between different images. That is, these parts effect on the model in a relatively consistent way.
