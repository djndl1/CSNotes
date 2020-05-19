
Implementation https://medium.com/@smallfishbigsea/understand-ssd-and-implement-your-own-caa3232cd6ad

# Background

R-CNN deal with the tasks of object detection, localization and classification. The output is generally a set of bounding boxes that closely match each of the detected objects. However, R-CNN series are difficult to train and they are too slow at inference time. _YOLO_ and _SSD_ are here to address the issues.

# Ideas

The approach discretizes the output space of bounding boxes into a set of default boxes over different aspect ratios and scales per feature map location.
At inference time, scores for the presence of each object category in each default box are generated and adjustments to the box are produced to better
match the object shape.

The improvements include using a small convolutional filter to predict object categories and offsets in bounding box locations, 
using separate predictors (filters) for different aspect ratio detections, and applying these filters to multipleo feature maps 
from the later stages of a network in order to perform detection at multiple scales.

# Single-Shot Detector


- Feed-forward convolutional network that produces a fixed-size collection of bounding boxes and scores for the presence of object instances in those boxes.
    For each bounding box, a shape offset and the confidences for all object categories are produced.

- A small set of default boxes of different aspect ratios at each location in several feature maps with different scales
    At training time, these default boxes are matched to the ground truth boxes.
    
## Multi-scale feature maps for detection

Addtional conv layers after the base network. Decreases in size progressively and allow predictions of detections at multiple scales.

## Convolutional predictors for detection

Each added feature layer can produce a fixed set of detection predictions using a set of convolutional filters.
A (3, 3) is used to produce either a score for a category or a shape offset relative to the default box coordinates. At each location where the kernel is applied,
it produces an output value.

## Default Boxes and Aspect Ratio

Each feature map cell is associated with a set of default bounding boxes. Each location has a total (class+4)k filters where k is the number of locations. yielding 
(c+4)kmn outputs for a (m, n) feature map.

## Choosing scales and aspect ratio for default boxes

The tiling of default boxes is designed so that specific features maps learn to be responsive to particular scales of objects.

The scale of the default boxes for each feature map is computed as

$$
s_{k}=s_{\min}+\frac{s_{\max}-s_{\min}}{m-1}\left(k-1\right),\quad k\in\left[1,m\right]
$$

where $s_{\max}$ or $s_{\min}$ denote the scale of the lowest/highest layer and all layers in between are regularly spaced.

Different aspect ratios are imposed for the default boxes.

## Training

Default boxes are matched to any ground truth with jaccard overlap higher than a threshold, allowing the network to predict high scores for multiple overlapping default boxes.

The objective loss is a weighted sum of the localization loss and the confidence loss.

$$
L\left(x,c,l,g\right)=\frac{1}{N}\left(L_{conf}\left(x,c\right)+\alpha L_{loc}\left(x,l,g\right)\right)
$$

where $n$ is the number of matched default boxes. or the loss is $0$ if $N=0$.
The localization loss is a Smooth L1 loss between the predicted box and the 
ground truth box parameters. The confidence loss is the softmax loss over multipleclasses confidences.

Instead of using all the negative examples, only the top ones are picked so that the ratioi between the negatives and positives is at most 3:1

- data augmentation: use the entire original input image; sample a patch so that the minimum jaccard overlap with the objects is 0.1, 0.3, 0.5, 0.7, 0.9; randomly sample a patch; Each sampled patch is resized to fixed size and is horizontally flipped with probability of 0.5.

