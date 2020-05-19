Redmon, J., & Farhadi, A. (2017). YOLO9000: Better, faster, stronger. Proceedings - 30th IEEE Conference on Computer Vision and Pattern Recognition, CVPR 2017, 2017-Janua, 6517–6525. https://doi.org/10.1109/CVPR.2017.690

# Overview

1. a new method to harness the large amount of classification data and use it to expand the scope of current detection systems.

2. a joint training algorithm that allows to train object detectors on both detection and classification data, i.e. leverages labeled detection images to learn to precisely localize objects while it uses classification images to increase its vocabulary and robustness.

## Better Than YOLOv1

To improve recall and localization while maintaining classification accuracy

1. Batch Normalization, 2% mAP improvement

2. High resolution classifier: finetune the classification network at the full resolution (448, 448) for 10 epochs and then fine tune the resulting network on detection.

3. Convolution with anchor boxes: the FC layer in YOLO was removed and anchor boxes are used to predict bounding boxes. The network predict class and objectness for every anchor box. The objectness still predicts the IOU of the ground truth and the proposed box and the class predictions predict the conditional probability of that class given that there is an object. The output feature map is (13, 13).

4. k-means clustering is run on the training set bounding box to automatically find good priors. The distance metric is defined as `d(box, centroid) = 1 - IOU(box, centroid)`. k = 5 is chosen.

5. direct location predictions: instead of predicting offsets, the location coordinates relative to the location of the grid cell is predicted. logistic activation is used to constrain the network's predictions to fall in this range.

6. Fine-Grained Features: (26, 26) high resolution features of the earlier layer are transformed into (13, 13) and concatenated with the original features.

7. Multi-scale training: this full-conv net is trained at different input shape ranging from (320, 320) to (608, 608)

## Faster: Real Time Detection

- Darknet-19 base model

## Stronger

combine classfication and detection data by utilizing  hierarchical classification.
