# Background

R-CNN deal with the tasks of object detection, localization and classification. The output is generally a set of bounding boxes that closely match each of the detected objects. However, R-CNN series are difficult to train and they are too slow at inference time. _YOLO_ and _SSD_ are here to address the issues.

# Ideas

SSD localizes and classifies the objects in a _single forward pass_ of the network; It uses _mutlibox_. SSD's architecture builds on _VGG-16_ architecture but discards the fully connected layers.



Typically, a CNN shrinks the feature map size and increase the depth as it goes to the deeper layer. We can use shallow layers to predict small objects  and deeper layers to predict big objects.

