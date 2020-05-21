https://medium.com/paperspace/tutorial-on-implementing-yolo-v3-from-scratch-in-pytorch-part-1-a0054d38ec78

1. YOLOv3 predicts an objectness score for each bounding box using ligistic regression

2. no more softmax for class prediction. Use independent logistic classifiers instead.

3. from the base feature extractor, several conv layers are added. Upsampled feature maps are concatenated with feature maps from upper layers and go through a few more conv layers.

4. a new hybrid network betwwen darknet-19 and resnet: darknet-53

5. Focal loss actually dropped mAP
