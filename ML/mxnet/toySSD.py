#%%
import mxnet as mx
from mxnet import nd, gluon
import numpy as np
import matplotlib.pyplot as plt

#%%

def box2rect(box, color, linewidth=3):
    box = box.asnumpy()

    return plt.Rectangle((box[0], box[1]), (box[2] - box[0]), (box[3] - box[1]),
                         fill=False, edgecolor=color, linewidth=linewidth
    )

def class_predictor(num_anchors, num_classes):
    return gluon.nn.Conv2D(num_anchors * (num_classes + 1), 3, padding=1)

def box_predictor(num_anchors):
    "predict delta position"
    return gluon.nn.Conv2D(num_anchors * 4, 3, padding=1)

def down_sample(num_filters):
    out = nn.HybridSequential()
    for _ in range(2):
        out.add(nn.Conv2D(num_filters, 3, strides=1, padding=1))
        out.add(nn.BatchNorm(in_channels=num_filters))
        out.add(nn.Activation('relu'))
    out.add(nn.MaxPool2D(2))
    return out
