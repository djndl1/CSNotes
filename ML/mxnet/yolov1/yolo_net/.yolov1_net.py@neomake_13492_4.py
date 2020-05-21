#! -*- coding: utf-8 -*-

import mxnet as mx
from mxnet import gluon, nd
from mxnet.gluon import nn

class yolov1:
    def __init__(self, S=7, B=3, num_classes=20, batch_size=32,
            wd=1e-4, leaky_alpha=0.1, dropout_rate=0.4):
        """
        :param S: grid dimension 
        :param B: number of boxes per grid
        :param wd: l2 weight decay coefficient
        :param leaky_alpha: Leaky ReLU alpha
        :param dropout_rate: dropout rate
        """

