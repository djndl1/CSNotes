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

        self.B = B
        self.S = S
        self.num_classes = num_classes
        self.wd = wd
        self.leaky_alpha = leaky_alpha
        self.batch_size = batch_size
        self.dropout_rate = dropout_rate

        self.output_size = self.S * self.S * (5 * self.B + self.num_classes)

    def yolov1_net(self):
        net = gluon.HybridSequential()
        with net.name_scope():
            net.add(nn.Conv2D(64, (7, 7), strides=(2, 2), padding=3, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha),
                    nn.MaxPool2D((2, 2))
                )

            net.add(nn.Conv2D(192, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha),
                    nn.MaxPool2D((2, 2))
                    )

            nn.add(nn.Conv2D(128, (1, 1), use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

            nn.add(nn.Conv2D(256, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

            nn.add(nn.Conv2D(256, (1, 1), use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

            net.add(nn.Conv2D(512, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha),
                    nn.MaxPool2D((2, 2))
                    )

            for _ in range(4):
                nn.add(nn.Conv2D(256, (1, 1), use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )
                nn.add(nn.Conv2D(512, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )


            nn.add(nn.Conv2D(512, (1, 1), use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

            nn.add(nn.Conv2D(1024, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha),
                    nn.MaxPool2D((2, 2))
                    )

            for _ in range(2):
                nn.add(nn.Conv2D(512, (1, 1), use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

                nn.add(nn.Conv2D(1024, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )

            nn.add(nn.Conv2D(1024, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )
            nn.add(nn.Conv2D(1024, (3, 3), strides=2, padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )
            nn.add(nn.Conv2D(1024, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )
            nn.add(nn.Conv2D(1024, (3, 3), padding=1, use_bias=False),
                    nn.BatchNorm(),
                    nn.LeakyReLU(self.alpha)
                    )


            nn.add(nn.Conv2D(256, (3, 3), padding=1, use_bias=False),
                    nn.LeakyReLU(self.alpha)
                    )
            nn.add(nn.Conv2D(128, (1, 1), use_bias=False),
                    nn.LeakyReLU(self.alpha)
                    )

            net.add(nn.Flatten(),
                    nn.Dropout(self.dropout_rate),
                    nn.Dense(self.output_size)
                    )

            return net
