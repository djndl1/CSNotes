import mxnet as mx
from mxnet import gluon, nd
from mxnet.gluon import nn
import numpy as np
import mxboard


class LRN(gluon.nn.HybridBlock):
    def __init__(self, alpha=10e-4, beta=0.75, nsize=5, knorm=2, **kwargs):
        super(LRN, self).__init__(**kwargs)

        self._alpha = alpha
        self._beta = beta
        self._nsize = nsize
        self._knorm = knorm

    def hybrid_forward(self, F, x):
        norm_data = F.LRN(x, self,_alpha, self._beta, self._knorm, self._nsize, name='fwd')
        return norm_data


class AlexNet(gluon.nn.HybridBlock):
    def __init__(self, num_classes, **kwargs):
        super(AlexNet, self).__init__(**kwargs)

        self._num_classes = num_classes

        with self.name_scope():
            self.features = gluon.nn.HybridSequential()
            with self.features.name_scope():
                self.features.add(
                    nn.Conv2D(96, 11, 4, 0, activation='relu'),
                    LRN(),
                    nn.MaxPool2D((3, 3), (2, 2)),
                    nn.Conv2D(256, 5, 1, groups=2, activation='relu'),
                    LRN(),
                    nn.MaxPool2D((3, 3), (2, 2)),
                    nn.Conv2D(384, (3, 3), activation='relu'),
                    nn.Conv2D(384, (3, 3), groups=2, activation='relu'),
                    nn.Conv2D(256, (3, 3), groups=2, activation='relu'),
                    nn.MaxPool2D((3, 3,), (2, 2)),
                    nn.Flatten(),
                )

            self.classifier = gluon.nn.HybridSequential()
            with self.classifier.name_scope():
                self.classifier(
                    nn.Dense(4096, activation='relu'),
                    nn.Dropout(0.5),
                    nn.Dense(4096, activation='relu'),
                    nn.Dropout(0.5),
                    nn.Dense(self._num_classes)
                ) 
        
    def hybrid_forward(self, F, x):
        features = self.features(x)
        return self.classifier(features)



def train_network_loop(net, train_data, val_data, batch_size, ctx=mx.gpu(), lr=0.01, momentum=0.9, wd=5e-4, params=None):
    if params:
        net.load_parameters(params, ctx)
    else:
        net.initialize(ctx=ctx)

    iter_per_epoch = 

    lr_sched = mx.lr_scheduler.FactorScheduler(step=, factor=0.1)
    optim = mx.optimizer.SGD(learning_rate=lr, momentum=momentum, wd=wd, lr_scheduler=lr_sched)
    trainer = mx.gluon.Trainer(net.collect_parameters(), optim)


    

