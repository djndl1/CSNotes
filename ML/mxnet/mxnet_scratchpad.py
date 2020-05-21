#%%
import mxnet as mx
from mxnet.gluon import nn
from mxnet import nd, autograd, gluon
from mxnet.gluon.data.vision import datasets, transforms
import timeit
import matplotlib.pyplot as plt
import numpy as np

# %%

class Conv2DBNReLU(gluon.HybridBlock):
    def __init__(self, channels, kernel_size, strides=(1, 1), padding=(0, 0), 
        dilation=(1, 1), groups=1, use_bias=True, weight_initializer=None, bias_initializer='zeros', 
        in_channels=0, **kwargs):
        super().__init__(**kwargs)

        with self.name_scope():
            self.conv = gluon.nn.Conv2D(channels, kernel_size, strides=strides, padding=padding, 
            dilation=dilation, groups=groups, use_bias=use_bias,
            weight_initializer=weight_initializer, bias_initializer=bias_initializer, in_channels=in_channels)
            self.batchnorm = gluon.nn.BatchNorm()
            self.relu = gluon.nn.Activation('relu')
            

    def hybrid_forward(self, F, x):
        conved = self.conv(x)
        normed = self.batchnorm(conved)

        return self.relu(normed)

lenet = nn.HybridSequential()

with lenet.name_scope():
    lenet.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
    lenet.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
    lenet.add(gluon.nn.MaxPool2D((2, 2), 2))
    for _ in range(2):
        lenet.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
    lenet.add(gluon.nn.MaxPool2D((2, 2)))
    for _ in range(3):
        lenet.add(Conv2DBNReLU(128, (3, 3), padding=(1, 1)))
    lenet.add(gluon.nn.MaxPool2D((2, 2)))
    for _ in range(3): 
        lenet.add(Conv2DBNReLU(256, (3, 3), padding=(1, 1)))
    lenet.add(gluon.nn.MaxPool2D((2, 2)))
    for _ in range(3): 
        lenet.add(Conv2DBNReLU(512, (3, 3), padding=(1, 1)))
    lenet.add(gluon.nn.MaxPool2D((2, 2)))
    lenet.add(gluon.nn.Flatten())
    lenet.add(gluon.nn.Dense(1024, activation='relu'))
    lenet.add(gluon.nn.Dense(1024, activation='relu'))
    lenet.add(gluon.nn.Dense(256))



dev = mx.gpu(0)
lenet.initialize(init=mx.init.Xavier(), ctx=dev)

# %%
transformer = transforms.Compose([
    transforms.Resize((64, 64)),
    transforms.ToTensor()
])
#mnist_train = mnist_train.transform_first(transformer)
# %%
mnist_train = gluon.data.vision.ImageFolderDataset("/home/djn/Datasets/caltech256/train")
mnist_train = mnist_train.transform_first(transformer)
batch_size = 64
train_data = gluon.data.DataLoader(mnist_train, batch_size=batch_size, shuffle=True, num_workers=4)

# %%
mnist_valid = gluon.data.vision.ImageFolderDataset("/home/djn/Datasets/caltech256/test")
valid_data = gluon.data.DataLoader(mnist_valid.transform_first(transformer),
                                batch_size=batch_size)

# %%

# %%
softmax_xent = gluon.loss.SoftmaxCrossEntropyLoss()

# %%
trainer = gluon.Trainer(lenet.collect_params(), 'sgd', {'learning_rate': 0.01})

# %%
from mxnet import metric

train_acc = metric.Accuracy()
valid_acc = metric.Accuracy()
# %%
for epoch in range(3):
    with mx.gpu():
        train_loss = nd.zeros((1,), ctx=dev)
        for it , (data, label) in enumerate(train_data):
            data = data.as_in_context(dev)
            label = label.as_in_context(dev)
            with autograd.record():
                output = lenet(data)
                loss_val = softmax_xent(output, label)
            loss_val.backward()

            trainer.step(batch_size)
            train_loss += loss_val.mean()

            train_acc.update(label, output)

            print("Epoch {}, batch {}, train loss {}".format(epoch, it, train_loss.asscalar() / (it+1)))

        for data, label in valid_data:
            data = data.as_in_context(dev)
            label = label.as_in_context(dev)
            outval = lenet(data)
            valid_acc.update(label, outval)

    validation_accuracy = valid_acc.get()[1]
    train_accuracy = train_acc.get()[1]
    print("Epoch {} loss {:.3f}".format(epoch, train_loss.asscalar()/len(train_data)))
    print("Train Acc", train_accuracy)
    print("Test acc", validation_accuracy)

