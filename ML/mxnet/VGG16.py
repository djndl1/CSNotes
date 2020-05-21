#%%
import mxnet as mx
from mxnet import gluon, nd, autograd
import mxboard
import numpy as np
import matplotlib.pyplot as plt
import timeit

#%%

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

#%%
def build_network(n_classes):
    model = gluon.nn.HybridSequential(prefix="VGG16")

    with model.name_scope():
        model.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
        model.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
        model.add(gluon.nn.MaxPool2D((2, 2), 2))

        for _ in range(2):
            model.add(Conv2DBNReLU(64, (3, 3), padding=(1, 1)))
        model.add(gluon.nn.MaxPool2D((2, 2), 2))

        for _ in range(3):
            model.add(Conv2DBNReLU(128, (3, 3), padding=(1, 1)))
        model.add(gluon.nn.MaxPool2D((2, 2), 2))

        for _ in range(3): 
            model.add(Conv2DBNReLU(256, (3, 3), padding=(1, 1)))
        model.add(gluon.nn.MaxPool2D((2, 2), 2))

        for _ in range(3): 
            model.add(Conv2DBNReLU(512, (3, 3), padding=(1, 1)))
        model.add(gluon.nn.MaxPool2D((2, 2), 2))

        model.add(gluon.nn.Flatten())
        model.add(gluon.nn.Dense(1024, activation='relu'))
        model.add(gluon.nn.Dropout(0.5))
        model.add(gluon.nn.Dense(1024, activation='relu'))
        model.add(gluon.nn.Dropout(0.5))
        model.add(gluon.nn.Dense(n_classes))

    return model

#%%
def evaluate(data_iterator, net, ctx):
    acc_meter = mx.metric.Accuracy()
    loss_meter = mx.metric.CrossEntropy()
    for i, (data, label) in enumerate(data_iterator):
        data = data.as_in_context(ctx)
        label = label.as_in_context(ctx)
        output = net(data)
        acc_meter.update(preds=[output], labels=[label])
        loss_meter.update([label], [nd.softmax(output, axis=1)])
    return loss_meter.get()[1], acc_meter.get()[1]
#%%

def train_network(net, lr, input_shape, batch_size, train_path, test_path, epoch, ctx):
    train_data, val_data = prepare_data(train_path, test_path, input_shape, batch_size)

    for X, y in train_data:
        print("X shape {}, y shape", X.shape, y.shape)
        break

    net.initialize(mx.init.Xavier(magnitude=2.24), ctx=ctx)

    net.summary(nd.zeros(shape=(1, 3) + input_shape, ctx=ctx))

    net.hybridize()

    lr_sched = mx.lr_scheduler.FactorScheduler(2000, factor=0.6, base_lr=1.0)
    optim = mx.optimizer.SGD(learning_rate=lr, momentum=0.9, wd=0.0001, lr_scheduler=lr_sched)
    trainer = gluon.Trainer(net.collect_params(), optim)

    loss_fn = gluon.loss.SoftmaxCrossEntropyLoss()

    train_acc_meter = mx.metric.Accuracy()
    train_loss_meter = mx.metric.CrossEntropy()

    hybridized = False

    with mxboard.SummaryWriter(logdir="./vgg_logs", flush_secs=60) as sw:
        for ep in range(1, epoch+1):
            epoch_start = timeit.default_timer()

            train_acc_meter.reset()
            train_loss_meter.reset()
            
            print("Current Learning Rate: {}".format(trainer.learning_rate))
            for it, (data, label) in enumerate(train_data):
                data = data.as_in_context(ctx)
                label = label.as_in_context(ctx)

                with autograd.record():
                    output = net(data)
                    loss_val = loss_fn(output, label)
                loss_val.backward()
                trainer.step(data.shape[0])

                train_acc_meter.update(preds=[output], labels=[label])
                train_loss_meter.update(labels=[label], preds=[nd.softmax(output, axis=1)])

                if it % 10 == 0:            
                    print("Epoch {}, batch {}, train loss {:.4f}, train acc {:.4f}".format(
                        ep, it, train_loss_meter.get()[1], train_acc_meter.get()[1]
                    ))

            nd.waitall()
            epoch_stop = timeit.default_timer()
            
            val_loss, val_acc = evaluate(val_data, net, ctx)
            nd.waitall()
            print("Epoch {}, Training time {}, learning rate {}, validation loss {:.5f}, validatoin acc {:.5f}".format(
                    ep, epoch_stop - epoch_start, trainer.learning_rate ,val_loss, val_acc
                ))
            sw.add_scalar(tag="train_loss", value=train_loss_meter.get()[1],
            global_step=ep)
            sw.add_scalar(tag="train_acc", value=train_acc_meter.get()[1],
            global_step=ep)
            sw.add_scalar(tag="val_acc", value=val_acc, global_step=ep)
            sw.add_scalar(tag="val_loss", value=val_loss, global_step=ep)
            sw.add_scalar(tag="learning_rate", value=trainer.learning_rate, global_step=ep)
            if not hybridized:
                sw.add_graph(net)
                hybridized = True
        
            if ep % 2 == 0:
                net.export("vgg_models/vgg", ep)

    return net

#%%
def prepare_data(train_data_path, test_data_path, pic_size, batch_size):
    train_data_transformer = gluon.data.vision.transforms.Compose([
        gluon.data.vision.transforms.RandomFlipLeftRight(),
        gluon.data.vision.transforms.RandomColorJitter(0.1, 0.1, 0.1, 0.1),
        gluon.data.vision.transforms.RandomBrightness(0.3),
        gluon.data.vision.transforms.Resize(size=pic_size),
        gluon.data.vision.transforms.ToTensor()
    ])
    test_data_transformer = gluon.data.vision.transforms.Compose([
        gluon.data.vision.transforms.Resize(size=pic_size),
        gluon.data.vision.transforms.ToTensor()
    ])

    train_datasets = gluon.data.vision.ImageFolderDataset(train_data_path)

    test_datasets = gluon.data.vision.ImageFolderDataset(test_data_path)
    
    train_data_iter = gluon.data.DataLoader(train_datasets.transform_first(train_data_transformer), batch_size,
            shuffle=True, num_workers=4)
    test_data_iter = gluon.data.DataLoader(test_datasets.transform_first(test_data_transformer), batch_size,
            shuffle=True, num_workers=4)

    return train_data_iter, test_data_iter

#%%

if __name__ == "__main__":
    train_path = "/home/djn/Datasets/caltech256/train"
    test_path = "/home/djn/Datasets/caltech256/test"

    net = build_network(256)
    
    net = train_network(net, 0.01, (72, 72), 48, train_path, test_path, 80, mx.gpu())

    net.export("resnet", epoch=80)
