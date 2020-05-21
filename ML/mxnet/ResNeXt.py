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

class ResNeXtBlock(gluon.HybridBlock):
    def __init__(self, channels, cardinality=32, cross=False, **kwargs):
        super().__init__(**kwargs)
        with self.name_scope():
            self.block1 = Conv2DBNReLU(channels, (1, 1))
            self.block2 = Conv2DBNReLU(channels, (3, 3), padding=(1, 1), groups=cardinality)
            self.block3 = Conv2DBNReLU(channels * (64 // cardinality), (1, 1), strides=(2, 2) if cross else (1, 1))
            self.cross = cross

            if cross:
                self.shortcut1 = gluon.nn.Conv2D(channels * (64 // cardinality), (1, 1), strides=(2, 2) if cross else (1, 1))
                self.shortcut2 = gluon.nn.BatchNorm()

    def hybrid_forward(self, F, x):
        if self.cross:
            shortcut = self.shortcut1(x)
            shortcut = self.shortcut2(shortcut)
        else:
            shortcut = x

        resout = self.block1(x)
        resout = self.block2(resout)
        resout = self.block3(resout)

        return F.Activation(resout + shortcut, act_type='relu')

#%%
def build_ResNeXt50(n_classes):
    model = gluon.nn.HybridSequential()

    with model.name_scope():
        model.add(Conv2DBNReLU(64, (3, 3), 1, padding=(3, 3)))
        model.add(gluon.nn.MaxPool2D((3, 3), 2, padding=(1, 1)))

        model.add(ResNeXtBlock(128, cross=True))
        for _ in range(2): # 2
            model.add(ResNeXtBlock(128))

        model.add(ResNeXtBlock(256, cross=True))
        for _ in range(2): # 3
            model.add(ResNeXtBlock(256))

        model.add(ResNeXtBlock(512, cross=True))
        for _ in range(3): # 6
            model.add(ResNeXtBlock(512))

        model.add(ResNeXtBlock(1024, cross=True))
        for _ in range(1): # 2
            model.add(ResNeXtBlock(1024))

        model.add(gluon.nn.GlobalAvgPool2D())
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
def train_ResNeXt(net, lr, input_shape, batch_size, train_path, test_path, epoch, ctx):
    train_data, val_data = prepare_data(train_path, test_path, input_shape, batch_size)

    lr_sched = mx.lr_scheduler.FactorScheduler(step=1000, factor=0.94, base_lr=1)
    optim = mx.optimizer.SGD(learning_rate=lr, momentum=0.9, wd=1e-3, lr_scheduler=lr_sched)
    trainer = gluon.Trainer(net.collect_params(), optim)

    loss_fn = gluon.loss.SoftmaxCrossEntropyLoss()

    train_acc_meter = mx.metric.Accuracy()
    train_loss_meter = mx.metric.CrossEntropy()

    hybridized = False

    with mxboard.SummaryWriter(logdir="./resnext_logs", flush_secs=30) as sw:
        for ep in range(1, epoch+1):
            #train_data.reset()
            #val_data.reset()
            print("Current Learning Rate {}".format(trainer.learning_rate))
            epoch_start = timeit.default_timer()

            train_acc_meter.reset()
            train_loss_meter.reset()
            
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

            epoch_stop = timeit.default_timer()
            
            val_loss, val_acc = evaluate(val_data, net, ctx)
            print("Epoch {}, Training time {}, validation loss {:.5f}, validation acc {:.5f}".format(
                    ep, epoch_stop - epoch_start,val_loss, val_acc
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
        
            if ep % 1 == 0:
                net.export("resnext_models/resnext", ep)

    return net

#%%
def prepare_data(train_data_path, test_data_path, pic_size, batch_size):
    train_data_transformer = gluon.data.vision.transforms.Compose([
        gluon.data.vision.transforms.CenterCrop((200, 200)),
        gluon.data.vision.transforms.RandomFlipLeftRight(),
        gluon.data.vision.transforms.RandomColorJitter(0.1, 0.1, 0.1, 0.1),
        gluon.data.vision.transforms.RandomBrightness(0.1),
        gluon.data.vision.transforms.Resize(size=pic_size),
        gluon.data.vision.transforms.ToTensor(),
    ])
    test_data_transformer = gluon.data.vision.transforms.Compose([
        gluon.data.vision.transforms.Resize(size=pic_size),
        gluon.data.vision.transforms.ToTensor()
    ])

    train_datasets = gluon.data.vision.ImageFolderDataset(train_data_path)
    train_datasets = train_datasets.transform_first(train_data_transformer)
    test_datasets = gluon.data.vision.ImageFolderDataset(test_data_path)
    test_datasets = test_datasets.transform_first(test_data_transformer)

    train_data_iter = gluon.data.DataLoader(train_datasets, batch_size, shuffle=True, num_workers=2)
    test_data_iter = gluon.data.DataLoader(test_datasets, batch_size, shuffle=True, num_workers=2)

    return train_data_iter, test_data_iter

#%%
import argparse

def cmd_parser():
    parser = argparse.ArgumentParser(description="Training options")
    parser.add_argument('-c', action='store_true', 
            help='continue to train the model with specified parameters')
    parser.add_argument('--param_path', nargs='?', help='parameter file path')
    parser.add_argument('--model_path', nargs='?', help='model file path')
    parser.add_argument('--lr', type=float , nargs='?', help='learning rate')

    parser.add_argument('--train_path', nargs=1, help="train data path")
    parser.add_argument('--val_path', nargs=1, help="validation data path")
    parser.add_argument('--gpu', action='store_true', help='use GPU')
    args = parser.parse_args()

    return args

if __name__ == "__main__":
    args = cmd_parser()
    train_path = args.train_path
    test_path = args.val_path


    if args.gpu:
        ctx = mx.gpu()
    else:
        ctx = mx.cpu()

    if args.c:
        param_path = args.param_path
        model_path = args.model_path
        resnext = gluon.nn.SymbolBlock.imports(model_path, ['data'], param_path, ctx=ctx)
    else:
        resnext = build_ResNeXt50(256)
        resnext.initialize(mx.init.Xavier(magnitude=2.41), ctx=ctx)
        resnext.summary(nd.zeros(shape=(1, 3) + input_shape, ctx=ctx))
        resnext.hybridize()
    
    net = train_ResNeXt(resnext, args.lr, (80, 80), 36, str(train_path[0]), str(test_path[0]), 60, ctx)

    net.export("resnet", epoch=80)
