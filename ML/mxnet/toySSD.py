#%%
import mxnet as mx
from mxnet import nd, gluon
from mxnet.gluon import nn
import numpy as np
import matplotlib.pyplot as plt
from mxnet.ndarray.contrib import MultiBoxPrior

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

def flatten_prediction(pred):
    return nd.flatten(nd.transpose(pred, axes=(0, 2, 3, 1)))

def concat_predictions(preds):
    return nd.concat(*preds, dim=1)

def toy_ssd_model(num_anchors, num_classes):
    downsamples = nn.HybridSequential()
    class_preds = nn.HybridSequential()
    box_preds = nn.HybridSequential()

    for _ in range(3):
        downsamples.add(down_sample(128))

    for scale in range(5):
        class_preds.add(class_predictor(num_anchors, num_classes))
        box_preds.add(box_predictor(num_anchors))

    return body(), downsamples, class_preds, box_preds

def toy_ssd_forward(x, body, downsamples, class_preds, box_preds, sizes, ratios):
    x = body(x)

    default_anchors = []
    predicted_boxes = []
    predicted_classes = []

    for i in range(5):
        default_anchors.append(MultiBoxPrior(x, sizes=sizes[i], ratios=ratios[i]))
        predicted_boxes.append(flatten_prediction(box_preds[i](x)))
        predicted_classes.append(flatten_prediction(class_preds[i](x)))

        if i < 3:
            x = downsamples[i](x)
        elif i == 3:
            x = nd.Pooling(x, global_pool=True, pool_type='max', kernel=(4, 4))

    return default_anchors, predicted_classes, predicted_boxes

def body():
    out = nn.HybridSequential()
    for nfilters in [16, 32, 64, 128]:
        out.add(down_sample(nfilters))
    return out


class ToySSD(gluon.HybridBlock):
    def __init__(self, num_classes, **kwargs):
        super(ToySSD, self).__init__(**kwargs)
        self.anchor_sizes = [[.2, .272], 
                             [.37, .447],
                             [.54, .619],
                             [.71, .79], 
                             [.88, .961]]

        self.anchor_ratios = [[1, 2, 0.5]] * 5
        self.num_classes = num_classes

        with self.name_scope():
            self.body, self.downsamples, self.class_preds, self.box_preds = \
                    toy_ssd_model(4, num_classes)

    def hybrid_forward(self, F, x):
        default_anchors, predicted_classes, predicted_boxes = \
                toy_ssd_forward(x, self.body, self.downsamples, self.class_preds, 
                        self.box_preds, self.anchor_sizes, self.anchor_ratios)

        anchors = concat_predictions(default_anchors)
        box_preds = concat_predictions(predicted_boxes)
        class_preds = concat_predictions(predicted_classes)

        class_preds = nd.reshape(class_preds, shape=(0, -1, self.num_classes + 1))

        return anchors, class_preds, box_preds

def get_iterators(data_shape, batch_size):
    class_names = ['pikachu']
    num_class = len(class_names)
    train_iter = image.ImageDetIter(
        batch_size=batch_size,
        data_shape=(3, data_shape, data_shape),
        path_imgrec='./data/pikachu_train.rec',
        path_imgidx='./data/pikachu_train.idx',
        shuffle=True,
        mean=True,
        rand_crop=1,
        min_object_covered=0.95,
        max_attempts=200)
    val_iter = image.ImageDetIter(
        batch_size=batch_size,
        data_shape=(3, data_shape, data_shape),
        path_imgrec='./data/pikachu_val.rec',
        shuffle=False,
        mean=True)
    return train_iter, val_iter, class_names, num_class

from mxnet.contrib.ndarray import MultiBoxTarget

def training_targets(default_anchors, class_predicts, labels):
    class_predicts = nd.transpose(class_predicts, axes=(0, 2, 1))
    z = MultiBoxTarget(*[default_anchors, labels, class_predicts])
    box_target = z[0]
    box_mask = z[1] # ignore certain box offsets 
    cls_target = z[2]

    return box_target, box_mask, cls_target

class FocalLoss(gluon.loss.Loss):
    def __init__(self, axis=-1, alpha=0.25, gamma=2, batch_axis=0, **kwargs):
        super().__init__(None, batch_axis, **kwargs)
        self._axis = axis
        self._alpha = alpha
        self._gamma = gamma

    def hybrid_forward(self, F, output, label):
        output = F.softmax(output)
        pt = F.pick(output, label, axis=self._axis, keepdims=True)
        loss = -self._alpha * ((1 - pt) ** self._gamma) * F.log(pt)
        return F.mean(loss, axis=self._batch_axis, exclude=True)

class SmoothL1Loss(gluon.loss.Loss):
    def __init__(self, batch_axis, **kwargs):
        super().__init__(None, batch_axis, **kwargs)

    def hybrid_forward(self, F, output, label, mask):
        loss = F.smooth_l1((output - label) * mask, scalar=1.0)
        return F.mean(loss, self._batch_axis, exclude=True)