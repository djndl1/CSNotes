# -*- coding: utf-8 -*-

#%%
import os

import mxnet as mx
from mxnet import gluon, nd

import gluoncv as gcv
from gluoncv.data import VOCDetection
from gluoncv.data.transforms import presets
from gluoncv import utils
from gluoncv.utils import viz
from gluoncv.data.batchify import Tuple, Stack, Pad
from mxnet.gluon.data import DataLoader

import matplotlib.pyplot as plt
#%%

class petVOC(VOCDetection):
    CLASSES = ['cat', 'dog']
    def __init__(self, root=os.path.join('~','.mxnet','datasets','voc'), 
    splits=( (2007,'trainval'), (2012,'trainval')), transform=None, index_map=None, preload_label=True):
        
        super().__init__(root=root, splits=splits, transform=transform, index_map=index_map, preload_label=preload_label)

#%%
train_dataset = petVOC(root=os.path.join('~', 'Datasets', 'pets', 'dataset'), 
                                splits=((2019, 'train_val'),))

# %%
width, height = 168, 168
train_transform = presets.yolo.YOLO3DefaultTrainTransform(width, height)
utils.random.seed(123)  
# %%
num_workers = 3
batch_size = 32

batchify_fn = Tuple(Stack(), Pad(pad_val=-1))
train_loader = DataLoader(train_dataset.transform(train_transform), batch_size, shuffle=True,
                          batchify_fn=batchify_fn, last_batch='rollover', num_workers=num_workers)

# %%
from gluoncv import model_zoo
net = model_zoo.get_model('yolo3_darknet53_voc', pretrained_base=False)
print(net)

# %%

# %%
