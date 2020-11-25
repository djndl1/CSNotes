import mxnet as mx
from mxnet.gluon import data, nn
from mxnet import gluon, nd
from mxnet.gluon.data.vision.datasets import ImageFolderDataset
from mxnet.gluon.data import ArrayDataset
from mxnet.gluon.data.vision import transforms

import os

def get_folder_data(train_path, val_path, data_shape, batch_size, num_workers=os.cpu_count()):
    train_dataset = ImageFolderDataset(train_path)
    val_dataset = ImageFolderDataset(val_path)

    train_transformer = gluon.data.vision.transforms.Compose([
        transforms.RandomFlipLeftRight(),
        transforms.RandomResizedCrop(data_shape, scale=(0.5, 1.0)),
        transforms.RandomBrightness(0.5),
        transforms.RandomHue(0.1),
        transforms.Resize(data_shape),
        transforms.ToTensor()
    ])
    val_transformer = gluon.data.vision.transforms.Compose([
        transforms.Resize(data_shape),
        transforms.ToTensor()
    ])

    train_dataloader = data.DataLoader(train_dataset.transform_first(train_transformer),
                                         batch_size=batch_size, shuffle=True, last_batch='rollover', 
                                        num_workers=num_workers)
    val_dataloader = data.DataLoader(val_dataset.transform_first(val_transformer),
                                         batch_size=batch_size, shuffle=True, last_batch='rollover', 
                                        num_workers=num_workers)

    return train_dataloader, val_dataloader


def get_array_data(train_array, val_array, data_shape, batch_size, num_workers=os.cpu_count()):
    train_dataset = ArrayDataset(train_array)
    val_dataset = ArrayDataset(val_array)

    train_transformer = gluon.data.vision.transforms.Compose([
        transforms.RandomFlipLeftRight(),
        transforms.RandomResizedCrop(data_shape, scale=(0.5, 1.0)),
        transforms.RandomBrightness(0.5),
        transforms.RandomHue(0.1),
        transforms.Resize(data_shape),
        transforms.ToTensor()
    ])
    val_transformer = gluon.data.vision.transforms.Compose([
        transforms.Resize(data_shape),
        transforms.ToTensor()
    ])

    train_dataloader = data.DataLoader(train_dataset.transform_first(train_transformer),
                                         batch_size=batch_size, shuffle=True, last_batch='rollover', 
                                        num_workers=num_workers)
    val_dataloader = data.DataLoader(val_dataset.transform_first(val_transformer),
                                         batch_size=batch_size, shuffle=True, last_batch='rollover', 
                                        num_workers=num_workers)

    return train_dataloader, val_dataloader