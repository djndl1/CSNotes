#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import numpy as np
import pandas as pd
import operator


def classify0(inX, dataSet, labels, k):
    class_count = dict()

    diff = inX - dataSet
    dists = np.linalg.norm(diff, axis=-1)
    sorted_indices = dists.argsort()
    all_labels = np.unique(labels.flatten()).tolist()
    k_labels = labels[sorted_indices[:k]]
    for label in all_labels:
        class_count[label] = np.sum(k_labels == label)
    sorted_counts = sorted(class_count, key=operator.itemgetter(1), reverse=True)

    return sorted_counts[0][0]

def file2matrix(filename):
    love_dict = {'largeDoses': 3, 'smallDoses': 2, 'didntLike': 1}
    love_df = pd.read_csv(filename, sep='\t')
    X = love_df.iloc[:, :3].values
    Y = love_df.iloc[:, 3].values
    for k, v in love_dict.items():
        Y[Y == k] = v
    Y = Y.astype('int')

    return X, Y
