#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

def greedy_ascent_peak_finder(mat):
    length = mat.shape[0]
    height = mat.shape[1]
    start = (np.random.randint(0, length), np.random.randint(0, height))
