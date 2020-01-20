#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np

def __find_greatest_neighbor(y, x, mat):
    '''find the greatest neighbor of mat[y, x]'''
    assert y < mat.shape[0]
    assert x < mat.shape[1]

    neighbors = dict()
    if x != 0:
        neighbors[y, x-1] = mat[y, x-1] # left
    if y != 0:
        neighbors[y-1, x] = mat[y-1, x] # up
    if x != mat.shape[0] - 1:
        neighbors[y, x+1] = mat[y, x+1] # right
    if y != mat.shape[1] - 1:
        neighbors[y+1, x] = mat[y+1, x] # down

    cur_val = mat[y, x]
    cur_pos = y, x
    for pos, val in neighbors.items():
        if val > cur_val:
            cur_val = val
            cur_pos = pos
    return cur_pos
    
# has serious local optimum problem
def greedy_ascent_peak_finder(mat):

    height = mat.shape[0]
    width = mat.shape[1]
    x_start, y_start = np.random.randint(0, width), np.random.randint(0, height)
    y, x = y_start, x_start
    y_prev, x_prev = y, x
    y, x = __find_greatest_neighbor(y, x, mat)
    while y != y_prev or x != x_prev:
        y_prev, x_prev = y, x
        y, x = __find_greatest_neighbor(y, x, mat)
    return y, x

def __find_global_maximum_1d(col):
    assert col.ndim == 1
    return np.argmax(col)

def __recursive_peak_finder_2d(mat, start_col):
    if mat.ndim == 1:
        mat = mat[:,np.newaxis]
    n, m = mat.shape[0], mat.shape[1]

    if m == 1:
        return __find_global_maximum_1d(mat[:,0]), start_col

    j = m // 2
    cur_row = __find_global_maximum_1d(mat[:,j])

    if m == 2:
        if mat[cur_row, j] > mat[cur_row, 0]:
            return cur_row, start_col + 1
        else:
            return __find_global_maximum_1d(mat[:,0]), start_col

    if mat[cur_row, j] < mat[cur_row, j-1]:
        return __recursive_peak_finder_2d(mat[:,:j], start_col)
    elif mat[cur_row, j] < mat[cur_row, j+1]:
        return __recursive_peak_finder_2d(mat[:,j+1:], start_col+j+1)
    else:
        return cur_row, j + start_col

def recursive_peak_finder_2d(mat):
    assert mat.ndim == 2
    if mat.shape[0] * mat.shape[1] == 1:
        return None
    else:
        return __recursive_peak_finder_2d(mat, 0)
