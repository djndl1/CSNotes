#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

def find_min_index(arr, base, comp=operator.le):
    if not arr:
        return None

    min_ind = 0
    for i in range(0, len(arr)):
        if not comp(arr[min_ind], arr[i]):
            min_ind = i
    return min_ind + base

def selection_sort(arr, comp=operator.le):
    if len(arr) < 2:
        return arr

    for i in range(0, len(arr)):
        min_ind = find_min_index(arr[i:], i, comp)
        arr[i], arr[min_ind] = arr[min_ind], arr[i]

    return arr

