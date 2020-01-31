#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator
import unittest
import numpy as np

def merge_sorted_array(arr1, arr2, comp=operator.le):
    if len(arr1) == 0:
        return arr2
    if len(arr2) == 0:
        return arr1
    cnt1 = 0
    cnt2 = 0
    new_arr = []
    while cnt1 < len(arr1) and cnt2 < len(arr2):
#        print('cnt1 = {0}, cnt2 = {1}'.format(cnt1, cnt2))
        if comp(arr1[cnt1], arr2[cnt2]):
            new_arr.append(arr1[cnt1])
            cnt1 += 1
        else:
            new_arr.append(arr2[cnt2])
            cnt2 += 1
    if len(arr1) > cnt1:
        new_arr.extend(arr1[cnt1:])
    if len(arr2) > cnt2:
        new_arr.extend(arr2[cnt2:])

    return new_arr

def merge_sort(arr, comp=operator.le):
    if len(arr) > 1:
        q = len(arr) // 2
        larr = merge_sort(arr[:q], comp)
        rarr = merge_sort(arr[q:], comp)
        return merge_sorted_array(larr, rarr, comp)
    else:
        return arr
