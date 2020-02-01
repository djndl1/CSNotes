#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

def naive_insertion_sort(arr, comp=operator.le):
    if len(arr) < 2:
        return arr
    for j in range(1, len(arr)):
        key = arr[j]
        i = j
        while i > 0 and comp(key, arr[i-1]):
            arr[i] = arr[i-1]
            i -= 1
        arr[i] = key
    return arr

def binary_search(arr, base_ind, val, comp=operator.le):
    '''Not much better than vanilla insertion sort'''
    if len(arr) == 1:
        if comp(val, arr[0]):
            return base_ind
        else:
            return base_ind+1
    mid_ind = len(arr) // 2

    if comp(arr[mid_ind], val):
        return binary_search(arr[mid_ind:], base_ind+mid_ind, val, comp)
    else:
        return binary_search(arr[:mid_ind], base_ind, val, comp)


def binary_insertion_sort(arr, comp=operator.le):
    if len(arr) < 2:
        return arr
    insertion_pos = []
    for j in range(1, len(arr)):
        key = arr[j]
        insert_ind = binary_search(arr[:j], 0, key, comp)
        insertion_pos.append(insert_ind)
        if insert_ind < j:
            arr[insert_ind+1:j+1] = arr[insert_ind:j]
            arr[insert_ind] = key
            # don't know which one is faster
            #del arr[j]
            #arr.insert(insert_ind, key)
            
    return arr
