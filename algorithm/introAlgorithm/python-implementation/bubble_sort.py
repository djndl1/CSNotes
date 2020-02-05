#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

def optimized_bubble_sort(arr, comp=operator.le):
    if len(arr) < 2:
        return arr

    n = len(arr)
    while n > 1:
        next_n = 0
        for i in range(1, n):
            if not comp(arr[i-1], arr[i]): # if out of order, then swap
                arr[i-1], arr[i] = arr[i], arr[i-1]
                next_n = i
        n = next_n
        # any pair between [next_n:] is in order and will remain unchanged
        # arr[next_n] is greater than arr[:next_n] so that swapping occurs
        # only in arr[:next_n]
    return arr

def naive_bubble_sort(arr, comp=operator.le):
    if len(arr) < 2:
        return arr

    while True:
        swapped = False
        for j in range(1, len(arr)):
            if not comp(arr[j-1], arr[j]): # swap whenever out of order
                arr[j-1], arr[j] = arr[j], arr[j-1]
                swapped = True
        if not swapped: # until not swapped
            break

    return arr


