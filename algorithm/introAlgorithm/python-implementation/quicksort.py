#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

def partition(arr, p, r, comp=operator.le):
    x = arr[r] # pivot
    i = p   # the first element of the larger subarray
    for j in range(p, r):
        if comp(arr[j], x):
            arr[i], arr[j] = arr[j], arr[i]
            i += 1
    arr[i], arr[r] = arr[r], arr[i]
    return i

def __qsort(A, p, r, comp):
    if p < r:
        q = partition(A, p, r)
        __qsort(A, p, q-1, comp)
        __qsort(A, q, r, comp)

def quicksort(A, comp=operator.le):
     __qsort(A, 0, len(A)-1, comp)
     return A
