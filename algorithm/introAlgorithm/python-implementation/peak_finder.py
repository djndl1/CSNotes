#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def naive_peak_finder_1d(list):
    if len(list) < 2:
        return None
    for i in range(1, len(list) - 1):
        if list[i] >= list[i-1] and list[i] >= list[i+1]:
            return list[i]
    if list[0] >= list[1]:
        return list[0]
    if list[-1] >= list[-2]:
        return list[-1]
    return None

def __recursive_peak_finder_1d(list, start_index):
    if len(list) == 1:
        return start_index
    mid_index = len(list) // 2
    mid_val = list[mid_index]
    if len(list) == 2:
        if mid_val >= list[0]:
            return start_index + 1
        else:
            return start_index

    if list[mid_index-1] > mid_val:
        return __recursive_peak_finder_1d(list[:mid_index], start_index)
    elif list[mid_index+1] > mid_val:
        return __recursive_peak_finder_1d(list[mid_index+1:], start_index + mid_index + 1)
    else:
        return start_index + mid_index

def recursive_peak_finder_1d(list):
    if len(list) < 2:
        return None
    else:
        return __recursive_peak_finder_1d(list, 0)
