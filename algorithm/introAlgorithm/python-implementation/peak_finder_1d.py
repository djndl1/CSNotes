#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def naive_peak_finder_1d(pl):
    if len(pl) < 2:
        return None
    for i in range(1, len(pl) - 1):
        if pl[i] >= pl[i-1] and pl[i] >= pl[i+1]:
            return i
    if pl[0] >= pl[1]:
        return 0
    if pl[-1] >= pl[-2]:
        return len(pl) - 1
    return None

def __recursive_peak_finder_1d(pl, start_index):
    if len(pl) == 1:
        return start_index
    mid_index = len(pl) // 2
    mid_val = pl[mid_index]
    if len(pl) == 2:
        if mid_val >= pl[0]:
            return start_index + 1
        else:
            return start_index

    if pl[mid_index-1] > mid_val:
        return __recursive_peak_finder_1d(pl[:mid_index], start_index)
    elif pl[mid_index+1] > mid_val:
        return __recursive_peak_finder_1d(pl[mid_index+1:], start_index + mid_index + 1)
    else:
        return start_index + mid_index

def recursive_peak_finder_1d(list):
    if len(list) < 2:
        return None
    else:
        return __recursive_peak_finder_1d(list, 0)
