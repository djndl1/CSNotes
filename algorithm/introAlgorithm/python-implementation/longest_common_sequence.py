#!/usr/bin/env python3
# -*- coding: utf-8 -*-

S1 = 'ACCGGTCGAGTGCGCGGAAGCCGGCCGAA'
S2 = 'GTCGTTCGGAATGCCGTTGCTCTGTAAA'

def LCS_length_recursive(X, Y):
    '''
    X, Y: sequences in the form of list
    slow as hell
    '''
    if not X or not Y:
        return 0

    if X[-1] == Y[-1]:
        return LCS_length_recursive(X[:-1], Y[:-1]) + 1
    else:
        return max(LCS_length_recursive(X, Y[:-1]),
                   LCS_length_recursive(X[:-1], Y))

import numpy as np

def LCS_length(X, Y):
    if not X or not Y:
        return [], []
    m = len(X)
    n = len(Y)
    b = np.empty((m, n), dtype=int)
    c = np.empty((1+m, 1+n), dtype=int)
    c[:, 0] = 0
    c[0, :] = 0

    for i in range(1, m+1):
        for j in range(1, n+1):
            if X[i-1] == Y[j-1]:
                c[i, j] = c[i-1, j-1] + 1
                b[i-1, j-1] = 1
            elif c[i-1, j] >= c[i, j-1]:
                c[i, j] = c[i-1, j]
                b[i-1, j-1] = 2
            else:
                c[i, j] = c[i, j-1]
                b[i-1, j-1] = 3
    return c, b

def LCS(X, Y):
    '''
    Whenever b[i, j] == 1, it implies x[i] == y[j] is an element of the LCS
    '''
    if not X or not Y:
        return []
    c, b = LCS_length(X, Y)
    i, j = c.shape
    i -= 1
    j -= 1
    lcs = []
    while c[i, j]:
        if b[i-1, j-1] == 1:
            lcs.append(X[i-1])
            i -= 1
            j -= 1
        elif b[i-1,j-1] == 2:
            i -= 1
        else:
            j -= 1
    lcs.reverse()
    return lcs
