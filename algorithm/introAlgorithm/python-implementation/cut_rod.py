#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import math

price_table = [0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

def cut_rod_recursive(p, n):
    if not n:
        return 0
    q = 0
    for i in range(1, n+1):
        q = max(q, p[i] + cut_rod_recursive(p, n-i))
    return q

def memoized_cut_rod(p, n):
    r = []
    for i in range(n+1):
        r.append(-1)
    # the only thing it does is to initialize a revenue table
    return memoized_cut_rod_aux(p, n, r)

def memoized_cut_rod_aux(p, n, r):
    """
    p: price table
    n: length
    r: revenue table
    """
    if r[n] >= 0:       # check if already known
        return r[n]

    if not n: # if unknown
        q = 0
    else:
        q = -1
        for i in range(1, n+1):
            q = max(q, p[i] + memoized_cut_rod_aux(p, n-i, r))
    r[n] = q    # save it

    return q

def bottom_up_cut_rod(p, n):
    if n > len(p)+1:
        return None
    r = [0]
    for j in range(1, n+1):
        q = -1
        for i in range(1, j+1):
            q = max(q, p[i] + r[j-i]) # if p[i] + r[j-i] > q: q = ...
        r.append(q)
    return r[n]


def extended_bottom_up_cut_rod(p, n):
    if n > len(p)+1:
        return None
    r = [0]
    s = [0] * (n+1)
    for j in range(1, n+1):
        q = -1
        for i in range(1, j+1):
            if q < p[i] + r[j-i]:
                q = p[i] + r[j-i]
                s[j] = i        # s records the cut position that the
                                # remainder has the optimal solution
        r.append(q)
    return r, s

def cut_rod_solution(p, n):
    r, s = extended_bottom_up_cut_rod(p, n)
    cuts = []
    while n > 0:
        cuts.append(s[n])
        n -= s[n]
    return cuts
