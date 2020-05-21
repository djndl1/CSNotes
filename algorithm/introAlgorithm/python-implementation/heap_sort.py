#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

class heap:
    def __init__(self, arr=[], comp=operator.le):
        self.elems = arr
        self.size = len(arr)
        self.comp = comp

        self.heapify(arr)

    def __repr__(self):
        return self.elems.__repr__() + self.comp.__repr__()

    def __str__(self):
        return self.elems.__str__() + self.comp.__str__()

    def __len__(self):
        return self.size

    def isHeapified(self):
        for i in range(0, len(self)//2):
            if heap._left(i) < len(self) and not self.comp(self.elems[i], self.elems[heap._left(i)]):
                return False
            if heap._right(i) < len(self) and not self.comp(self.elems[i], self.elems[heap._right(i)]):
                return False
        return True

    @staticmethod
    def _left(ind):
        return ind*2 + 1

    @staticmethod
    def _right(ind):
        return ind*2 + 2

    @staticmethod
    def _parent(ind):
        return (ind-1) // 2

    def partial_heapify(self, i):
        l, r = heap._left(i), heap._right(i)
        smallest = i
        if l < len(self) and self.comp(self.elems[l], self.elems[i]):
            smallest = l
        if r < len(self) and self.comp(self.elems[r], self.elems[smallest]):
            smallest = r
        if smallest != i:
            self.elems[i], self.elems[smallest] = self.elems[smallest], self.elems[i]
            self.partial_heapify(smallest)

    def heapify(self, arr):
        for i in range(heap._parent(len(self)-1), -1, -1):
            self.partial_heapify(i)

    def pop(self):
        if len(self) == 0:
            return None
        min = self.elems[0]
        last = self.elems[len(self)-1]
        self.size -= 1

        i = 0
        while heap._left(i) < len(self):
            child = heap._left(i)
            if child+1 < len(self) and self.comp(self.elems[child+1], self.elems[child]):
                child += 1

            if self.comp(self.elems[child], last):
                self.elems[i] = self.elems[child]
                i = child
            else:
                break
        self.elems[i] = last

        return min

    def pop2(self):
        if len(self) == 0:
            return None
        self.elems[0], self.elems[len(self)-1] = self.elems[len(self)-1], self.elems[0]
        min = self.elems[len(self)-1]
        self.size -= 1
        self.partial_heapify(0)
        return min

    def insert(self, elem):
        self.elems.append(elem)
        self.size += 1

        i = len(self)-1
        while not self.comp(self.elems[heap._parent(i)], elem):
            if (i == 0):
                break
            self.elems[i] = self.elems[heap._parent(i)]
            i = heap._parent(i)
        self.elems[i] = elem

    @staticmethod
    def heap_sort2(arr, comp=operator.le):
        h = heap(arr, comp)
        while len(h) != 0:
            h.pop2()

        return list(reversed(h.elems))

    @staticmethod
    def heap_sort(arr, comp=operator.le):
        b = []
        h = heap(arr, comp)
        while len(h) != 0:
            b.append(h.pop())
        
        return b

import unittest
import numpy as np

class TestHeap(unittest.TestCase):
    def setUp(self):
        self.low = np.random.randint(1, 20000)
        self.regular_lists = 1000 # sort 1000 arrays

    def test_empty_heapify(self):
        h = heap([])
        self.assertTrue(h.isHeapified())
        self.assertEqual(h)

