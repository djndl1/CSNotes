#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import nuplas np
from peak_finder_1d import *

class PeakIndexError(RuntimeError):
    pass

def isPeak1d(list, peak_ind):
    if peak_ind < 0 or peak_ind >= len(list):
        raise PeakIndexError("Peak index out of range")
    if len(list) < 2:
        return False
    peak = list[peak_ind]
    if peak_ind == len(list) - 1:
        return peak >= list[peak_ind-1]
    elif peak_ind == 0:
        return peak >= list[1]
    else:
        return peak >= list[peak_ind-1] and peak >= list[peak_ind+1]


class TestPeakFinder1d(unittest.TestCase):
    def setUp(self):
        self.low = np.random.randint(1, 10000)
        self.regular_lists = 1000

    def test_zero_length(self):
        l = []
        self.assertEqual(naive_peak_finder_1d(l), None)
        self.assertEqual(recursive_peak_finder_1d(l), None)

    def test_single(self):
        l = [np.random.randint(self.low)]
        self.assertEqual(naive_peak_finder_1d(l), None,
                         "naive_peak_finder_1d failed on\n" + str(l))
        self.assertEqual(recursive_peak_finder_1d(l), None,
                         "recursive_peak_finder_1d failed on\n" + str(l))

    def test_double(self):
        l1 = np.random.randint(self.low, size=2).tolist()
        ind1 = naive_peak_finder_1d(l1)
        self.assertTrue(isPeak1d(l1, ind1),
                        "naive_peak_finder_1d failed on\n" + str(l1) + "\n with index " + str(ind1))
        ind2 = recursive_peak_finder_1d(l1)
        self.assertTrue(isPeak1d(l1, ind2),
                        "recursive_peak_finder_1d failed on\n" + str(l1) + "\nwith index " + str(ind2))

        l2 = [4, 3]
        ind1 = naive_peak_finder_1d(l2)
        self.assertTrue(isPeak1d(l2, ind1))
        ind2 = recursive_peak_finder_1d(l2)
        self.assertTrue(isPeak1d(l2, ind2))

    def test_regulars(self):
        list_lens = np.random.randint(3, self.low, size=self.regular_lists)
        for list_len in list_lens:
            l = np.random.randint(self.low, size=list_len).tolist()
            ind = naive_peak_finder_1d(l)
            self.assertTrue(isPeak1d(l, ind),
                    "naive_peak_finder_1d failed on\n" + str(l) + "\n with index " + str(ind))
            ind = recursive_peak_finder_1d(l)
            self.assertTrue(isPeak1d(l, ind),
                           "recursive_peak_finder_1d failed on\n" + str(l) + "\nwith index " + str(ind))

#class TestPeakFinder2d(unittest.TestCase)

#   def setUp(self):
    

if __name__ == '__main__':
    unittest.main()
