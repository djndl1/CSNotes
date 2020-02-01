#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import operator
import numpy as np
import argparse


def isSorted(arr, comp=operator.le): # comp defaulted to  <=
    if len(arr) == 0:
        return True
    prev = arr[0]
    for elem in arr[1:]:
        if not comp(prev, elem):
            return False
        prev = elem
    return True

class ParametrizedTestCase(unittest.TestCase):
    """ TestCase classes that want to be parametrized should
        inherit from this class.
    """
    def __init__(self, methodName='runTest', param1=None, param2=None):
        super(ParametrizedTestCase, self).__init__(methodName)
        self.param1 = param1
        self.param2 = param2

    @staticmethod
    def parametrize(testcase_klass, param1=None, param2=None):
        """ Create a suite containing all tests taken from the given
            subclass, passing them the parameter 'param'.
        """
        testloader = unittest.TestLoader()
        testnames = testloader.getTestCaseNames(testcase_klass)
        suite = unittest.TestSuite()
        for name in testnames:
            suite.addTest(testcase_klass(name, param1=param1, param2=param2))
        return suite


class TestSortingInteger(ParametrizedTestCase):
    def setUp(self):
        # control integer scale and array length
        self.low = np.random.randint(1, 2000)
        self.regular_lists = 1000 # sort 1000 arrays

    def test_empty_arr(self):
        arr = []
        result = self.param1(arr, self.param2)
        self.assertTrue(isSorted(result, self.param2))
        self.assertEqual(set(arr), set(arr))

    def test_single_arr(self):
        arr = np.random.randint(self.low, size=1).tolist()
        result = self.param1(arr, self.param2)
        self.assertTrue(isSorted(result, self.param2))
        self.assertEqual(set(arr), set(arr))

    def test_regular_arr(self):
        lens = np.random.randint(self.low, size=self.regular_lists)
        for i in lens:
            arr = np.random.randint(self.low, size=i).tolist()
            result = self.param1(arr, self.param2)
            self.assertTrue(isSorted(result, self.param2))
            self.assertEqual(set(arr), set(result))

from merge_sort import merge_sorted_array

class TestMerge(unittest.TestCase):
    def setUp(self):
        # control integer scale and array length
        self.low = np.random.randint(1, 20000)
        self.regular_lists = 1000 # sort 1000 arrays

    def test_merge_empty(self):
        a = []
        b = []
        lst_len = np.random.randint(self.low)
        c = np.random.randint(self.low, size=lst_len).tolist()
        c.sort()
        res = merge_sorted_array(a, b)
        self.assertEqual(a, res)
        self.assertTrue(isSorted(res))

        res = merge_sorted_array(a, c)
        self.assertEqual(c, res)
        self.assertTrue(isSorted(res))

        res = merge_sorted_array(c, a)
        self.assertEqual(c, res)
        self.assertTrue(isSorted(res))

    def test_merge_equal(self):
        lens = np.random.randint(self.low, size=self.regular_lists)
        for l in lens:
            arr1 = np.random.randint(self.low, size=l).tolist()
            arr1.sort()
            arr2 = np.random.randint(self.low, size=l).tolist()
            arr2.sort()
            arr3 = arr1 + arr2
            arr3.sort()
            result = merge_sorted_array(arr1, arr2)
            msg = "\nArr1: {0}\nArr2: {1}\nResult: {2}".format(arr1,
                                                             arr2,
                                                             result)
            self.assertTrue(isSorted(result), msg)
            self.assertEqual(arr3, result, msg)

    def test_merge_regular(self):
        lens1 = np.random.randint(self.low, size=self.regular_lists)
        lens2 = np.random.randint(self.low, size=self.regular_lists)
        for i in range(0, len(lens1)):
            arr1 = np.random.randint(self.low, size=lens1[i]).tolist()
            arr1.sort()
            arr2 = np.random.randint(self.low, size=lens2[i]).tolist()
            arr2.sort()
            arr3 = arr1 + arr2
            arr3.sort()
            result = merge_sorted_array(arr1, arr2)
            msg = "\nArr1: {0}\nArr2: {1}\nResult: {2}".format(arr1,
                                                             arr2,
                                                             result)
            self.assertTrue(isSorted(result), msg)
            self.assertEqual(arr3, result, msg)


import insertion_sort
import merge_sort

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Sorting Algorithm Unit Tests")
    parser.add_argument('--insertion_sort', action='store_true',
                        help='enable tests for insertion sort')
    parser.add_argument('--merge_sort', action='store_true',
                        help='enable tests for merge sort')
    parser.add_argument('-a', '--all', action='store_true',
                        help='enable all tests')

    args = parser.parse_args()

    sorted_suite = unittest.TestSuite()
    if args.insertion_sort or args.all:
        sorted_suite.addTest(ParametrizedTestCase.parametrize(TestSortingInteger,
                                                          insertion_sort.naive_insertion_sort,
                                                          operator.le))
        sorted_suite.addTest(ParametrizedTestCase.parametrize(TestSortingInteger,
                                                          insertion_sort.binary_insertion_sort,
                                                          operator.le))
    if args.merge_sort or args.all:
        sorted_suite.addTests([TestMerge('test_merge_empty'),
                               TestMerge('test_merge_equal'),
                               TestMerge('test_merge_regular')],)
        sorted_suite.addTest(ParametrizedTestCase.parametrize(TestSortingInteger,
                                                              merge_sort.merge_sort,
                                                          operator.le))
    unittest.TextTestRunner(verbosity=2).run(sorted_suite)
