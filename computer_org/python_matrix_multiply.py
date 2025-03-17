#!/usr/bin/env python3
#
import timeit
import numpy as np


def make_matrix(i, j):
    mat = [
        [
            0 for cidx in range(j)
        ] for ridx in range(i)
    ]
    return mat

def matrix_multiply_numpy(A, B):
    arrA = np.asarray(A)
    arrB = np.asarray(B)
    arrC = arrA @ arrB
    return arrC


def matrix_multiply_naive(A, B):
    '''
    A: i, j
    B: j, k
    returns: C i, k
    '''
    i = len(A)
    j = len(A[0])
    k = len(B[0])

    C = make_matrix(i, k)

    for a in range(i):
        for b in range(j):
            for c in range(k):
                C[a][b] += A[a][c] * B[c][b]


def run_matmul(i, j, k):
    A = make_matrix(i, j)
    B = make_matrix(j, k)
    C = matrix_multiply_naive(A, B)
    return C

def run_matmul_np(i, j, k):
    A = make_matrix(i, j)
    B = make_matrix(j, k)
    C = matrix_multiply_numpy(A, B)
    return C

if __name__ == '__main__':
    dims = (960, 960, 960)
    t = timeit.timeit(stmt=lambda: run_matmul(*dims), number=1)
    print(f'Naive: {t}')

    t = timeit.timeit(stmt=lambda: run_matmul_np(*dims), number=1)
    print(f'Numpy: {t}')
