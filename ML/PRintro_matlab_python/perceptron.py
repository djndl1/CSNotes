#!/usr/bin/env python


import numpy as np


def perce(X, y, w_ini, rho):
    """

    """
    l,N = X.shape
    max_iter = 20000 # Maximum allowable number of iterations
    
    w = w_ini        # Initialization of the parameter vector
    iter = 0         # Iteration counter

    mis_clas = N     # Number of misclassified vectors

    while mis_clas > 0 and iter < max_iter:
        iter = iter + 1
        mis_clas = 0

        gradi = np.zeros(l, 1)
        for i in range(1, N):
            if X[:, i].dot(w) * y[i] < 0:
                mis_clas = mis_clas + 1
                gradi = gradi + (-y[i] * X[:, i])


        if iter == 1:
            print("\n First Iteration: # Misclassified points = %g \n".format(mis_clas))

        w = w - rho * gradi


    return w, iter, mis_clas
