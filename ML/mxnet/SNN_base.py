#-*- coding: utf-8 -*-

import numpy as np

class SNN_base:

    def __init__(self, B=1, C=1, lamda=1, iteration=30):
        self.B = B
        self.C = C
        self.lamda = lamda
        self.iteration = iteration

    def _evolve_numpy(self, order_param, iteration):
        for i in range(iteration):
            D = (self.B + self.C) * np.sum(np.square(order_param))
            gamma = 1.0 / D
            order_param += gamma * (self.lamda - self.B * np.square(order_param)) * order_param

        return order_param 

    def _predict_numpy(self, x, iteration=40):
        x_scaled = x.astype(np.float)
        x_scaled = (x_scaled - x_scaled.mean(axis=-1)[:, np.newaxis]) / x_scaled.std(axis=-1)[:, np.newaxis]
        order_params = x_scaled.dot(self._weight_numpy)
        order_params = self._evolve_numpy(order_params, iteration)

        return order_params

    def _scap(self, x, y):
        proto = np.empty_like(self.prototypes)
        for klas in range(0, self.n_classes):
            klas_mean = x[y == klas].mean(axis=0)
            proto[klas] = klas_mean

        return proto

    def _scapal(self, x, y, learning_rate, scapal_iter=100):
        self.avr = self._scap(x, y)
        for i in range(scapal_iter):
            assert not np.any(self.avr) == np.nan
            self._weight_numpy = np.linalg.pinv(self.avr)
            pred = self._predict_numpy(x).argmax(axis=-1)
            misidx = np.not_equal(pred, y)

            # compute delta
            x_mis, y_mis = x[misidx], y[misidx]
            self.mis_delta = np.zeros_like(self.prototypes)
            for klas in range(0, self.n_classes):
                mis_klas = x_mis[y_mis == klas]
                if mis_klas.shape[0] == 0:
                    continue
                assert mis_klas.shape[0] != 0

                klas_mean = mis_klas.mean(axis=0)
                klas_mean = (klas_mean - klas_mean.mean()) / klas_mean.std()
                self.mis_delta[klas] = klas_mean
            
            # adjust
            self.avr += learning_rate * self.mis_delta
        
        return self.avr


    def fit(self, x, y, method='scapal', scapal_alpha=0.01, scapal_it=1):
        x_train = x.astype(np.float64)
        x_train = (x_train - x_train.mean(axis=-1)[:, np.newaxis]) / x_train.std(axis=-1)[:, np.newaxis] 

        n_input = x.shape[1]

        if y.ndim == 2 and y.shape[1] != 1:
            yy = y.argmax(-1)
            self.n_classes = y.shape[1]
        elif y.ndim == 1 or (y.ndim == 2 and y.shape[1] == 1):
            self.n_classes = np.max(y) + 1 # just guessing
            yy = y.ravel()
        else:
            raise Exception("Wrong Dimension")

        self.prototypes = np.empty((self.n_classes, n_input), dtype=np.float)

        if method == 'scap':
            self.prototypes = self._scap(x_train, yy)
        elif method == 'scapal':
            self.prototypes = self._scapal(x_train, yy, scapal_alpha, scapal_it)
        else:
            raise Exception("Wrong method!")
        
        self.weight = np.linalg.pinv(self.prototypes)
