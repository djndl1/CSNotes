# -*- coding: utf-8 -*-

import mxnet as mx
from mxnet import gluon, nd

import SNN_base

class evolve_orders(gluon.HybridBlock):
    def __init__(self, **kwargs):
        super(evolve_orders, self).__init__(**kwargs)

    def forward(self, F, order_params):
        D = 2.0 * F.sum(F.square(order_params))
        gamma = 1.0 / (D + 1e-8)
        order_params_new = gamma * (1.0 - F.square(order_params)) * order_params + order_params

        return order_params_new

class generate_orders(gluon.HybridBlock):
    def __init__(self, weight, **kwargs):
        super(generate_orders, self).__init__(**kwargs)
        self.weight = self.params.get_constant('adjoined', nd.array(weight))

    def forward(self, F, X):
        x_scaled = X

        x_mean = F.mean(x_scaled, axis=-1, keepdims=True)
        x_std = F.sqrt(F.mean(F.square(x_scaled - x_mean), axis=-1, keepdims=True))
        x_scaled = (x_scaled - x_mean) / (x_std + 1e-8)
        
        order_params = F.dot(x_scaled, self.weight)

        return order_params

class mxSNN(SNN_base.SNN_base):
    def __call__(self, X):
        """
        mxnet ndarray expected
        """

        order_params = generate_orders(self._weight_numpy)(X)

        for _ in range(40):
            order_params = evolve_orders()(order_params)

        return order_params




