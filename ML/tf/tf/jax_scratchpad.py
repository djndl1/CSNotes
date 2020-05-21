#-*- coding: utf-8 -*-

import jax
import jax.numpy as np

import numpy as onp

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))


def net(params, x):
    """
        Two layer MLP
    """
    w1, b1, w2, b2 = params
    hidden = np.tanh(np.dot(w1, x) + b1)
    return sigmoid(np.dot(w2, hidden) + b2)

def xent_loss(params, x, y):
    out = net(params, x)
    xent = -y * np.log(out) - (1 - y) * np.log(1-out)

    return xent


def test_all_inputs(inputs, params):
    predictions = [int(net(params, inp) > 0.5) for inp in inputs]
    for inp, out in zip(inputs, predictions):
        print(inp, '->', out)

    return (predictions == [onp.bitwise_xor(*inp) for inp in inputs])

def initial_params():
    return [
        onp.random.randn(3, 2),
        onp.random.randn(3),
        onp.random.randn(3),
        onp.random.randn()
    ]