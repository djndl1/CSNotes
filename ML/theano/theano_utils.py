#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import theano.tensor as T, theano as th
import numpy as np


def shared_glorot_uniform(shape, dtype=th.config.floatX, name='', n=None):
    if isinstance(shape, int):
        high = np.sqrt(6.0 / shape)
    else:
        high = np.sqrt(6.0 / (np.sum(shape[:2]) * np.prod(shape[2:])))
    shape = shape if n is None else (n,) + shape

    return th.shared(np.asarray(
        np.random.uniform(low=-high, high=high, size=shape),
        dtype=dtype),
                     name=name)


def shared_zeros(shape, dtype=th.config.floatX, name='', n=None):
    shape = shape if n is None else (n,) + shape
    return th.shared(np.zeros(shape, dtype=dtype), name=name)

class conv_layer:

    idx=0

    def __init__(self, input, input_shape=None, 
                 filter_shape=None, n_filters=1, border_mode='valid',
                activation=T.nnet.relu, name=None):
        
        if not name:
            W_name = 'Weight_' + str(conv_layer.idx) + '_conv'
            b_name = 'bias_' + str(conv_layer.idx) + '_conv'
            layer_name = 'conv' + '_' + str(conv_layer.idx) + '_conv'
            conv_layer.idx += 1
        else:
            W_name = 'Weight_' + name
            b_name = 'bias_' + name
            layer_name = name
    
        n_channels = input_shape[1]
        self._filter_shape = [n_filters, n_channels] + list(filter_shape)

        self.W = shared_glorot_uniform(self._filter_shape, name=W_name)
        self.b = th.shared(np.asarray(np.random.normal(size=[n_filters, 1, 1]),
                                      dtype=th.config.floatX),
                           name=name,
                          broadcastable=(False, True, True))
        self._input_tensor = input
        self._border_mode = border_mode
        self._input_shape = input_shape
        self._activation = activation
        
    def __call__(self):
        conv_out = th.tensor.nnet.conv2d(input=self._input_tensor,
                                           filters=self.W,
                                           input_shape=self._input_shape,
                                           filter_shape=self._filter_shape,
                                           border_mode=self._border_mode)
        out = self._activation(conv_out + self.b.dimshuffle('x', 0, 'x', 'x'))
        return out
    
    def weights(self):
        return self.W, self.b
    
    @property
    def name(self):
        return self.layer_name


class fc_layer:
    idx = 0
    
    def __init__(self, input, input_length, output_length, 
                 name=None, activation=T.tanh):
              
        if not name:
            W_name = 'Weight_' + str(fc_layer.idx) + '_fc'
            b_name = 'bias_' + str(fc_layer.idx) + '_fc'
            layer_name = 'fc' + '_' + str(fc_layer.idx) + '_fc'
            fc_layer.idx += 1
        else:
            W_name = 'Weight_' + name
            b_name = 'bias_' + name
            layer_name = name

        
        self.W = shared_glorot_uniform((input_length, output_length), 
                                  name=W_name)
        self.b = shared_glorot_uniform((output_length, ), name=b_name)
        self._input = input
        self._activation = activation
        
    def __call__(self):
        out = self._activation(T.dot(self._input, self.W) + self.b)
        return out
    
    def weights(self):
        return [self.W, self.b]
    
    @property
    def name(self):
        return self.layer_name


def dropout_layer(input, dropout_rate, size):
    import time
    random_seed = int(time.time())
    rng = np.random.RandomState(random_seed)
    srng = T.shared_randomstreams.RandomStreams(rng.randint(999999))

    if dropout_rate > 0:
        mask = srng.binomial(n=1, p=1-dropout_rate, size=size)
        return input * T.cast(mask, th.config.floatX)
    else:
        return input


def clip_norms(grads, clipnorm):
    norm = T.sqrt(sum([T.sum(g**2) for g in grads])) # L2 norm 
    return [T.switch(T.ge(norm, clipnorm), g*clipnorm / norm, g) for g in grads]


def momentum_SGD_update(params, g_params, learning_rate, alpha,
                        nesterov=False):
    
    momentum_sgd = []
    for param, Dparam in zip(params, g_params):
        delta_w = th.shared(param.get_value() * 0.0)
        delta_w_update = delta_w * alpha - learning_rate * Dparam

        if nesterov:
            param_update = param + delta_w_update*alpha - learning_rate*Dparam
        else:
            param_update = param + delta_w_update

        momentum_sgd.append((delta_w, delta_w_update))
        momentum_sgd.append((param, param_update))

    return momentum_sgd


def RMSProp_update(params, g_params, learning_rate, forgetting_factor=0.9):
    '''Root Mean Square Propagation: an adaptive SGD

    divide the learning rate for a weight by a running average of the
    mangitude of recent gradents for that weight
    '''

    if forgetting_factor > 1 or forgetting_factor < 0:
        raise Exception("Forgetting factor should be between 0 and 1")

    rmsprop_updates = []

    for param, gparam in zip(params, g_params):
        running_avr = th.shared(param.get_value() * 0.0)
        running_avr_update = running_avr*forgetting_factor + (1-forgetting_factor)*(gparam)**2

        rmsprop_updates.append((running_avr, running_avr_update))
        param_update = param - learning_rate/T.sqrt(running_avr_update + 1e-6) * gparam

        rmsprop_updates.append((param, param_update))

    return rmsprop_updates


def Adam_update(params, g_params, learning_rate, forget_first=0.9, forget_second=0.999):
    '''Adaptive Moment Estimation: an update to the RMSProp optimizer

    Running averages of both the gradients and the second moments
    of the gradients are used.
    '''
    adam_updates = []

    if forget_first > 1 or forget_first < 0 or \
       forget_second > 1 or forget_second < 0:
        raise Exception("Forgetting factor should be between 0 and 1")

    t = th.shared(np.asarray(1.0, dtype=th.config.floatX))
    for param, gparam in zip(params, g_params):
        m = th.shared(param.get_value() * 0.0)
        v = th.shared(param.get_value() * 0.0)

        m_update = forget_first*m + (1.-forget_first)*gparam
        v_update = forget_second*v + (1.-forget_second)*gparam**2
        adam_updates.append((m, m_update))
        adam_updates.append((v, v_update))

        m_t = m_update / (1 - forget_first**(t))
        v_t = v_update / (1 - forget_second**(t))
        param_update = param - learning_rate * m_t / (T.sqrt(v_t) + 1e-6)
        adam_updates.append((param, param_update))

    adam_updates.append((t, t+1))
    return adam_updates


def Adagra_update(params, g_params, learning_rate):
    '''Adaptive Gradient Algorithm'''

    adagra_updates = []

    for param, gparam in zip(params, g_params):
        G = th.shared(param.get_value() * 0.0)
        G_update = G + gparam ** 2
        adagra_updates.append((G, G_update))

        param_update = param - learning_rate /T.sqrt(G_update+1e-6) * gparam
        adagra_updates.append((param, param_update))

    return adagra_updates

def SGD(params, g_params, learning_rate):
    sgd_updates = []

    for param, gparam in zip(params, g_params):
        param_update = param - learning_rate * gparam
        sgd_updates.append((param, param_update))

    return sgd_updates

def zero_one_loss(prob, target):
    return T.sum(T.neq(T.argmax(prob), target))

def neg_MAL(prob, target):
    return -T.sum(T.log(prob)[T.arange(y.shape[0], y)])