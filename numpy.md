## numpy.array
`numpy.array(object, dtype=None, copy=True, order='K', subok=False, ndmin=0)`

Create an array

- `object`: any object exposing the array interface, an object whose `__array__` method returns an array

- `dtype`: if not given, determined as the minimum type required to hold the objects

- `copy`: if the object is copies.

- `subok`: if pass-through the subclasses

- `ndmin`: specifies the minimum number of dimensions that the output array should have

_returns_: the resulting array

## Class `numpy.ndarray`

`numpy.ndarray(shape, dtype=float, buffer=None, offset=0, strides=None, order=None)`

Multidimensional, homogeneous array of fixed-size items. Arrays should not be constructed using the ctor here.

important attributes

- `T`: transpose

- `dtype`: data type

- `shape`: array dimensions

- `size`: number of elements

- `ndim`: number of dimensions

- `flat`: 1-D iterator over the array

- `real`/`imag`: real/imaginary part of the array

important methods and corresponding functions

- `ndarray.transpose(*axes)`: 


