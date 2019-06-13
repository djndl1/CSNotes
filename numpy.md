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

- `ndarray.fill(value)`: Fill the array with a scalar value.

- `ndarray.flatten(order='C')`: Return a copy of the array collapsed into one dimension.

- `ndarray.transpose(*axes)`: returns a view of the array with the speficied reordered axes

- `ndarray.conj()`: 

- `ndarray.conjugate()`: Return the complex conjugate, element-wise.

```python
ndarray.conj().T
ndarray.conjugate().T
np.conj is np.conjugate
```
These two are the same function.

should be the alternative way to obtain the conjugate transpose

- `ndarray.trace(offset=0, axis1=0, axis2=1, dtype=None, out=None)`; `numpy.trace(a, offset=0, axis1=0, axis2=1, dtype=None, out=None)`: return the trace of an array

- `ndarray.nonzero()`; `numpy.nonzero(a)`: Return the indices of the elements that are non-zero.

```python
>>> a = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
>>> a > 3
array([[False, False, False],
       [ True,  True,  True],
       [ True,  True,  True]])
>>> np.nonzero(a > 3)
(array([1, 1, 1, 2, 2, 2]), array([0, 1, 2, 0, 1, 2]))
```

- `sum()`, `mean`, `std` (standard deviation), `var` (variance), `cumprod()`, `cumsum()`, `dot()`; `max()`; `min()`; `prod()`; `ptp()` (peak-to-peak value); 



- `ndarray.all(axis=None, out=None, keepdims=<no value>)`; `numpy.all(array, axis=None, out=None, keepdims=<no value>)`: test whether all array elements along a given axis evaluate to True

> axis: along which a logical ANDreduction is performaed, may be nagative, counting from the last axis. can be a tuple of multiple axes.

> out: output

> keepdims: If this is set to True, the axes which are reduced are left in the result as dimensions with size one. With this option, the result will broadcast correctly against the input array.

> _returns_: boolean

```python
a = np.array([[1,2],
             [3,4]])
np.all(a, axis=0, keepdims=True)
np.all(a, axis=0, keepdims=False)
```
Result
```python
array([[ True,  True]])
array([ True,  True])
```

- `ndarray.any(axis=None, out=None, keepdims=False)`; `numpy.any(a, axis=None, out=None, keepdims=<no value>)`: Test whether any array element along a given axis evaluates to True. Refer to `all()` above

- `ndarray.argmax(axis=None, out=None)`; `numpy.argmax(a, axis=None, out=None)`: Returns the indices of the maximum values along an axis.

- `ndarray.argmin(axis=None, out=None)`; `numpy.argmin(a, axis=None, out=None)`: see above

- `ndarray.partition(kth, axis=-1, kind='introselect', order=None)`; `numpy.partition(a, kth, axis=-1, kind='introselect', order=None)`: partition the array using the `kth` element in a sorted array as the delimiter

- `ndarray.argpartition(kth, axis=-1, kind='introselect', order=None)`; `numpy.argpartition(a, kth, axis=-1, kind='introselect', order=None)`

```python
b = np.array([5,76,1,5,2,3])
np.sort(b)
np.partition(b,1)
b[np.argpartition(b, 1)]
```
result 
```python
array([ 1,  2,  3,  5,  5, 76])
array([ 1,  2,  5,  5, 76,  3])
array([ 1,  2,  5,  5, 76,  3])
```

- `ndarray.sort(axis=-1, kind='quicksort', order=None)`; `numpy.sort(a, axis=-1, kind='quicksort', order=None)`; Return a sorted copy of an array. 

- `ndarray.sort(axis=-1, kind='quicksort', order=None)`; `numpy.argsort(a, axis=-1, kind='quicksort', order=None)`; 

- `ndarray.choose(choices, out=None, mode='raise')`; `numpy.choose(a, choices, out=None, mode='raise')`: Construct an array from an index array and a set of arrays to choose from. ???? confusing method

- `numpy.clip(a, a_min, a_max, out=None)`; Clip (limit) the values in an array.

- ``
