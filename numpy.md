## numpy.array
`numpy.array(object, dtype=None, copy=True, order='K', subok=False, ndmin=0)`

Create an array

- `object`: any object exposing the array interface, an object whose `__array__` method returns an array

- `dtype`: if not given, determined as the minimum type required to hold the objects

- `copy`: if the object is copies.

- `subok`: if pass-through the subclasses, otherwise the returned array will be forced to be a base-class array

- `ndmin`: specifies the minimum number of dimensions that the output array should have

_returns_: the resulting array

## Class `numpy.ndarray`

`numpy.ndarray(shape, dtype=float, buffer=None, offset=0, strides=None, order=None****

**container** object

Multidimensional, homogeneous array of fixed-size items. Arrays should not be constructed using the ctor here.

Different ndarrays can share the same data, so that changes made in one ndarray may be visible in another. That is, an ndarray can be a “view” to another ndarray, and the data it is referring to is taken care of by the “base” ndarray.

Generally, accessing an array through its attributes allows you to get and sometimes set intrinsic properties of the array without creating a new array. The exposed attributes are the core parts of an array and only some of them can be reset meaningfully without creating a new array. 

```python
>>> c = np.array([[1+1j, 2],
             [3, 4-1j]], dtype=np.complex)
>>> c.shape = [c.size]
>>> c
array([1.+1.j, 2.+0.j, 3.+0.j, 4.-1.j])
```

### Internal memory layout

A contiguous one-dimension segment of computer memory, combining with an _indexing scheme_, interpreted by the data-type object associated with the array.
Given the index of an element of an array, say $(n_0, n_1, \dots, n_{N-1})$ and the strides of the array $(s_0, s_1, \dots, s_{N-1}), i.e. the bytes between two elements along a certain axis, the offset from the beginning of the array is 

$$
n_{\text{offset}} = \sum^{N-1}_{k=0} s_k n_k
$$

Different stride scheme like fortran's column-major or C's row-major order exist. However, if `self.shape[k] == 1`, i.e. `self` and `self.squeeze()` always have the same contiguity and aligned flags value.

- `numpy.ndarray.itemsize`: length of one array element in bytes

- `ndarray.flags`: Information about the memory layout of the array.

- `ndarray.strides`: tuple of strides

- `ndarray.data`: buffer object pointing to the start of the array's data

- `ndarray.nbytes`: total bytes consumed by the elements of the array

### important attributes

- `T`: transpose

- `dtype`: data type

- `shape`: array dimensions

- `size`: number of elements

- `ndim`: number of dimensions

- `flat`: 1-D iterator over the array

- `real`/`imag`: real/imaginary part of the array

### important methods and related functions

- `ndarray.item(*args)`: Copy an element of an array to a standard Python scalar and return it. Note that slicing returns an ndarray, even if it is a scalar.

- `ndarray.fill(value)`: Fill the array with a scalar value.

- `ndarray.flatten(order='C')`: Return a copy of the array collapsed into one dimension.

- `ndarray.ravel([order])`; `numpy.ravel(a, order='C')`: Return a contiguous flattened array. A copy is made only if needed

- `ndarray.squeeze(axis=None)`; `numpy.squeeze(a, axis=None)`: Remove single-dimensional entries from the shape of an array.

```python
>>> x = np.array([[[0], [1], [2]]])
>>> x.shape
(1, 3, 1)
>>> np.squeeze(x).shape
(3,)
>>> np.squeeze(x, axis=0).shape
(3, 1)
```

- `ndarray.resize(new_shape, refcheck=True)`; Change shape and size of array in-place.

```python
>>> b = np.array([[0, 1], [2, 3]])
>>> b.resize(2, 3) # new_shape parameter doesn't have to be a tuple
>>> b
array([[0, 1, 2],
       [3, 0, 0]])
```

Note that this is different from `numpy.resize()`, which repeats copies of the array operated on.

```python
>>> a=np.array([[0,1],[2,3]])
>>> np.resize(a,(2,3))
array([[0, 1, 2],
       [3, 0, 1]])
```

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

- `sum()`, `mean`, `std` (standard deviation), `var` (variance), `cumprod()`, `cumsum()`, `dot()`; `max()`; `min()`; `prod()`; `ptp()` (peak-to-peak value); `.round()`; `numpy.around()`



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

```python
>>> choices = [[0, 1, 2, 3], [10, 11, 12, 13],
...   [20, 21, 22, 23], [30, 31, 32, 33]]
>>> np.choose([2, 3, 1, 0], choices
... # the first element of the result will be the first element of the
... # third (2+1) "array" in choices, namely, 20; the second element
... # will be the second element of the fourth (3+1) choice array, i.e.,
... # 31, etc.
... )
array([20, 31, 12,  3])
>>> np.choose([2, 4, 1, 0], choices, mode='clip') # 4 goes to 3 (4-1)
array([20, 31, 12,  3])
>>> # because there are 4 choice arrays
>>> np.choose([2, 4, 1, 0], choices, mode='wrap') # 4 goes to (4 mod 4)
array([20,  1, 12,  3])
```


- `numpy.clip(a, a_min, a_max, out=None)`; Clip (limit) the values in an array.

- `ndarray.take(indices, axis=None, out=None, mode='raise')`; `numpy.take(a, indices, axis=None, out=None, mode='raise')`: Take elements from an array along an axis.

- `ndarray.put(indices, values, mode='raise')`; `numpy.put(a, ind, v, mode='raise')`: Replaces specified elements of an array with given values.

- `ndarray.compress(condition, axis=None, out=None)`; `numpy.compress(condition, a, axis=None, out=None)`: Return selected slices of an array along given axis.

```python
>>> np.compress([False, True, True], a, axis=0)
array([[3, 4],
       [5, 6]])
```

- `ndarray.repeat(repeats, axis=None)`; `numpy.repeat(a, repeats, axis=None)`: Repeat elements of an array.

```python
>>> x = np.array([[1,2],[3,4]])
>>> np.repeat(x, 3, axis=1)
array([[1, 1, 1, 2, 2, 2],
       [3, 3, 3, 4, 4, 4]])
```

### Arithmetic, matrix multiplication and comparison operations

Arithmetic and comparison operations on ndarrays are defined as element-wise operations, and generally yield ndarray objects as results.

Each of the arithmetic operations (`+`, `-`, `*`, `/`, `//`, `%`, `divmod()`, `**` or `pow()`, `<<`, `>>`, `&`, `^`, `|`, `~`) and the comparisons (`==`, `<`, `>`, `<=`, `>=`, `!=`) is equivalent to the corresponding universal function (or ufunc for short) in NumPy.
