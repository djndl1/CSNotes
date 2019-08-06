# # Built-in Function

- `all(iterable)`: true if all elements of the iterable are true, or if the iterable is empty.

- `any(iterable)`: true if any element of the iterable is true.

- `ascii(object)`: return a string containing printable representation of an object but escape the non-ASCII characters.

```python
In [28]: ascii('你好')
Out[28]: "'\\u4f60\\u597d'"
```

- `bin(x)`: conert an integer number to a binary string with `0b`.

- (3.7) `breakpoint(*args, **kws)`: drop into the debugger at the call site

- `callable(object)`: true if the object has `__call__()` method.


# Built-in types

## sequences

Sequences support some common operations.

- `x (not) in seq`: tests if `x` is in `seq`.

- `s + t`: concatenation

- `s * n`/`n * s`: they are not copied but referenced `n` times.

```python
In [32]: a*3
Out[32]: [1, 23, 2, 4, 5, 'a', 1, 23, 2, 4, 5, 'a', 1, 23, 2, 4, 5, 'a']
```

```python
>>> lists = [[]] * 3
>>> lists
[[], [], []]
>>> lists[0].append(3)
>>> lists
[[3], [3], [3]]
```

- slicing

- `len(s)`; `min(s)`; `max(s)`

- `s.index(x[, i[, j]])`: inex of the first occurrence of x in s at or after index `i` and before index `j`.

- `s.count(x)`: total number of occurrences of `x` in `s`
