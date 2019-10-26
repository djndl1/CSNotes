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

- `s + t`: concatenation. Concatenating immutable sequences always results in a new object.

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

- slicing `seq[i:j:k]`: empty if `i >= j`; if `i` or `j` > `len(s)`, use `len(s)`.  When `k` is negative, `i` and `j` are reduced to `len(s) - 1` if they are greater than `len(s)`.

- `len(s)`; `min(s)`; `max(s)`

- `s.index(x[, i[, j]])`: inex of the first occurrence of x in s at or after index `i` and before index `j`.

- `s.count(x)`: total number of occurrences of `x` in `s`

## Immutable and Mutable Sequence Types

Immutables support `hash()` so that they can be used as `dict` keys.

Mutables accepts the following operations:

- `del s[i:j]`: `s[i:j] = 0`

- slicing assignment `s[i:j] = t`

- `s.append()`; `s.copy()`;  `s.insert()`; 

- `s.pop()`; `s.remove()`; `s.clear()` (`del s[:]`);

- `s.reverse()`

- `s.extend(t)`; `s += t`; `s * n`;

### class `list([iterable])`

Lists are mutable sequences, typically used to store collections of homogeneous items. 

- `.sort()`: using `<` comparisons

### class `tuple([iterable])`

Tuples are immutable sequences, typically used to store collections of heterogeneous data. It is actually the comma not the parentheses that makes a tuple.

### `class range()`

an immutable sequences of numbers. The advantage of the range type over a regular list or tuple is that a range object will always take the same (small) amount of memory, no matter the size of the range it represents (as it only stores the start, stop and step values, calculating individual items and subranges as needed).


## Text Sequence Type -`str`

Strings are immutable sequences of Unicode code points. There is also no mutable string type, but `str.join()` or `io.StringIO` can be used to efficiently construct strings from multiple fragments.

If neither encoding nor errors is given, `str(object)` returns `object.__str__()`, which is the “informal” or nicely printable string representation of object. If `object` has no `__str__()`, it returns `repr(object)`.
