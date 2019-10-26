
# Iterator 

Python supports a concept of iteration over containers. A container may supports different types of iteration, additional methods can be provided to specifically requiest iterators for those iteration types (e.g. tree iteration).

The container needs to implement its `container.__iter__()` interface, which returns an iterator object.

The iterator are required to support the so-called _iterator protocol_:

- `iterator.__iter__()`: returns the iterator object itself. Used in for and in statements.

- `iterator.__next__()`: returns the next item from the container. If no more items in the container, raise the `StopIteration` exception.

An example

```python
class Reverse:
    '''Iterator for looping over a sequence backwards'''
    def __init__(self, data):
        self.data = data
        self.index = len(data)

    def __iter__(self):
        return self

    def __next__(self):
        if self.index == 0:
            raise StopIteration

self.index = self.index - 1
        return self.data[self.index]
```

a Fibonacci iterator

```python
class Fib:
    '''iterator that yields numbers in the Fibonacci sequence'''

    def __init__(self, max):
        self.max = max

    def __iter__(self):
        self.a = 0
        self.b = 1
        return self

    def  __next__(self):
        fib = self.a
        if fib > self.max:
            raise StopIteration
        self.a, self.b = self.b, self.a + self.b
        return fib
```

`iter(object [,sentinel])` returns an iterator object. If `sentine` is absent, `object` must support the iteration protocol or the sequence protocol. If `sentinel` is givne, the `object` must be callable.

See [Iterators on PEP](https://www.python.org/dev/peps/pep-0234/) and 
https://www.python.org/dev/peps/pep-0255/

## important functions about iterators

`zip`: make an iterator taht aggregates elements from each of the iterables.

```python
>>> list(zip(range(0, 3), range(10, 13)))
[(0, 10), (1, 11), (2, 12)]
```

# Generator function and generator iterators

Generators are a simple and powerful tool for creating iterators. They are written like regular functions but use the `yield` statement whenever they want to return data. 

```python
    def reverse(data):
        for index in range(len(data)-1, -1, -1):
            yield data[index]
```

Generators provide a convenient way to implement the iterator protocol. If a container object's `__iter__()` method is implemented as a generator, it will automatically return an iterator object supplying the `__iter__()` and `__next__()` methods.

`
Some simple generators can be coded succinctly as expressions using a syntax similar to list comprehensions but with parentheses instead of square brackets. Generator expressions are more compact but less versatile than full generator definitions and tend to be more memory friendly than equivalent list comprehensions.

```python
sum(i*i for i in range(10))
```

# `itertools`: functions creating iterators for efficient looping

This module implements a number of iterator building blocks inspired by. It standardizes a core set of fast, memory effcient tools that are useful by themselves or in combination.

## Combinatoric iterators

`product()`: Cartesian product

`combination()`; `permutations()`

## others

`groupby()`: groups an iterable by a function and works only when already sorted by the function.

`chain()`: returns an iterator that chains input iterables together.

`zip_longest()`: similar to `zip`, but stops at the longest sequence, inserting `None` if necessary.

