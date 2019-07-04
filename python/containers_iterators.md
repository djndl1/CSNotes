
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

