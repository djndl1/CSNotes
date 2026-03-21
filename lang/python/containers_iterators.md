
# Iterator  (PEP 234, since 2.2)

Iterator provides an interface to control the behavior of `for` loops.

Python supports the concept of iteration over containers. A container may supports different types of iteration, additional methods can be provided to specifically requiest iterators for those iteration types (e.g. tree iteration).

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

# Generator function and generator iterators (PEP 255, since 2.2)

A generator is a function that maintains its local state between calls so that the function
can be resumed again right where it left off.

> When a generator function is called, the actual arguments are bound to function-local formal argument names in the usual way, but no code in the body of the function is executed. Instead a generator-iterator object is returned; this conforms to the iterator protocol, so in particular can be used in for-loops in a natural way. 
> Each time the .next() method of a generator-iterator is invoked, the code in the body of the generator-function is executed until a yield or return statement (see below) is encountered, or until the end of the body is reached.

A `yield` is not allowd in the `try` part of a `try-finally` clause as there is no guarantee that the generator will be resumed so that `finally` could be executed. A consequence is that generators should allocate critical resources with great care.

> If an unhandled exception– including, but not limited to, StopIteration –is raised by, or passes through, a generator function, then the exception is passed on to the caller in the usual way, and subsequent attempts to resume the generator function raise StopIteration. In other words, an unhandled exception terminates a generator’s useful life.

Generators are a simple and powerful tool for creating iterators. They are written like regular functions but use the `yield` statement whenever they want to return data. 

```python
    def reverse(data):
        for index in range(len(data)-1, -1, -1):
            yield data[index]
```

Generators provide a convenient way to implement the iterator protocol. If a container object's `__iter__()` method is implemented as a generator, it will automatically return an iterator object supplying the `__iter__()` and `__next__()` methods.

Some simple generators can be coded succinctly as expressions using a syntax similar to list comprehensions but with parentheses instead of square brackets. Generator expressions are more compact but less versatile than full generator definitions and tend to be more memory friendly than equivalent list comprehensions.

```python
sum(i*i for i in range(10))
```

