
# Iterator 

Python supports a concept of iteration over containers. A container may supports different types of iteration, additional methods can be provided to specifically requiest iterators for those iteration types (e.g. tree iteration).

The container needs to implement its `container.__iter__()` interface, which returns an iterator object.

The iterator are required to support the so-called _iterator protocol_:

- `iterator.__iter__()`: returns the iterator object itself. Used in for and in statements.

- `iterator.__next__()`: returns the next item from the container. If no more items in the container, raise the `StopIteration` exception.
