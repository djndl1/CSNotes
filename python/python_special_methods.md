# Basics

- `x = MyClass()`: `x.__init__()`, called after the instance is created

- `repr(x)`: `x.__repr__()`

- `str(x)`: `x.__str__()`

- `bytes(x)`: `x.__bytes__()`

- `format(x, format_spec)`: `x.__format__(format_spec)`

# Iterators

- `iter(seq)`: `seq.__iter__()`

- `next(seq)`: `seq.__next__()`

- `reversed(seq)`: `seq.__reversed__()`

# Computed Attributes

- `x.my_property`: `x.__getattribute__('my_property')` (unconditionally, always called even the attribute is set explicitly otherwise), `x.__getattr__('my_property')` (fallback)

- `x.my_property = value`: `x.__setattr__('my_property', value)`

- `del x.my_property`: `x.__delattr__('my_property')`

- `dir(x)`: `x.__dir__()`

# Classes that act like functions

- `my_instance()`: `my_instance.__call__()`, used in functions requiring states within.

# Sequence

- `len(seq)`: `seq.__len__()`

- `x in seq`: `seq.__contains__(x)`
