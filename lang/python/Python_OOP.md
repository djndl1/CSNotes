## Two basic OO principles

- Composition is the act of collecting several objects together to create a new one. Aggregation is almost exactly like composition. The difference is that aggregate objects can exist independently.

- Inheritance: Programmers often use inheritance to share code between two kinds of objects that are only distantly related, with no is a relationship in sight. 

In Python, duck typing is more used as polymorphism, which doesn't care if a class inherits another or not.

# Objects in Python

PEP8 recommends that classes be named using CamelCase notation.

The one difference between methods and normal functions is that all methods have one required argument `self`, a reference to the objet that the method is being invoked on.

Python has a constructor (`__new__()`) and an initializer (`__init__()`).

Private attributes are prefixed by `__` (on which name mangling will be performed), internal attributes with `_` (still accessible).

```python
a.__plain_string
---------------------------------------------------------------------------
AttributeError                            Traceback (most recent call last)
<ipython-input-4-5f2b11c3fe5b> in <module>()
----> 1 a.__plain_string

AttributeError: 'SecretString' object has no attribute '__plain_string'
```

However, it's actually still there

```python
In [6]: a._SecretString__plain_string
Out[6]: 'abc'
```

Most Python programmers will not touch a single underscore variable without a compelling reason either. Therefore, there are very few good reasons to use a name-mangled variable in Python

# Inheritance

## `super()`

Child classes do not automatically creates attributes of the parent classes: the parents' initializers `super().__init__()` is required.
Unlike other OOP languages, method calls in Python are only resolved to the current instance's method (including inherited), 
never its ancestors' method with the same name unless `super()` is used.

> When calling super() to resolve to a parent's version of a classmethod, instance method, or staticmethod, we want to pass the current class whose scope we are in as the first argument, to indicate which parent's scope we're trying to resolve to, and as a second argument the object of interest to indicate which object we're trying to apply that scope to. 

### A Few Notes on the `super()` Call

- `super()` rely on the MRO and is used to avoid referring to the base class explicitly and statically

- `super(type, instance)` returns a proxy bound to `instance` as if the instance is cast to its superclass; 
   `super(type, type2)` returns a proxy bound to the result superclass of `type2`, which is useful 
   for static methods and class methods of a subclass.

- `super()` itself is a normal callable. Without the instance argument or the parameterless version, 
   it will not modify the current instance with `super().__init__()`. 
   There is a reason why an object instance is required before Python 3, where the parameterless version automatically passes `self`. 
   It is more than `super` in Java or `base` in C# where the keyword simply represents the current instance in the facade of the parent class where in Python, 
   `super()` returns a proxy object that performs dynamic method/attribute resolution up the hierarchy.
   
- do not use `super(self.__class__, self).__init__()`: `self` might be a subclass instance passed into the current `__init__()`, 
  no the current class's type and may cause recursion or code error if the subclass inherits the method, as the call is actually `super(subclass, subclass_instance).__init__()` in the superclass,
  the same as in the subclass
  
- The `type` parameter determines the search starting point and the second argument determines the MRO, hence the typing requirement for the two arguments.
  For single inheritance, it seems that `super().__init__()` calls the parent class's `__init__()`, which in turns calls the grantparent class's.
  For multiple inheritance, it reveals the actual mechanism: the MRO is the same with each call to `super().__init__()` as `self` is the same object,
  but the class type is different, resulting in different search starting points.
  - With a MRO like `A -> B -> C`, A's `super()` searches only `B -> C` 
    and then calls `super()` in B's `__init__()`, which searches only `C` and calls C's `__init__()`,
    this is the same for both single and multiple inheritance; it differs only in that multiple inheritance has sibling classes and their `__init__()` 
    are called by their sibling before them.
  - be careful with the order in which `super().__init__()` and other code are placed: it might cause unexpected results.
  
See [OOP Test Code](../CodeOfLanguages/python_tutorial/python_oop_test.py)

# Object Models

An object has an _identity_ `id()` and a _value_ (mutale or immutable), determined (including mutability) by its _type_.

Operations that compute new values may actually return a reference to any existing object with the same type and value, while for mutable objects this is not allowed: `1` may or may not be the same integer object.

## Type Hierarchy

- `None`: a single value/object.

- `NotImplemented`: retruned by unimplemented numeric or rich comparison methods.

- `...` or `Ellipsis`: a single object/value

- numeric types

- Various container types

- Callable: 
  - built-in functions
  - built-in methods
  - classes (as factories for new instances)
  - class instances with `__class__()`
  - user-defined functions
  - instance methods: an instance method is a function bound to a class instance, with the instance 
    as its first argument.
  - generator functions
  - coroutine function (async)
  - asynchronous generator functions

- modules
  - a module has a namespace (`module.__dict__`)

- Custom classes
  - `__dict__`: class namespace that contains class attributes (but not the only way)
  - `type.mro()`: called to create `__mro__` and may be overridden
  - `__subclasses__()`: its direct subclasses

- Class instances
  - `__class__`: its class object instance
  - `__dict__`: an object's writable attributes. Some objects have `__slots__` instead of `__dict__`

- File objects

## Special Methods

For operator overloading, constructors, initializers and finalizers.

### Object Construction

The `__new__(cls, args...)` static method is called first and creates an instance of the class,
and then the initializer `__init__(self, args...)` is called to further customize the instance (the reason 
 why it has the `self` reference to the instance). Both methods accept the same set of arguments.

Explicit calls to `__init__` and `__new__` of the superclass are necessary to create and initialize the parents data: there is the ultimate superclass' `object.__new__()` that is responsible for allocating memory for the object.

Improperly implementing `__new__` and `__init__` can result in [some weird result](https://pdarragh.github.io/blog/2017/05/22/oddities-in-pythons-new-method/)

#### Some Examples of `__new__`:

```python
# create an object without using __init__
class Person:
    def __new__(cls, name):
        instance = object.__new__(cls)
        return instance

# as a hook
class LowerCaseTuple(tuple):
   def __new__(cls, iterable):
      lower_iterable = (l.lower() for l in iterable)
      return super().__new(cls, lower_iterable)

# inherit int
class SuperInt(int):
    def __new__(cls, value):
        return super().__new__(cls, value ** 2)

# singleton
class Singleton:
    instance_ = None

    def __new__(cls):
        if cls.instance_ is None:
            cls.instance = super().__new__(cls)
        return cls.instance_
```



### Object Finalization

`__del__(self)` is called before an object is destroyed. Note that this is not the method being called 
when the `del` operator is used, which simply decrease the refcount.

Explicit calls to a superclass' `__del__` is required to properly dispose an object.

### `__bytes__`, `__str__`, `__repr__` and `__format__(self, format_spec)`

The official wording is somewhat confusing.
`repr` is mean to be unambiguous and `__str__` readable. `__repr__` for authors (debugging, logging etc.) and `__str__` for users.  `__bytes__` returns a byte string representation.
 
`str` uses `repr` if no `__str__` is defined. `__format__` generates a formatted string depending on the input specification.

### `__eq__` and `__hash__`

The purpose of a hash function is to find equivalent objects in a hashed collection.
Equal objects should return the same hash:

1. If `___eq__` is not defined then `__hash__` has no meaning and thus should be not defined (`__hash__ = None`).

2. An object with `__eq__` but not `__hash__`  cannot be hashed (implicitly set to `None` by Python) and thus cannot be used in a hashed collection

3. mutable objects should not implement `__hash__` since its hash would be mutable.

`__hash__` and `__eq__` are inherited from `object`, which uses identity comparison.

### `__bool__`

Called to implement truth values and for `bool()` to use. Otherwise `__len__()` is called. If None are defined, all instances are considered true.

## `__dict__`

Most if not all objects have a special attribute `__dict__` that stores its dynamic writable attributes (including data attributes, methods, metadata etc.). Class members and instance members are typically stored in it. Builtin types, their instances, and builtin functions and those objects with a `__slot__` attribute do not have it. The `__slots__` attribute is intended to be a memory-efficient, immutable alternative to `__dict__`. Types with a `__dict__` supports dynamic addition and deletion of an attribute by assigning or `del` the attribute directly.

using `vars()` for introspection and debugging is more Pythonic than using the `__dict__` attribute directly.

### What about `setattr`, `getattr`, `delattr`, `hasattr` 

These are generally the better tools to manipulate attributes.
