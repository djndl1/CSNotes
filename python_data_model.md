# Python Data Model

## Objects, types and values

Every object has an identity, a _type_ and a _value_.

`is`: compares the identity of two objects

`id()`: returns an integer representing its identity (memory address)

Objects are never explicitly destroyed. Instead, they may be garbage-collected

CPython currently uses a reference-counting scheme with (optional) delayed detection of cyclically linked garbage, which collects most objects as soon as they become unreachable, but is not guaranteed to collect garbage containing circular references.

Some objects contain references to other objects; these are called containers. Examples of containers are tuples, lists and dictionaries. The references are part of a container’s value.

### Type

determins the operations that the object supports and defines the possible valus for objects of that type.

`type()`: returns an object's type.

### value 

mutable and immutable.



## The standard type hierarchy

### None

signifies the absence of a value in many situations.

### NotImplemented

### Ellipsis

`...` or the built-in name `Ellipsis`

### numbers.Number

Immutable

#### numbers.Integral

##### Integer(`int`)

represent numbers in an unlimited range.

##### Booleans(`bool`)

A subtype of the integer type

#### numbers.Real(`float`)

machine-level double precision floating point numbers. Python does not uspport single-precision floating point numbers.

#### numbers.Complex(`complex`)

### Sequences

finite ordered sets indexed by non-negative numbers.

#### Immutable sequences

cannot change once it is created. The objects referenced by an immutable object may change, but the collecton of objects directly referenced by an immutable object cannt change.

##### Strings

a sequence of values that represent __Unicode__ code points. Python does not have a `char` type. Every code point in the string is represented as a string object with length 1.

##### Tuples

contain arbitrary Python objects.

##### Bytes

8-bit bytes

#### Mutable sequences

can be changed after created. The subscription and slicing notations can be used as the target of assignment and `del` statements.

##### Lists

contain arbitrary Python objects

##### Byte Arrays

created by `bytearray()` constructor. Provides the same interface and functionality as immutable `bytes` objects.

### Set types

unordered, finite sets of unique immutable objects. Cannot be indexed but iteratable.

#### Sets
mutable set; `set()`

#### Frozen sets
immutable set. `frozenset()`

### Mappings

finite sets of objects indexed by arbitrary index sets.

#### Dictionaries

indexed by values other than objects containing lists or dictionaries or other mutable types.

### Callable types

#### User-defined functions

created by a function definition.

#### Instance methods

#### Generator funcitons, Coroutine, built-in ...

### Modules

### Custom Classes

created by class definitions.

### Class instances

### I/O objects (file objects)

### Internal types

used internally by the interpreter are exposed to the user.

#### Code objects

byte-compiled executable Python code, or bytecode.

#### Frame objects

execution frames

#### Traceback objects

a stack trace of an exception.

## Special method names

A class can implement certain operations that are invoked by special syntax (such as arithmetic operations or subscripting and slicing) by defining methods with special names. This is Python’s approach to _operator overloading_.

### Basic customization

- `object.__new__(cls[,...])`: a static method; called to create a new instance of class `cls`. 

- `object.__init__(self[, ...])`: Called after the instance has been created (by __new__()), but before it is returned to the caller. The arguments are those passed to the class constructor expression. the derived class’s __init__() method, if any, must explicitly call it to ensure proper initialization of the base class part of the instance

- `object.__del__(self)`: Called when the instance is about to be destroyed. This is also called a finalizer or (improperly) a destructor. If a base class has a __del__() method, the derived class’s __del__() method, if any, must explicitly call it to ensure proper deletion of the base class part of the instance.

- `object.__repr__(self)`: called by the `repr()` built-in function to compute the "official" string representation of an object. Typically used for debugging.

- `object.__str__(self)`: Called by str(object) and the built-in functions format() and print() to compute the “informal” or nicely printable string representation of an object.

- ```python
object.__lt__(self, other)
object.__le__(self, other)
object.__eq__(self, other)
object.__ne__(self, other)
object.__gt__(self, other)
object.__ge__(self, other)
```

rich comparison mthods, corresponding to operator symbols.

`object.__bool__(self)`: Called to implement truth value testing and the built-in operation bool(); should return False or True.



## Coroutines

TODO

