# OOP design

An object is a collection of dat and associated behaviors.

- OO analysis -> identifies the objects and interactions between those objects;

- OO design -> converting the resulting requirements of OOA into an implementation specification.

- OO programming -> converting the design into a working program.

Most twenty-first century development happens in an iterative development model. In iterative development, a small part of the task is modeled, designed, and programmed, then the program is reviewed and expanded to improve each feature and include new features in a series of short development cycles.

Objects: attributes, behaviors

Objects are an abstraction of a real concept. Abstraction means dealing with the level of detail that is most appropriate to a given task. Abstrction is the process of encapsulating information with separate public and private interfaces.

## Two basic OO principles

- Composition is the act of collecting several objects together to create a new one. Aggregation is almost exactly like composition. The difference is that aggregate objects can exist independently.

- Inheritance: Programmers often use inheritance to share code between two kinds of objects that are only distantly related, with no is a relationship in sight. 

In Python, duck typing is more used as polymorphism, which doesn't care if a class inherits another or not.

# Objects in Python

PEP8 recommends that classes be named using CamelCase notation.

The one difference between methods and normal functions is that all methods have one required argument `self`, a reference to the objet that the method is being invoked on.

Python has a constructor (`__new__()`) and an initializer (`__init__()`).

Besides not cluttering up our namespace, `import`  instead of `from aaa import *` at the beginning of the file is more clear on what is available.

A module is just a file. A __package__ is a collection of modules in a folder, where `__init__.py` is placed. The name of the package is the name of the folder. Absolute imports specify the complete path to the module, function, or path, which will work from any module. Relative imports are basically a way of saying find class, function, or module as it is positioned relative to the current module. Suppose we have the following package hierarchy:

```bash
parent_directory/
    main.py
    ecommerce/
        __init__.py
        database.py
        products.py
        payments/
            __init__.py
            square.py
            stripe.py
```

```python
from .database import Database   # products.py imports database.py
from ..database import Dataqbase # from the payments folder
```

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

- `super()` is used to avoid referring to the base class explicitly and statically and rely on the MRO.

- `super(type, instance)` returns a method bound to `instance`

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
