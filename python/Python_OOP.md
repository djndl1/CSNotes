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
