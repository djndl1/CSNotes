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
