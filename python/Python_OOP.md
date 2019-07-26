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
