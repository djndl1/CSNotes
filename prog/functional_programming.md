# Basic definition

Functional programming is a programming paradigm that treats computation as the evaluation of mathematical functions and avoids changing-state and mutable data.

It is a declarative programming paradigm in that programming is done with expressions or declarations instead of statements.

Functional code is idempotent: a function's return value depends only on its arguments, so calling a function with the same value for an argument always produces the same result.

Functional programming has its origins in lambda calculus.

# Concepts

## higher-order functions

Higher-order functions are functions that can either take other functions as arguments or return them as results.

e.g. differential operator $d/dx$, which returns the derivative of a function $f$.

## first-class function

first class is a CS term that describes programming language entities that have no restriction on their, such as arguments to other functions.

## Pure functions

Pure funcitons (or expressions) have no side effects (memory or I/O).

## Recursion

Iteration/looping in functional languages is usually accomplished via recursion.

?tail recursion?

## strict versus non-strict evaluation

refer to how function arguments are processes when an expression is being evaluated.

## referential transparency

Functional programs do not have assignment statements, that is, the value of a variable in a functional program never changes once defined. This eliminates any chances of side effects because any variable can be replaced with its actual value at any point of execution. So, functional programs are referentially transparent.

## type systems

Functional programming languages have tended to use typed lambda calculus, rejecting all invalid programs at compilation t ime and risking false positive errors.

???

## data structures

???


