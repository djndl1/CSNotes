# Unexpected Python Features

- `else`  of `for` and `while`: executed after the loop reaches its final iteration without a `break`.

## `match` Pattern Match Statement(PEP 636, since Python 3.10)

The `case` keyword is always required.

```python
match expr:
    case value1: # literal patterns
        ...
    case value2 | value2 | value3: # or pattern
        ...
    case (0, y): # unpack assignment
        do_something_about_y
    # class type pattern supports built-in types like str() and int()
    case Point(x=x, y=y): # if expr produces a Point, this binds Point's attributes to x and y
        ...
    case Point(x, y): # or positional arguments
        ...
    case [Point(x, y)]: # nested pattern, a sequence pattern: the expression produces a list containing one Point
        ...
    case Point(x, y) if x == y: # add some constraints with if `guard`
        ... 
    case [x, y, *rest]: # matches a sequence of at least two elements without binding the rest to multiple variables; rest is a sequence
        ...
    case { "A": x, "B": y, **rest }: # matches a dictionary with `rest` for additional kv pairs.
        ...
    case Point(x, 2) as p: # matches and bind to a variable
        ...
    case ["go", ("north" | "south" | "east" | "west") as direction]: # capturing matched subpatterns ??? why parentheses
        do_something_with_direction
    case _: # wildcard pattern
        ...
```

## Position-Only, Keyword-Only Arguments

```python
def f(pos1, pos2, /, pos_or_kwd, *, kwd1, kwd2):
      -----------    ----------     ----------
        |             |                  |
        |        Positional or keyword   |
        |                                - Keyword only
         -- Positional only
```

If `/` and `*` are not present in the function definition, arguments may be passed to a function by position or by keyword.

Also, with `/`, it is possible to have two parameters with the same name, one as a positional-only parameter and the other as a keyword-only parameter in `kwargs`.

## Default Argument

The default value is evaluated only only once for a function. If the argument value is mutable:

```python
def f(a, L=[]):
    L.append(a)
    return L

print(f(1))
print(f(2))
print(f(3))
```

prints

```
[1]
[1, 2]
[1, 2, 3]
```

## Slicing Assigment

Slicing may be used in assignment or deletion. Supposedly, in assigment, the elements in the slicing should be replaced. However, if the assign iterable is shorter or longer than the slice, the result is unexpected.

> If the target is a slicing: The primary expression in the reference is evaluated. It should yield a mutable sequence object (such as a list). The assigned object should be a sequence object of the same type. Next, the lower and upper bound expressions are evaluated, insofar they are present; defaults are zero and the sequence’s length. The bounds should evaluate to integers. If either bound is negative, the sequence’s length is added to it. The resulting bounds are clipped to lie between zero and the sequence’s length, inclusive. Finally, the sequence object is asked to replace the slice with the items of the assigned sequence. The length of the slice may be different from the length of the assigned sequence, thus changing the length of the target sequence, if the target sequence allows it.

```python
# append, extend
a[len(a):] = iterable

# insertion, where the index may be the end of the sequence
# and this is how sequence.insert is defined
s[i:i] = [x]
# append is equal to
s[len(s):len(s)] = [x]
# and even worse
s.insert(len(s) + 10, 5) # appends 5 ten times
```

## `del` 

A variable may not be referenced after a `del` on the variable. `del` not only deletes the object recursively, `del` a variable removes the binding of the name.
