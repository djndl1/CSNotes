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
