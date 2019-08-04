# `*args` and `**kwargs` 

Only `*` and `**` matter, the names are just a convention. They allow you to pass variadic length arguments to a function. `*args` sends _non-keyworded_ variable length argument list:

```python
def test_var_args(f_arg, *argv):
    print("first normal arg:", f_arg)
    for arg in argv: # list
        print("another arg through *argv:", arg)

args = ("two", 2, 5)
test_var_args(*args)
```

`**kwargs` passes _keyworded_ variable length of arguments to a function. They are named arguments.

```python
def greet_me(**kwargs):
    for key, value in kwargs.items(): # dict
        print("{0} = {1}".format(key, value))

kwargs = {'args3': 3, 'arg2': 'two', 'args1': 5}
```

# Map, Filter and Reduce

`Map` applies a function to all the items in an input list (iterable).
