# The `with statement`

The `with` statement is used wrap the execution of a block with methods defined by a context manager.

```python
with_stm ::= "with" with_item ("," with_item)* ":" suite
with_item ::= expression ["as" target]
 ```

The context expression (`with_item`) is evaluted to obtain a context manager; The context manager's `__enter__()` is invoked; the return value, if any, is assigned to the `target`; The `suite` is executed; the context manager's `__exit__()` method is invoked; if an an exception caused the suite to be exited, its type, value and traceback are passed as arguments to `__exit__()`, otherwise, three `None` arguments are supplied.

# Context Manager Types

The context manager type is a user-defined class that defines a runtime context that is entered before the statement body is executed and exited when the statement ednds.

## `__enter__()`

Enter the runtime context and return either this object or another object related to the runtime context.

## `__exit__(exc_type, exc_val, exc_tb)`

Exit the runtime context and return a Boolean flag indicating if any exception that occurred should be suppressed. If an exception occurred while executing the body of the with statement, the arguments contain the exception type, value and traceback information. Otherwise, all three arguments are `None`.

[More](www.python.org/dev/peps/pep-0343/) to read 

# `contextlib` - Utilities for `with`-statement contexts

Python defines several context managers to support easy thread synchronisation, prompt closure of files or other objects, and simpler manipulation of the active decimal arithmetic context.
