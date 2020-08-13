# Basics

- All classes are subclasses of `GLib.Object`.

- `unichar`: 32-bit unicode

- `string`: UTF-8 string. string templates: `==` and `!=` compare the content of two strings. Slicing `[start:end]` is supported. Strings are immutable. Many basic types have methods for parsing from and converting to strings. `in` can be used to determine whether one string contains another.

```vala
int a = 6, b = 7;
string s = @"$a * $b = $(a * b)";  // => "6 * 7 = 42"
```

- Slicing an array results in a reference to the requested data. Assigning the slice to an owned variable results in a copy. Multi-dimensional arrays are like those in C#. A mono-dimensional array can't be obtained from a multidimensional array. 

- Use `+=` to append an element to an array.

- All class types are reference types, regardless of whether they are descended from `GLib.Object` or not. Objects are ref-counted

- `var` type inferencing is available.

- `??`: null coalescing 

- `in`: works on arrays, strings, collections or any other  type that has an appropriate `contain()` method.

- Operators/functions/methods cannot be overloaded. Choose slightly different names to avoid a name clash. However, default argument is supported.

- no fallthrough between cases. Each non-empty case must end with a `break`, `return` or `throw`.

- Nullability check can be added to a method parameter by postfixing the parameter with `?`.

- Closures/lambda: `(a) => { stdout.printf("%d\n", a); }`.

- `using` namespace is supported. `global::` namespace is there.
