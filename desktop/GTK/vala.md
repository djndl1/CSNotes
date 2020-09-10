# Basics

- basic types have `to_string()` and `parse(string str)` methods.

- All classes are subclasses of `GLib.Object`.

- non-specified fields in `struct` are zero-initialized and constructed with `new`.

- `unichar`: 32-bit unicode

- `string`: UTF-8 string. string templates: `==` and `!=` compare the content of two strings. Slicing `[start:end]` is supported. Strings are immutable. Many basic types have methods for parsing from and converting to strings. `in` can be used to determine whether one string contains another.

```vala
int a = 6, b = 7;
string s = @"$a * $b = $(a * b)";  // => "6 * 7 = 42"
```

- Slicing an array results in a reference to the requested data. Assigning the slice to an owned variable results in a copy. Multi-dimensional arrays are like those in C#. A mono-dimensional array can't be obtained from a multidimensional array. 

- Use `+=` to append an element to an array.

- All class types are reference types, regardless of whether they are descended from `GLib.Object` or not. Objects are ref-counted

- `var` type inference is available.

- `??`: null coalescing 

- `in`: works on arrays, strings, collections or any other type that has an appropriate `contain()` method.

- Operators/functions/methods cannot be overloaded. Choose slightly different names to avoid a name clash. However , default argument is supported. Index access are mapped to`get`/`set` methods. `[start:end]` is mapped to `slice(long start, long end)`. `in` is mapped to `contains()`; `foreach/in` loop is mapped to `iterator()`

- no fallthrough between cases. Each non-empty case must end with a `break`, `return` or `throw`.

- Nullability check can be added to a method parameter by postfixing the parameter with `?`.

- Closures/lambda: `(a) => { stdout.printf("%d\n", a); }`.

- `using` namespace is supported. `global::` namespace is there.

- Vala support _named constructors_ with different names. Constructor dispatch is supported. Destructors are there if needed.

# Structured Error Handling

For recoverable errors.

Related errors are grouped into error domains. Vala error domains do not form a class hierarchy.

```c
public errordomain MathError 
{
    DIVISION_BY_ZERO,
    OVERFLOW,
    //...
}
```

# Method Contracts

Vala has basic support for contract-based programming. Every method can specify a boolean expression that it `requires` to be true (a precondition checked at runtime) before the control flow enters the code block. If evaluated to `false`, a critical warning is emitted that usually leads to program termination. Postconditions are specified using the `ensures` keyword.


# Signal

Defined as a member of a class and appears similar to a method with no body. Signal handlers can be added to the signal using the `connect()` method.

Every `GLib.Object` instance has a signal called `notify`, which is emitted every time a property changes.

- Interface in vala may not inherit from other interfaces but may declare other interfaces (and classes) to be prerequisites, that is, when a class implements the interface, all the prerequisites must also be specified by the class declaration. 

- Vala allows method implementation in interfaces. 

- Vala also allows implementing two interfaces that have methods with the same name.

```vala
interface Foo {
 public abstract int m();
}

interface Bar {
 public abstract string m();
}

class Cls: Foo, Bar {
 public int Foo.m() {
  return 10;
 }

 public string Bar.m() {
  return "bar";
 }
}

void main () {
 var cls = new Cls ();
 message ("%d %s", ((Foo) cls).m(), ((Bar) cls).m());
}
```

- Vala provides `base` keyword to refer to the base class.

- `new` modifier hide an inherited method with a new method of the same name.

# RTTI

- `is`: check type

- `get_type()`

- `typeof()`

```vala
Type t = typeof(Foo);
Foo foo = (Foo) Object.new(t);
```

# Dynamic Type Casting

- `as`: `Button b = widget as Button;`

# Generics

Vala includes a runtime generics system. Vala's is similar to the system used by Java. There is no restriction on what type may be used in generic.


# Files

- `.vapi`: interface file

- `.deps`: depedency specification for interface files
