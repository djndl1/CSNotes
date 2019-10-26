# Behavior

A declaration `typename& ref` is not allowed. However, references can be declared as `extern`.

# When to use references

- When a function doesn’t change the value of its class- or struct-type arguments, or if the modification of the argument is a trivial side-effect (e.g., the argument is a stream) references can be used. Const-references should be used if the function does not modify the argument.

- When a function explicitly must change the values of its arguments, a pointer parameter is preferred. These pointer parameters should preferably be the function’s initial parameters. This is called _return by argument_.

# Rvalue reference

Def: An _anonymous variable_ has no name and is the distinguishing feature for the compiler to associate it automatically with an rvalue reference if it has a choice.

```c++
void receive(int &value) // note: lvalue reference
{
cout << "int value parameter\n";
}

void receive(int &&value) // note: rvalue reference
{
cout << "int R-value parameter\n";
}

int intVal() 
{
    return 5;
    
}
```

```c++
int main()
{
    receive(18);
    int value = 5;
    receive(value);
    receive(intVal());
}
```


```shell
 djn  debian  ~/FOSS/playground  ./reference_test 
int R-value parameter
int L-value parameter
int R-value parameter
```

The compiler selects the overloaded function using the rvalue reference of the function is passed an anonymous value.

This distinction allows the implementation of _move semantics_ and _perfect forwarding_.

# Lvalues, rvalues et al.

- lvalue: It referes to a location where a value can be stored, like a variable, a reference to a variable or a dereferenced pointer.

- xvalue: _expiring value_. An expiring value refers to an object just before its lifetime ends. Such objects may have resources to destroy or move to another location to prevent their destruction.

- gvalue: _generalized value_, anything that may receive a value, either a lvalue or an xvalue.

- prvalue: pure rvalue, a literal value except for string literal or an immutable object.

 An expression's value is an xvalue if it is:
 
- the value returned by a function returning an rvalue reference to an object.

- an object that is cast to an rvalue reference.

- an expression accessing a non-static class data member whose object is an xvalue or a pointer-to-member (`a.*mp`) expression in which the left-hand side operand is an xvalue and the right-hand side operand is a pointer to a data member. ???

Named rvalue reference are treated as lvalues and anonymous rvalue references to objects are treated as xvalues. Rvalue references to functions are treated as lvalues whether anonymous or not. 

```c++
struct Demo {
    int d_value
};

Demo &&operator+(const Demo &lhs, const Demo &rhs);
Demo &&factor();

Demo demo;
Demo &&rref = static_cast<Demo&&>(demo);
```

Expressions like 

```C++
factory();
factory().d_value;
static_cast<Demo&&>(demo);
demo + demo
```

are xvalues. However, `rref` is an lvalue.

# More on value category

Each C++ expression (an operator with its operands, a literal, a variable name, etc.) is characterized by two independent properties, type and _value category_. Each expression belongs to exactly one of the three primary value categories: _prvalue_, _xvalue_, _lvalue_.

- _glvalue_: an expression whose evaluation determines the identity of an object, bit-field, or function.

- _xvalue_: a glvalue that denotes an object or bit-field whose resources can be reused

- _lvalue_: a glvalue that is not an xvalue.

- _prvalue_: an expression whose evaluation either computes the value of the operand of an operator or initializes an object or a bit-field.

- _rvalue_: a prvalue or an xvalue.

# More on references

https://blog.petrzemek.net/2016/09/17/universal-vs-forwarding-references-in-cpp/

https://isocpp.org/files/papers/N4164.pdf
