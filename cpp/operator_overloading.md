# overloading `operator[]()`

```cpp
int &operator[](size_t index);
int operator[](size_t index) const; // const object &operator[](size_t) const, also, const member specifier is needed in case *this is const
```
# overloading the insertion and extraction operators

They can be defined as overloaded free functions.

```cpp
std::ostream& operator<<(std::ostream &out, Person const& person);
```


# Conversion Operators

`char const *` can be easily converted to `std::string` thanks to assignment operator overloading and cosntructor overloading. However, the other way around is not possible unless using a conversion operator, in the form of `operator <type>() const;`

```cpp
class String
{
        char *d_string;
        public:
        String();
        String(char const *arg);
        ~String();
        String(String const &other);
        String &operator=(String const &rvalue);
        String &operator=(char const *rvalue);
};


String::operator char const *() const {
    return d_string;
}
```

Conversion operators usually are const member functions: they are automatically called when their
objects are used as rvalues in expressions having a type lvalue. As a rule of thumb: classes should define at most one conversion operator. Multiple conversion operators may be defined but frequently result in ambiguous code. 

A conversion operator should return an rvalue. It should do so to enforce data-hiding and because it is the intended use of the conversion operator. Defining a conversion operator as an lvalue (e.g., defining an `operator int &()` conversion operator) opens up a back door, and the operator can only be used as lvalue when explicitly called.

# `explicit`

Conversions are not only performed by conversion operators, but also by constructors accepting one argument. It is good practice to prevent implicit promotions by using the `explicit` modifier when declaring a constructor. Constructors using the explicit modifier can only be used to construct objects explicitly.

C++ supports _explicit conversion operators_. When defining explicit conversion operators implicit conversions are prevented. Such conversion operators can only be used in situations where the converted type is explicitly required (as in the condition clauses of if or while statements), or is explicitly requested using a `static_cast`.


# Overloading binary operators

https://stackoverflow.com/questions/4622330/operator-overloading-member-function-vs-non-member-function

## Member function reference bindings


# Overloading `operator new(size_t)`, `operator delete(void*)`

