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
