The Standard Template Library is a general purpose library consisting of containers, generic algorithms, iterators, function objects, allocators, adaptors and abstract data structures. 

# Predefined Function Objects `<functional>`

Function objects play important roles in generic algorithms.

```cpp
class CaseInsensitiveComp {
    public:
        bool operator()(string const &left, string const &right) const 
        {
            return strcasecmp(left.c_str(), right.c_str()) < 0;
        }
}

sort(argv, argv + argc, CaseInsensitiveComp());
```

The arithmetic function objects support the standard arithmetic operations: 

- addition: `plus<T>`, calling the binary `operator+`;

- subtraction: `minus<T>`, calling the binary `operator-`;

- multiplication: `multipliers<T>`, calling the binary `operator*`;

- division: `divides<T>`, calling `operator/`;

- modulo: `modulus<T>`, calling `operator%`;

- negation: `negate<T>`, calling unary `operator-`;
