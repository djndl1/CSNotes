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

The STL supports the following set of relational function objects:

- `equal_to<T>`: calls `operator==`;

- `not_equal_to<T>`: calls `operator!=`;

- `greater<T>`: calls `operator>`;

- `greater_equal`: calls `operator>=`;

- `less<T>`: calls `operator<`;

- `less_equal<T>`: calls `opeator<=`;

Logical function object:

- `logical_and<T>`: `operator&&`

- `logical_or<T>`: `operator||`

- `logical_not<>`: `operator!`

```cpp
std::transform(bArr, bArr + bArrSize, logical_not<bool>()); # inverts a boolean array
```

(C++17) A negator (`std::not_fn`) is a function object toggling the truth value of a function that's called from the negator: if the function returns `true`, the negator returns false and vice versa. This is useful when the negated function cannot be modified.

# Iterators

Iterators are objects acting like pointers (a generalization, plain pointers can be used as iterators). Iterators have the following general characteristics:

- two iterators may be compared for equality using `==` and `!=`. The ordering operators can usually not be used;

- Given an iterator `iter`, `*iter` represents the object the iterator points to, `iter->member` is legal.

- `++iter` and `iter++` advances the iterator to the next element.

- Pointer arithmetic may be used with iterators of containers storing their elements consecutively in memory.

- Merely defining an iterator is comparable to having a null pointer.

 In general, iterators must define `operator==`, `operator!=`, `operator++`, `operator*`.

Standard practice requires iterator ranges to left inclusive (`[left, right)`). The iterator range is empty when `left==right`.

```cpp
vector<string> args(argv, argv + argc);

    for_each(args.cbegin(), args.cend(), [](const string& s) -> void { cout << s << ' '; });
    cout <<  '\n';

    for_each(args.rbegin(), args.rend(), [](const string& s) -> void { cout << s << ' '; });
    cout <<  '\n';
```

```bash
djn-pc  djn-pc-lenovo  ~/CodeSpace/playground  ./a.out a b c d e f g h i j k l m n o p q r s t
./a.out a b c d e f g h i j k l m n o p q r s t 
t s r q p o n m l k j i h g f e d c b a ./a.out 
```



The STL defines five types of iterators. Each category of iterator is defined by the operations that can be performed on it:

- `InputIterator`: The deference operator is guaranteed to work as rvalue in expressions. They are used to read from a container.

- `OutputIterator`: The deference operator is guaranteed to work as an lvalue in expression, but not necessarily as rvalue. They are used to write to a container.

- `Forwarditerator`: combine `InputIterator` and `OutputIterator`. They can be used to traverse containers in one direction, for reading and/or writing.

- `BidirectionalIterators`: They can traverse containers in both directions, for reading and writing.

- `RandomAccessIterators`: provides random acess to container elements.

`std::distance` expects two `InputIterator`s and returns the number of elements between them. `std::size` returns the number of elements in a container.

Generic algorithms often require a target container into which the results of the algorithm are deposited. Situations exist where pointer arithmetic cannot be used. Analogously, the number of resulting elements sometimes differs from the number of elements in the initial range. In situations like these an inserter adaptor function can often be used to create elements in the destination container.

- `back_inserter`: calls the container's `push_back` member to add new elements at the end of the container.

- `front_inserter` calls the container’s push_front member, adding new elements at the beginning of the container. 

-  `inserter` calls the container’s `insert` member adding new elements starting at a specified starting point.


The `istream_iterator<Type>` can be used to define a set of iterators for `istream` objects. 

```cpp
vector<string> vs;
copy(istream_iterator<string>(cin), istream_iterator<string>(), back_inserter(vs));
```

An `ostream_iterator<Type>` adaptor can be used to pass an `ostream` to algorithms expecting an `OutputIterator`.

```cpp
cin.unsetf(ios::skipws);
copy(istream_iterator<char>(cin), istream_iterator<char>(),
ostream_iterator<char>(cout));
```

Input iterators are also available for `streambuf` objects: `istreambuf_iterator`. Output iterators are also available for `streambuf` objects: `ostreambuf_iterator`.

```cpp
istreambuf_iterator<char> in(cin.rdbuf());
istreambuf_iterator<char> eof;
ostreambuf_iterator,char> out(cout.rdbuf());
copy(in, eof, out);
```

# Algorithms

- `<algorithm>`: generic algorithms except for operators

- `<numeric>`: generic algorithm in the operator category

Almost every generic algorithm expects an iterator range [first, last), defining the series of elments on which the algorithm operates. 

## Operators performing arithmetic operations of some sort

- `accumulate` in `<numeric>`

```cpp
int ia[] = {1, 2, 3, 4};
vector<int> iv(ia, ia + 4);

cout << accumulate(ia, ia+4, int{}) << ' ' << accumulate(iv.cbegin(), iv.cend(), int{1}, multiplies<int>()) << '\n';
```

```bash
10 24
```

- `adjacence_difference` in `<numeric>`

```cpp
    int ia[] = {1, 2, 3, 4};
    vector<int> iv(ia, ia + 4);

   vector<int> ov(iv.size());
    adjacent_difference(iv.begin(), iv.end(), ov.begin());
    copy(ov.begin(), ov.end(), ostream_iterator<int>(cout, " "));
    cout << '\n';

    adjacent_difference(iv.begin(), iv.end(), ov.begin(), multiplies<int>());
    copy(ov.begin(), ov.end(), ostream_iterator<int>(cout, " "));
    cout << '\n';
```

```bash
1 1 1 1
1 2 6 12
```

- `inner_product` in `<numeric>`

```cpp
    size_t ia1[] = {1, 2, 3, 4, 5, 6, 7};
    size_t ia2[] = {7, 6, 5, 4, 3, 2, 1};
    size_t init = 0;
    cout << inner_product(ia1, ia1+7, ia1, init) << '\n';
    cout << inner_product(ia1, ia1+7, ia2, init) << '\n';
```

```bash
140
84
```

- `partial_sum` in `<numeric>`

```cpp
    size_t ia1[] = {1, 2, 3, 4, 5, 6, 7};

    size_t ia3[7];
    copy(ia3, partial_sum(ia1, ia1+7, ia3), ostream_iterator<size_t>(cout, " "));
    cout << '\n';
    copy(ia3, partial_sum(ia1, ia1+7, ia3, multiplies<int>()), ostream_iterator<size_t>(cout, " "));
    cout << '\n';
```

```bash
1 3 6 10 15 21 28 
1 2 6 24 120 720 5040 
```

## Searchers performing search and find operations 

- `adjacent_find`: Searches the range [first, last) for two consecutive identical elements

- `binary_search`: ready sorted using `operator<` or a provided prdicate.

- `equal_range`

- `find`

- `find_end`: find the last occurrent of an element in the the sequence of elements

- `find_first_of`: find the first occurrent of an element in the sequence of elements

- `find_if`

- `lower_bound`

- `upper_bound`

- `max_element`

- `min_element`

- `search`: search the first occurrence of the sequence of elements

- `search_n`: search the sequence of n consecutive elements having the same value

- `set_difference`: must be sorted beforehand

- `set_intersection`: must be sorted beforehand (true for `std::set`)

- `set_symmetric_difference`:

- `set_union`

## Counters performing count operations

- `count`

- `count_if`

## Visitors visiting elements in a range

- `for_each`: apply a function to each element in the range. The return value of the function is ignored. If the elements should be transformed, use `transform`

- `replace`: replace all oldval in the range with the newval

- `replace_copy`: replace and the result is copied

- `replace_copy_if`

- `replace_if`

- `transform`: A unary operator is applied to each of the elements in the range and the resulting values are stored in another range.

- `unique_copy`: 


