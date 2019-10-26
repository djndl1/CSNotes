The Standard Template Library is a general purpose library consisting of containers, generic algorithms, iterators, function objects, allocators, adaptors and abstract data structures. 

# Predefined Function Objects `<functional>`

Function objects play important roles in generic algorithms. By default, function objects are passed by value rather than by reference. Passing function objects by value instead of by reference has the advantage of being able to pass constant and temporary expressions. The disadvantage of passing the function object by value is that you can’t benefit from modifications of the state of the function objects

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


To get a result from function objects passed to algorithms:

1. keep the state externally and let the function objects refer to it;

2. pass the function objects by reference

```cpp
IntSequence seq(1);
generate_n<back_insert_iterator<list<int>>, int, IntSequence&>(back_inserter(coll), 4, seq); // 2 3 4 5
generate_n(back_inserter(coll), 4, seq); // insert 6 7 8 9
```

3. use the return value of `for_each`

```cpp
class MeanValue {
private:
    long num;
    long sum;
public:
    MeanValue() : num(0), sum(0) {
    }
    
    void operator() (int elem) {
        ++num;
        sum += elem;
    }
    
    double value() {
        return static_cast<double>(sum) / static_cast<double>(num);
    }
};

vector<int> coll = {1, 2, 3, 4, 5, 6, 7, 8};
MeanValue mv = for_each(coll.begin(), coll.end(), MeanValue());
```

Not every function that returns a Boolean value is a valid predicate for the STL. A predicate should always be stateless.

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

A function adapter is a function object that enables the composition of function objects with each other, with certain values, or with special functions.

- `bind(op, args...)`: binds parameters `args` to a callable object`op`. It allows for adapting and composing new function objects out of existing or predefined function objects; call global functions; call member functions for objects, pointers to objects and smart pointers to objects

```cpp
auto plus10 = std::bind(std::plus<int>(), std:;placeholder::_1, 10); // + 10
auto plus10times2 = std:;bind(std::mutliplies<int>(), 
                              std::bind(std::plus<int>(),
                                        std::placeholders::_1,
                                        10),
                              2)); // + 10  *2 
                              
    auto pow3 = std::bind(std::multiplies<int>(),
                  std::bind(std::multiplies<int>(),
                            std::placeholders::_1,
                            std::placeholders::_1),
                  std::placeholders::_1); // x * x * x
                  
auto inversDivide = std::bind(std::divides<double>(),
                                  std::placeholders::_2,
                                  std::placeholders::_1); // inverse division 

```

The `binder` can be called directly.

```cpp
std::cout << std::bind(std::plus<int>(), _1, 10)(32) << std::endl;
std::transform(coll.begin(), coll.end(), coll.begin(), coll.begin(), 
            std:;bind(std::plus<int>(), _1, 10));
```

`bind()` internally copies passed arguments. To let the function object use a reference to a passed argument, use `ref()`, `cref()`.

```cpp
void incr(int& i)
{
    i++;
}

int i = 0;
bind(incr, i)();
bind(incr, ref(i)();
```

Calling member function. Calling virtual member functions also work.

```cpp
for_each(coll.begin(), coll.end(),
        bind(&Person::print, _1)); // the first argument is this pointer
```

```cpp
int sum = accumulate(coll.begin(), coll.end(), 0,
                    bind(plus<int>(), _1, bind(&map<string, int>::value_type::second, _2)));
```

`bind()` can be used to call global functions

```cpp
char myToupper(char c)
{
    std::locale loc;
    return std::use_facet<std::ctype<char>>(loc).toupper(c);
}

pos = search(str.begin(), str.end(),
            subs.begin(), subs.end(),
            bind(equal_to<char>(), bind(myToupper, _1), bind(myToupper, _2)));
```


- `mem_fn(op)`: call `op()` as a member function for an object or pointer to object. The placeholder for the object the member function is called for can be skipped.

```cpp
std::for_each(coll.begin(), coll.end(), std::mem_fn(&Person::print));
```

However, to bind an additional argument to the function object, `bind()` is still needed:

```cpp
std::for_each(coll.begin(), coll.end(), 
            std::bind(std::mem_fn(&Person::print2), std::placeholders::_1, "Person: ));
```

- (removed in C++20)`not1(op)`: unary negation `!op(param)`;  `not2(op)`: binary negation `!op(param1, param2)`, they can be replaced by `std:;bind(std::logical_not<bool>(), func`. There is no real-world scenario for `not1()` and `not2()`.

- `bind1st`, `bind2st`, `ptr_fun`, `mem_fun`, `mem_fun_ref`, `not1`, `not2` are all deprecated (most are removed in C++17).

- `std::function` provides support for storing arbitrary function objects. It is a (general-purpose polymorphic function wrapper)[https://probablydance.com/2012/12/16/the-importance-of-stdfunction/]. Instances of `std::function` can store, copy and invoke any `Callable` target -- lambdas, functions, bind expressions or other function objects as well as pointers to member functions and pointers to data members. Invoking an empty `std::function` results in `std::bad_function_call` exception being thrown.

## Lambdas versus Binders

```cpp
auto plus10 = [] (int i) {
    return i + 10;
};

auto plus10times2 = [] (int i) {
    return (i + 10) * 2;
};

auto pow3 = [] (int i) {
    return i * i * i;
}

auto inversDivide = [] (double d1, double d2) {
    return d2 / d1;
};
```

## Lambdas versus Stateful Function Objects

```cpp
vector<int> coll = { 1, 2, 3, 4, 5, 6, 7, 8 };

   // process and print mean value
   long sum = 0; // the state is now outside the function
   for_each (coll.begin(), coll.end(),  // range
             [&sum] (int elem) {
                 sum += elem;
             });
   double mv = static_cast<double>(sum)/static_cast<double>(coll.size());
   cout << "mean value: " << mv << endl;
```

```cpp
list<int> coll = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

list<int>::iterator pos;
    int count=0;     // call counter
    pos = remove_if(coll.begin(),coll.end(),   // range
                    [count] (int) mutable {   // count now an internal state
                        return ++count == 3;
                    });
    coll.erase(pos,coll.end());
```

However, to make this work correctly

```cpp
list<int> coll = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

list<int>::iterator pos;
    int count=0;     // call counter
    pos = remove_if(coll.begin(),coll.end(),   // range
                    [&count] (int)  {   // count must be an external state
                        return ++count == 3;
                    });
```

## Lambdas Calling Global and Member Functions

```cpp
search(s.begin(), s.end(),
    sub.begin(), sub.end(),
    [] (char c1, char c2) {
        return myToupper(c1) == myToupper(c2);
    });
```

```cpp
for_each(coll.begin(), coll.end(),
    [] (const Person& p) {
        p.print();
    });
    
for_each(coll.begin(), coll.end(),
    [] (const Person& p) {
        p.print2("Person: ");
    });
```

## Lambdas as hash function, sorting or equivalence criterion

```cpp
auto hash = [] (const Person& p) {
    ...
};
auto eq = [] (const Person& p1, Person& p2) {
    ...
};

unordered_set<Person, decltype(hash), decltype(eq)> pset(10, hash, eq);
```

`decltype` is needed here. However, specifying a class for the function objects here can be considered as being more readable and even more convenient.

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

## `unique_ptr` in `<memory>`

`unique_ptr`s are objects masquerading as pointers. Since they are objects, their destructors are called when they go out of scope. Their destructors automatically delete the dynamically allocated memory to which they point. 

- when assigning a `unique_ptr` to another move semantics is used.

- multiple unique_ptr objects should not be allowed to point to the same block of dynamically allocated memory.

- If `T` is a derived class of some base `B`, then `std::unique_ptr<T>` is implicitly convertible to `std::unique_ptr<B>`. The default deleter of the resulting `std::unique_ptr<B>` will use `operator delete` for B, leading to undefined behavior unless the destructor of `B` is virtual. However, a custom deleter can be provided to `unique_ptr` so that proper deletion is guaranteed.

A `unique_ptr` can be constructed with a deleter, used in situations like the followintg

```cpp
struct Deleter {
    size_t d_size;
    Deleter(size_t size = 0) : d_size{size} {}
    void operator()(string **ptr) const {
        for (size_t idx = 0; idx < d_size; ++idx) {
            delete ptr[idx];
        }
        delete[] idx;
    }
};

unique_ptr<sstring*, Deleter> sp2{new string *[10], Deleter{10}};
```

A `unique_ptr` can point to an array:

```cpp
unique_ptr<int[]> intArr(new int[3]);
intArr[2] = intArr[0];
```

## `shared_ptr` in `<memory>`

The shared pointer automatically destroys its contents once its reference count has decayed to zero.

If `T` is a derived class of some base `B`,  `std::shared_ptr<B>` will use the `operator delete` for the type `T` and the owned object will be deleted correctly even if the destructor of `B` is not virtual. Polymorphism isn’t required.

Different from the `unique_ptr` class no specialization exists for the shared_ptr class to handle dynamically allocated arrays of objects.

To avoid double free error, pointer cast operations are provided so that after casting, the resulting pointer still share the ownership of the same object

```cpp
template< class T, class U > 
std::shared_ptr<T> static_pointer_cast( const std::shared_ptr) noexcept;

template< class T, class U > 
std::shared_ptr<T> dynamic_pointer_cast( std::shared_ptr) noexcept;

template< class T, class U > 
std::shared_ptr<T> const_pointer_cast( const std::shared_ptr) noexcept;

template< class T, class U > 
std::shared_ptr<T> reinterpret_pointer_cast( std::shared_ptr) noexcept;
```

## `weak_ptr` in `<memory>`

``std::weak_ptr is a smart pointer that holds a non-owning ("weak") reference to an object that is managed by `std::shared_ptr`. It must be converted to `std::shared_ptr` in order to access the referenced object.

`std::weak_ptr` models temporary ownership: when an object needs to be accessed only if it exists, and it may be deleted at any time by someone else, `std::weak_ptr` is used to track the object, and it is converted to `std::shared_ptr` to assume temporary ownership. If the original `std::shared_ptr` is destroyed at this time, the object's lifetime is extended until the temporary `std::shared_ptr` is destroyed as well. It has the semantics that the lifetime of a reference to an object outlives the object it refers to. Raw pointers may be used but it's risky.

In situations like cyclic references, `shared_ptr` may cause memory leak.

```cpp
class Person {
public:
    string name;
    shared_ptr<Person> mother;
    shared_ptr<Person> father;
    vector<shared_ptr<Person>> kids;
    Person (const string& n,
            shared_ptr<Person> m = nullptr,
            shared_ptr<Person> f = nullptr)
        : name(n), mother(m), father(f) {
    }
    ~Person() {
        cout << "delete " << name << endl;
    }
};

shared_ptr<Person> initFamily (const string& name)
{
    shared_ptr<Person> mom(new Person(name+"’s mom"));
    shared_ptr<Person> dad(new Person(name+"’s dad"));
    shared_ptr<Person> kid(new Person(name,mom,dad));
    mom->kids.push_back(kid);
    dad->kids.push_back(kid);
    return kid;
}
int main()
{
    shared_ptr<Person> p = initFamily("nico");
    cout << "nico’s family exists" << endl;
    cout << "- nico is shared " << p.use_count() << " times" << endl;
    cout << "- name of 1st kid of nico’s mom: " << p->mother->kids[0]->name << endl;
    p = initFamily("jim");
    cout << "jim’s family exists" << endl;
}
```

Here `"nico"`, his mother and father are never destroyed but no handle to him is now available. The solution is to use `weak_ptr`:

```cpp
class Person {
public:
    string name;
    shared_ptr<Person> mother;
    shared_ptr<Person> father;
    vector<weak_ptr<Person>> kids; // weak pointer !!!
    Person (const string& n,
            shared_ptr<Person> m = nullptr,
            shared_ptr<Person> f = nullptr)
        : name(n), mother(m), father(f) {
    }
    ~Person() {
        cout << "delete " << name << endl;
    }
};
```





## `make_shared` and `make_unique` in `<memory>`

To avoid double allocation overhead, use `make_*` instead of smart pointers' constructors. It employs perfect forwarding.

# Algorithms

All STL algorithms process one or more iterator ranges. The caller must ensure that the ranges are valid. Algorithms work in overwrite mode rather than insert mode. The caller must ensure that destination ranges have enough elements. Insert iterators switch from overwrite to insert mode.

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

- `unique_copy`: consecutively equal elements are copied only once.

## Initializers initializing data

- `fill`

- `fill_n`

- `generate`: initialized by the return value of generator, which can be a function or function object.

```cpp
class NaturalSquares {
    size_t d_newsqr;
    size_t d_last;

public:
    NaturalSquares() : d_newsqr{0}, d_last{0}
        {}
    size_t operator()()
        {
            return d_newsqr += (d_last++ << 1) + 1;
        }
};

    vector<size_t> uv(10);
    generate(uv.begin(), uv.end(), NaturalSquares{});
    copy(uv.begin(), uv.end(), ostream_iterator<size_t>{cout, " "});
    cout << '\n';
```

```bash
1 4 9 16 25 36 49 64 81 100 
```

- `generate_n`:

```cpp
    vector<size_t> uv_n(10);
    generate_n(uv_n.begin(), 5, NaturalSquares{});
    copy(uv_n.begin(), uv_n.end(), ostream_iterator<size_t>{cout, " "});
    cout << '\n';
```

```bash
1 4 9 16 25 0 0 0 0 0 
```

## Comparators comparing (range of)) elements

- `equal`: pairwise equality comparison

- `includes`: sorted beforehand; check if the second range is contained in the first

- `lexicographical_compare`

- `max`: larger of the two

- `min`: smaller of the two

- `mismatch`: find mismatch (nonequal) pair

## Copiers performing copy operations

- `copy`: copy a range of elements to a range starting with the specified destination

- `copy_backward`: copy a range of elements to a range ending with the specified destination 

```cpp

    for (int i = 0; i < 10; i++) {
        from_vector.push_back(i);
    }
 
    std::vector<int> to_vector(15);
 
    std::copy_backward(from_vector.begin(), from_vector.end(), to_vector.end());
 
    std::cout << "to_vector contains: ";
    for (auto i: to_vector) {
        std::cout << i << " ";
```

```bash
to_vector contains: 0 0 0 0 0 0 1 2 3 4 5 6 7 8 9
```


- `partial_sort_copy`: Sorts some of the elements in the range [first, last) in ascending order, storing the result in the destination range.

- `remove_copy`: remove all values that mismatch the given one to another place

```cpp
        string words[] =
            { "kilo", "alpha", "lima", "mike", "alpha", "november", "alpha",
                "oscar", "alpha", "alpha", "papa", "quebec" };
        size_t const size = sizeof(words) / sizeof(string);
        string remaining
                [
                    size -
                    count_if
                    (
                        words, words + size,
                        bind2nd(equal_to<string>(), string("alpha"))
                    )
                ];
        string *returnvalue =
                remove_copy(words, words + size, remaining, "alpha");

        cout << "Removing all \"alpha\"s:\n";
        copy(remaining, returnvalue, ostream_iterator<string>(cout, " "));
        cout << '\n';
```

```bash
Removing all "alpha"s:
kilo lima mike november oscar papa quebec 
```

- `remove_copy_if`

- `replace_copy`: replace and copy to a destination

```cpp
        string words[] =
            { "kilo", "alpha", "lima", "mike", "alpha", "november", "alpha",
                "oscar", "alpha", "alpha", "papa"};
        size_t const size = sizeof(words) / sizeof(string);
        string remaining[size];

        copy
        (
            remaining,
            replace_copy(words, words + size, remaining, string("alpha"),
                                                         string("ALPHA")),
            ostream_iterator<string>(cout, " ")
        );
        cout << '\n';
```

```bash
kilo ALPHA lima mike ALPHA november ALPHA oscar ALPHA ALPHA papa 
```

- `replace_copy_if`

- `reverse_copy`: copy to a destination in reverse order

- `rotate_copy`: rotate around an axis and copy to a destination

```cpp
        string words[] =
           { "kilo", "lima", "mike", "november", "oscar",
              "foxtrot", "golf", "hotel", "india", "juliet" };
        size_t const size = sizeof(words) / sizeof(string);
        size_t const midsize = size / 2;
        string out[size];

        copy(out,
            rotate_copy(words, words + midsize, words + size, out),
            ostream_iterator<string>(cout, " "));
        cout << '\n';
```

```cpp
foxtrot golf hotel india juliet kilo lima mike november oscar 
```

- `unique_copy`: consecutively equal elements are not copied

## Heap operators: manipulating a max-heap

Heaps can be constructed in containers supporting random access.

- `make_heap`

- `pop_heap`

- `push_heap`

- `sort_heap`


## Shuffler performing reordering operations

- `inplace_merge`: two sorted ranges merge in place 

- `iter_swap`: elements pointed by two iterators are swapped, implemented as `swap(*__a, *__b)`.

- `merge`: merge two sorted ranges into a destination

- `next_permutation`: The elements are reordered such that each new permutation represents the next bigger value using `operator<`.

- `nth_element`: `nth_element` partially sorts the range `[first, last)` in ascending order so that the condition `!(*j < *i)` is met for any i in the range `[first, nth)` and for any j in the range [nth, last). The element placed in the nth position is exactly the element that would occur in this position if the range was fully sorted.

- `partial_sort`: find the n smallest elements

- `partial_sort_copy`: depends on the range of the destination and the source

- `partition`: partition a range according to a predicate

- `prev_permutation`

```cpp
    int perms[]  = {2, 1, 3, 4, 5};
    do {
        copy(perms, perms+5, ostream_iterator<int>{cout, " "});
        cout << '\n';
    } while (prev_permutation(perms, perms+4));
```

```bash
2 1 3 4 5 
1 4 3 2 5 
1 4 2 3 5 
1 3 4 2 5 
1 3 2 4 5 
1 2 4 3 5 
1 2 3 4 5 
```

- `remove`: [returnvalue, last) are all removable

- `remove_copy`: anything not matching the specified one to remove is copied 

- `remove_copy_if`

- `remove_if`

- `reverse`

- `reverse_copy`

- `rotate`: more like translation rather than rotation around an axis

- `rotate_copy`

- `sort`

- `stable_partition`: 

- `stable_sort`the relative order of equal elements are kept

- `swap`

- `unique`: all unique elements are moved before the return value


