# Overview

All but the unordered containers support:

1. The overloaded assignment operator. (supported by unordered containers)

2. equality: `==`, `!=`; 

3. ordering operators: `<`, `<=`, `>=`, `>` 

A type stored in a container must at least support

- a default value;

- the equality operator (`==`)

- the less-than operator (`<`)

Virtually all containers support copy construction.

Normally containers should not contain pointers to data.

# `std::pair` in `<utility>` 

```cpp
pair<string, string> piper("PA28", "PH-AHI");
pair<string, string> cessna("C172", "PH-ANG");

cout << piper.first;
cessna.second = "PH-ANW";
cessna = pair<string, string>("C152", "PH-ANW");
```

`std::pair` is a basic ingredient of `std::map`, `std::multimap` and `hash_map`.

# `std::tuple` in `<tuple>`

 A generalized pair container. A tuple can be considered the ‘template’s answer to C’s `struct`’.
 
 ```cpp
 typedef std::tuple<int, double&, std::string, char const*> tuple_idsc;
 double pi = 3.14;
 tuple_idsc idsc(59, pi, "hello", "fixed");
 std::get<2>(idsc) = "hello world";
 ```

Type-based tuple addressing can be used for tuple types used once in a tuple definition (if the same type is used repeatedly referring to that type introduces an ambiguity).

```cpp
std::tuple_size<Tuple>::value // obtain the size of tuple at compile time
std::tuple_element<idx, Tuple>::type // obtains the type of the specified element
```

The unpack operator can also be used to forward the arguments of a constructor to a tuple data member.

```cpp
template <typename ...Params>
class Wrapper {
    std::tuple<Params...> d_tuple;
public:
    Wrapper(Params&& ...params) : d_tuple{std::forward<Params>(params)...}
    {}
}
```

# Structured Bindings with `std::tuple` and `std::pair`

TODO

# Allocators

Most containers use a special object for allocating the memory that is managed by them. This object
is called an allocator, and it’s type is (usually by default) specified when a container is constructed. 

```cpp

// allocate raw memory for holding count values of the container's value_type
      pointer
      allocate(size_type __n, const void* = static_cast<const void*>(0))
      {
	if (__n > this->max_size())
	  std::__throw_bad_alloc();

#if __cpp_aligned_new
	if (alignof(_Tp) > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
	  {
	    std::align_val_t __al = std::align_val_t(alignof(_Tp));
	    return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp), __al));
	  }
#endif
	return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
      }

// calls operator delete to delete object's memory, previously allocated by `allocate`
      void
      deallocate(pointer __p, size_type)
      {
#if __cpp_aligned_new
	if (alignof(_Tp) > __STDCPP_DEFAULT_NEW_ALIGNMENT__)
	  {
	    ::operator delete(__p, std::align_val_t(alignof(_Tp)));
	    return;
	  }
#endif
	::operator delete(__p);
      }      
```

```cpp
    vector<string> vs;

    auto allocator = vs.get_allocator();        // get the allocator

    string *sp = allocator.allocate(3);         // alloc. space for 3 strings

    allocator.construct(&sp[0], "hello world"); // initialize 1st string
    allocator.construct(&sp[1], sp[0]);         // use the copy constructor
    allocator.construct(&sp[2], 12, '=');       // string of 12 = chars

    cout << sp[0] << '\n' <<                    // show the strings
            sp[1] << '\n' <<
            sp[2] << '\n' <<
            "could have allocated " << allocator.max_size() << " strings\n";

    for (size_t idx = 0; idx != 3; ++idx)
        allocator.destroy(sp + idx);            // delete the string's
                                                // contents

    allocator.deallocate(sp, 3);                // and delete sp itself again.
```

# `std::array` in `<array>`

A fixed-size array. The elements of an array are stored contiguously. 

Using an array rather than a standard C style array offers several advantages:

- All its elements are immediately initialized

- Introspection is possible

- can be used with generic algorithms

```cpp
// how the elements are stored, note the weird typedef _Tp _Type[_Nm], that's how array typedef is done
// the elements are actually stored on the stack
  template<typename _Tp, std::size_t _Nm>
    struct __array_traits
    {
      typedef _Tp _Type[_Nm];
      typedef __is_swappable<_Tp> _Is_swappable;
      typedef __is_nothrow_swappable<_Tp> _Is_nothrow_swappable;
      ...
    }
    
  template<typename _Tp, std::size_t _Nm>
    struct array
    {
    ...
      typedef _GLIBCXX_STD_C::__array_traits<_Tp, _Nm> _AT_Type;
      typename _AT_Type::_Type                         _M_elems;
    ...
    }
```

In general, when looking for a sequential data structure, the `array` or `vector` should be the ‘weapon of choice’.

# `std::vector` in `<vector>`

An expandable array.

```cpp
iterator emplace(const_iterator position, Args&& ...args); 
/** Inserts a new element into the container directly before position. The element is constructed through std::allocator_traits::construct, which typically uses placement-new to construct the element in-place at a location provided by the container. */

void emplace_back(Args&& ...args);
vector::iterator erase(); // erase a specific range of elements in the vector.
```

# `std::list` in `<list>`

 It is usually implemented as a doubly-linked list. This container provides bidirectional iteration capability while being less space efficient. At present lists aren’t as useful anymore as they used to be (when computers were much slower and more memory-constrained). Except maybe for some rare cases, a vector should be the preferred container; even when implementing algorithms traditionally using lists.

```cpp
void splice(pos, object); 
// Transfers elements from one list to another. No elements are copied or moved, only the internal pointers of the list nodes are re-pointed.

void unique();
// operating on a sorted list to remove all consecutively identical elements from the list
```

# `std::queue` in `<queue>`

It is most often used in situations where events should be handled in the same order as they are generated.

 The queue does not support iterators or an index operator. The only elements that can be accessed are its front and back element.
 
# `std::priority_queue` in `<queue>`

The priority queue uses `operator<` of the data type stored in the priority queue to decide about the priority of the data elements. The smaller the value, the lower the priority. So, the priority queue could be used to sort values while they arrive.

```cpp
#include <queue>
#include <iostream>
#include <string>

class Text {
    std::string d_s;

public:
    Text(std::string const &str) : d_s{str} {};
    operator std::string const &() const
        {
            return d_s;
        }

    bool operator<(Text const &right) const
        {
            return d_s > right.d_s;
        }
};

using namespace std;

int main(int argc, char *argv[])
{
    priority_queue<Text> q;
    string word;
    while (cin >> word)
        q.push(word);

    while (q.size()) {
        word = q.top();
        cout << word << '\n';
        q.pop();
        
    }
    return 0;
}
```
