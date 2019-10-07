# `std::numeric_limits` and several examples

https://accu.org/index.php/journals/442

https://blog.galowicz.de/2016/02/18/what_is_a_type_trait/

http://blog.aaronballman.com/2011/11/a-simple-introduction-to-type-traits/

https://www.modernescpp.com/index.php/c-core-guidelines-programming-at-compile-time-with-the-type-traits

https://www.modernescpp.com/index.php/templates-misconceptions-and-surprises

https://www.modernescpp.com/index.php/c-core-guidelines-programming-at-compile-time-with-type-traits-ii



Instead of remember macros like `DBL_MAX`, `numeric_limits` provides a standardized way to query various properties of arithmetic types. The standard library makes available specialization for all arithmetic types, including their cv-qualified versions. The standard library types such as `std::size_t` may also be exambined with this type traits.

[About `isexact`](https://stackoverflow.com/questions/14203654/stdnumeric-limitsis-exact-what-is-a-usable-definition)

```cpp
template< typename T > 
struct is_void{ 
  static const bool value = false;
};

template<> 
struct is_void< void >{ 
  static const bool value = true; 
};
```

```cpp
template< typename T > 
struct is_pointer{ 
  static const bool value = false; 
};

template< typename T > 
struct is_pointer< T* >{ 
  static const bool value = true; 
};
```

