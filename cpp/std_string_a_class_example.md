# Use string types

In C++, the traditional C library functions manipulating NTB are deprecated in favor of using `string` objects. Many problems in C programs are caused by buffer overruns, boundary errors and allocation problems that can be traced back to improper using these traditional C string library functions. C++ strings are extremely versatile and there is hardly a reason for falling back on the C library to process text. C++ strings handle all the required memory management and thus memory related problems canbe prevented.

In addition to `std::string`, there is `std::wstring` ( a string type consisting of `wchar_t` characters), `std:;u16string (`char16_t` characters)`, `std::u32string` (`char32_t`).

# Operations on strings

`std::string` does have limitations, like, it doesn't provide case-insensitive comparisons. To perform a traditional string manipulation, use `.c_str()` function.

Strings are properly initialized when using, always in a valid state. It can be assigned using C strings or assign itself to a character buffer. Strings may be compared to other strings (NTBSs) using logical comparison operators. Its content may be modified. Searching is supported. Strings can be extracted from or inserted into streams.

`npos` (implemented as `static_cast<size_type>(-1)`) is returned if various o
perations fail.

# Initialization

Not every constructor should be called using `{}`, which may, if not paying attention, select the initializer-list constructor.

Strings can be initialized through `string`, a part of another string, a pair of iterators, an `initializer_list<char>`, a number of certain characters or as an empty string.

# Operators

`string[]` has no range checking (use `.at()` for that) .

# Conversion

There are several free functions declared in `<string>`
