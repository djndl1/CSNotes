#Class String

A `String` represents a string in the UTF-16 format in which supplementary characters are represented by surrogate pairs. Index values refer to `char` code points. Unless otherwise noted, methods for comparing `String`s do not take locale into account.

## Commonly used methods

- `char charAt(int index)`: returns the char value at the specified index

- `int codePointAt(int index)`: returns the code point that starts at the specified location.

- `int offsetByCodePoints(int startIndex, int cpCount)`: returns the index of the code point that is `cpCount` code points away from the code point at `startIndex`.

- `int compareTo(String other)`: returns a negative value if the string comes before other in dictionary order, a positive value if the string comes after other in dictionary order, or 0 if the strings are equal.

- `IntStream codePoints()`: returns the code points of this string as a stream. Call toArray to put them in an array.

- `isEmpty()`, `isBlank()`

- `boolean startsWith()`, `boolean endsWith()`


- `int indexOf()`: returns the start of the argument; `int lastIndexOf(String str)`: returns the start of last substring equal to the strings

- `length()`; `CodePointCount()`

- `replace()`

- `substring()`

- `toLowerCase()`; `toUpperCase()`

- `trim()`; `strip()`

- `join()`

```java
     String message = String.join("-", "Java", "is", "cool");
     // message returned is: "Java-is-cool"
```

- `repeat()`

# Class StringBuilder

A mutable sequence of characters, API compatible with `StringBuffer` without guarantee of synchronization. Every string builder has a capacity.If the internal buffer overflows, it is automatically made larger.

- `append()`, `insert()`: accepts boolean, char, char array, numbers, various `CharSequence` objects.

- `appendCodePoint()`, `codePointAt()`, `codePointBefore()`...

# Class `StringBuffer`

a thread-safe, mutable sequence of characters. String buffers are safe for use by multiple threads. The methods are synchronized where necessary so that all the operations on any particular instance behave as if they occur in some serial order that is consistent with the order of the method calls mde by each of the individual threads involved.


