## `ArrayList`

Resizable-array implementation of the `List` interface. Similar to the C++ `vector` template

The `ArrayList` class is similar to an array, but it automatically adjusts its capacity as you add and remove elements without any addtional code. 

- `.add()`, `.remove()`: add/remove new elements to an array list.

- `.ensureCapacity()`: allocates an internal array of a certain length. 

- `.size()`: returns the actual number of elements in the array list

- `.trimToSize()`: use this when the array list is at its permanent size.

- `.set()`, `.get()` access array list elements.

- `.toArray()`
