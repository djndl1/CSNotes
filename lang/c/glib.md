# Memory Allocation (`gmem.c`/`gmem.h`)

1. Some allocation function errors are fatal and meant to terminate the function.

2. Typed macros are based on `g_` allocation functions, which in turn call the underlying C allocation functions.

3. Customizing underlying allocation functions are no longer supported.

4. `g_steal_pointer` must accept the address of the pointer, i.e. `&pp` as its parameter; This function is nothing magic but some cast and assignment.

# Macros

- `#`: macro stringification

- Use negative array typedef as static assertion.

- offsetof: `((glong) ((guint8*) &((struct_type*) 0)->member))`

# Atomics

The atomic operations are implemented as thin wrappers around GCC instrincs or Windows API. If lock-free operations are not supported by the platform, a pthread mutex is used to simulate atomicity. All the operations act as a full memory barrier (the mutex implementation guarantees so due to [POSIX specification](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_12)).
https://stackoverflow.com/questions/24137964/does-pthread-mutex-lock-contains-memory-fence-instruction
An atomic operation ensures the corresponding operation is atomic, not the entire implementation unit.

The `volatile` indicates mutability of a variable, thus prevents some optimization, ensuring that each volatile variable assignment and read has a corresponding memory access.

https://stackoverflow.com/questions/5002046/atomicity-in-c-myth-or-reality
Some read/write operations are guaranteed to be atomic without any compiler intrinsics.
The instrincs used in `_get` and `_set` are for hardware memory barrier. 
