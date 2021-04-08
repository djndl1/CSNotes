# Memory Allocation (`gmem.c`/`gmem.h`)

1. Some allocation function errors are fatal and meant to terminate the function.

2. Typed macros are based on `g_` allocation functions, which in turn call the underlying C allocation functions.

3. Customizing underlying allocation functions are no longer supported.

4. `g_steal_pointer` must accept the address of the pointer, i.e. `&pp` as its parameter; This function is nothing magic but some cast and assignment.

# Macros

- `#`: macro stringification

- Use negative array typedef as static assertion.

- offsetof: `((glong) ((guint8*) &((struct_type*) 0)->member))`
