#pragma once

#include <stddef.h>

#define array_init(type, size, var)                    \
struct { \
    type data[size]; \
    size_t sz ; \
} var; var.sz = size

#define array_set_size(arr, size) \
    arr.sz = size

#define array_for_each(idx, arr) \
    for (size_t idx = 0; idx < arr.sz; idx++)

#define array_for_range(lower, upper, idx)     \
    for (size_t idx = 0; idx < upper; idx++)

#define array_for_each_in(type, iter, arr)           \
    for (type *iter = arr.data;                  \
         iter != arr.data + arr.sz; \
        iter++)

#define array_at(arr, i)                          \
    (arr.data[i])

#define array_begin(arr) \
    (arr.data)

#define array_end(arr) \
    (arr.data + arr.sz)
