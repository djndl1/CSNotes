#pragma once

#include <stdbool.h>

#define basic_swap(type, a, b)                         \
    { type tmp = a; a = b; b = tmp; }

#define type_predicate_register(type)           \
    typedef bool (*type##_binary_predicate)(type, type);    \
    typedef int (*type##_spaceship_comparator(type, type));    \
                                                               \
    bool type##_default_binary_predicate(type a, type b) \
    {                                                    \
        return a == b;                                   \
    }                                                    \
                                                         \
    int type##_default_spaceship_comparator(type a, type b) \
    {                                                       \
        if (a > b)                                          \
            return 1;                                       \
        else if (a < b) return -1;                          \
        else return 0;                                      \
    }


