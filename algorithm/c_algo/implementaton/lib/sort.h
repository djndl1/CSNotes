#pragma once

#include "utils.h"

#define sort_type_register(type)                                        \
    typedef int type##_comparator(type, type);                          \
                                                                        \
    int type##_default_comparator(type a, type b)                       \
    {                                                                   \
        if (a > b)                                                      \
                return 1;                                               \
        else if (a == b)                                                \
            return 0;                                                   \
        else                                                            \
            return -1;                                                  \
    }                                                                   \
                                                                        \
    void type##_insertion_sort(type *start, type *end, type##_comparator comp) \
    {                                                                   \
        type tmp;                                                       \
        for (type* pi = start + 1; pi != end; pi++) {                   \
            tmp = *pi;                                                  \
                                                                        \
            type* pj;                                                   \
            for (pj = pi; pj > start && comp(*(pj-1), tmp) > 0; pj--)   \
                *pj = *(pj-1);                                          \
            *pj = tmp;                                                  \
        }                                                               \
    }                                                                   \

#define INSERTION_SORT_FUNC(type, pbegin, pend, comp_func)  \
    type##_insertion_sort(pbegin, pend, comp_func)

#define INSERTION_SORT(type, pbegin, pend)                          \
    type##_insertion_sort(pbegin, pend, type##_default_comparator)

sort_type_register(int);
