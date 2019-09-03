#include "sort.h"

#include <stddef.h>

int element_comp(const void *l, const void *r)
{
        element_t lhs = l;
        element_t rhs = r;

        return lhs->dummy - rhs->dummy;
}

void insertSort(element_t elms[], size_t n)
{
        element_t tmp;
        for (size_t i = 1; i < n; i++) {
                tmp = elms[i];

                size_t j;
                for (j = i; j > 0 && element_comp(elms[j-1], tmp) > 0; j--)
                        elms[j] = elms[j-1];
                elms[j] = tmp;
        }
}
