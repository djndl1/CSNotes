#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "utest.h"

typedef int (compare_func_t)(void *left, void *right);

void insertion_sort(void* elems[], size_t len, compare_func_t comparer)
{
    if (len == 0 || len == 1 || comparer == NULL) return;

    for (size_t j = 1; j < len; j++) {
        void* cur = elems[j];

        size_t i = j;
        while (i > 0 && comparer(elems[i - 1], cur) > 0) {
            elems[i] = elems[i - 1];
            i--;
        }
        elems[i] = cur;
    }
}

int integer_comparer(void* left, void* right)
{
    return ((long)left) > ((long)right);
}

UTEST(INSERTION_SORT, INTEGER)
{
    void* a[] = { (void*)31, (void*)41, (void*)59, (void*)26, (void*)41, (void*)58 };

    insertion_sort(a, 6, integer_comparer);

    for (size_t i = 0; i < 6; i++) {
        printf("%ld ", (long)a[i]);
    }
    printf("\n");
}

UTEST_MAIN();
