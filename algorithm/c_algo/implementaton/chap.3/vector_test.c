#include "vector.h"
#include "../lib/utils.h"
#include "../lib/sort.h"

#include <stdio.h>

vector_type_register(int);

void inc(int* pi)
{
        (*pi)++;
}

int main(int argc, char *argv[])
{
        vector_init(int, 30, NULL, vec);

        vector_for_each(i, vec) {
                vector_at(vec, i) = 60 - 2 * i;
                inc(&vector_at(vec, i));
        }

        vector_for_range(0, 30, _)
                printf("%d ", vector_at(vec, _));
        putchar('\n');

        vector_push_back(int, vec, 10);
        vector_push_back(int, vec, 20);
        vector_push_back(int, vec, 30);
        vector_push_back(int, vec, 40);

        vector_init(int, 20, NULL, arr);
        vector_for_each(i, arr)
                vector_at(arr, i) = i;

        INSERTION_SORT(int, vector_begin(vec), vector_end(vec));

        vector_pop_back(int, vec);
        vector_insert(int, vec, 20, 100);
        vector_swap(int, vec, arr);
        vector_for_each_in(int, elem, vec) {
                printf("%d ", *elem);
        }
        putchar('\n');

        vector_for_each_in(int, elem, arr) { printf("%d ", *elem); }
        putchar('\n');

        vector_delete(int, vec);
        return 0;
}
