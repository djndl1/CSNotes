#include "array.h"
#include "../lib/utils.h"
#include "../lib/sort.h"

#include <stdio.h>

void inc(int* pi)
{
        (*pi)++;
}

int main(int argc, char *argv[])
{
        array_init(int, 30, arr);

        array_for_each(i, arr) {
                array_at(arr, i) = 60 - 2 * i;
                inc(&array_at(arr, i));
        }

        for_range(2, 30, _)
                printf("%d ", array_at(arr, _));
        putchar('\n');

        INSERTION_SORT(int, array_begin(arr), array_end(arr));

        array_for_each_in(int, elem, arr) {
                printf("%d ", *elem);
        }
        putchar('\n');
        return 0;
}
