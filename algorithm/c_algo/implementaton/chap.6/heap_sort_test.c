#include "heap_generic.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int dummy(const void *lhs, const void *rhs)
{
        const element_t *__lhs = lhs;
        const element_t *__rhs = rhs;

        if (__lhs->dummy > __rhs->dummy)
                return -1;
        else if (__lhs->dummy < __rhs->dummy)
                return 1;
        else
                return 0;
        
}

int main(int argc, char *argv[])
{
        priority_queue_t h;

        
        h = priority_queue_ctor(10);
        srand(time(0));
        for (size_t i = 1; i <= 10; i++) {
                element_t tmp = { (rand()) / 100000000 + 1 };
                priority_queue_insert(h, tmp, dummy);
        }
//        priority_queue_make_heap_order(h);
        priority_queue_print(h);
        putchar('\n');
        priority_queue_sort_heap(h, dummy);
        priority_queue_print(h);
        putchar('\n');

        priority_queue_make_heap_order(h, dummy);
        priority_queue_print(h);
        putchar('\n');

        return 0;
}

