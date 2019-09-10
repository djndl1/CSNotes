#include "heap.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char *argv[])
{
        priority_queue_t h;

        
        h = priority_queue_ctor(10);
        srand(time(0));
        for (size_t i = 1; i <= 10; i++) {
                element_t tmp = { (rand()) / 100000000 + 1 };
                priority_queue_insert(h, tmp);
        }
//        priority_queue_make_heap_order(h);
        priority_queue_print(h);
        putchar('\n');
        priority_queue_sort_heap(h);
        priority_queue_print(h);
        putchar('\n');

        priority_queue_make_heap_order(h);
        priority_queue_print(h);
        putchar('\n');

        return 0;
}

