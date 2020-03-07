#include "vector.h"

#include <stdio.h>

void inc(int *pi) { (*pi)++; }

int main(int argc, char *argv[])
{
        vec_t(int, 30, NULL, NULL, a);

        for_each_in(i, a)
                AT(a, i) = 5;

        for_each_do(a, inc);

        for_range(0, SIZE(a), i)
                printf("%d ", AT(a, i));
        putchar('\n');
        PUSH_BACK(10, a);
        PUSH_BACK(20, a);
        PUSH_BACK(30, a);
        PUSH_BACK(40, a);
        vector_ops(int, push_back)(&a, 50);

        for_range(0, SIZE(a), i) printf("%d ", AT(a, i));
        putchar('\n');

        DELETE(a);
        return 0;
}

