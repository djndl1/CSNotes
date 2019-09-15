#include "sort.h"

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

int element_comp(const void *l, const void *r)
{
        const struct Element* lhs = l;
        const struct Element* rhs = r;

        return lhs->dummy - rhs->dummy;
}

int element_comp_reverse(const void *l, const void *r)
{
        const struct Element* lhs = l;
        const struct Element* rhs = r;

        return rhs->dummy - lhs->dummy;
}

void randomizeArray(element_t A[], size_t n)
{
        static bool seeder = false;
        if (seeder == false) {
                srand(time(0));
                seeder = true;
        }

        for (size_t i = 0; i < n; i++) {
                A[i]->dummy = rand() / 100000000 ;
        }
}

void printArray(element_t A[], size_t n)
{
        for (size_t i = 0; i < n; i++) {
                printElement(A[i]);
                putchar(' ');
        }
        putchar('\n');
}

bool checkOrder(element_t A[], size_t n, comp_t element_comp)
{
        for (size_t i = 0; i < n - 1; i++) {
                if (element_comp(A[i], A[i+1]) > 0)
                        return false;
        }
        return true;
}

int main(int argc, char *argv[])
{
        element_t A[10];
        for (size_t i = 0; i < 10; i++)
                A[i] = element_ctor(0);

        randomizeArray(A, 10);
        printArray(A, 10);

        insertSort(A, 10, element_comp_reverse);
        printArray(A, 10);
        assert(checkOrder(A, 10, element_comp_reverse));
        printf("%s\n", checkOrder(A, 10, element_comp_reverse) ? "true" : "false");

        randomizeArray(A, 10);
        printArray(A, 10);

        shellSort(A, 10, element_comp_reverse);
        printArray(A, 10);
        assert(checkOrder(A, 10, element_comp_reverse));
        printf("%s\n", checkOrder(A, 10, element_comp_reverse) ? "true" : "false");

        randomizeArray(A, 10);
        printArray(A, 10);

        mergeSort(A, 10, element_comp_reverse);
        printArray(A, 10);
        assert(checkOrder(A, 10, element_comp_reverse));
        printf("%s\n", checkOrder(A, 10, element_comp_reverse) ? "true" : "false");

        randomizeArray(A, 10);
        printArray(A, 10);
        quickSort(A, 10, element_comp_reverse);
        printArray(A, 10);
        assert(checkOrder(A, 10, element_comp_reverse));
        printf("%s\n", checkOrder(A, 10, element_comp_reverse) ? "true" : "false");

        randomizeArray(A, 10);
        printArray(A, 10);
        quickSort(A, 10, element_comp);
        printArray(A, 10);
        assert(checkOrder(A, 10, element_comp));
        printf("%s\n", checkOrder(A, 10, element_comp) ? "true" : "false");
        return 0;
}
