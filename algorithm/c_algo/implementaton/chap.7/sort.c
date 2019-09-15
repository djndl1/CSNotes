#include "sort.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>


element_t element_ctor(int dummy)
{
        element_t elm = malloc(sizeof(struct Element));
        if (elm == NULL)
                return NULL;
        elm->dummy = dummy;

        return elm;
}



void insertSort(element_t elms[], size_t n, comp_t element_comp)
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


void shellSort(element_t elms[], size_t n, comp_t element_comp)
{
        for (size_t inc = n / 2; inc > 0; inc /= 2) {
                for (size_t i = inc; i < n; i++) {
                        element_t tmp = elms[i];

                        size_t j;
                        for (j = i; j >= inc; j -= inc)
                                if (element_comp(tmp, elms[j-inc]) < 0)
                                        elms[j] = elms[j-inc];
                                else
                                        break;
                        elms[j] = tmp;
                }
        }
}

void printElement(const element_t elm)
{
        printf("%d", elm->dummy);
}


static void merge(element_t A[], element_t tmp[],
                  size_t lpos, size_t rpos, size_t rend,
                  comp_t element_comp)
{
        size_t lend = rpos - 1;
        size_t tpos = lpos;
        size_t num = rend - lpos + 1;

        while (lpos <= lend && rpos <= rend) {
                if (element_comp(A[lpos], A[rpos]) <= 0)
                        tmp[tpos++] = A[lpos++];
                else
                        tmp[tpos++] = A[rpos++];
        }

        while (lpos <= lend) {
                tmp[tpos++] = A[lpos++];
        }

        while (rpos <= rend) {
                tmp[tpos++] = A[rpos++];
        }

        for (size_t i = 0; i < num; i++, rend--)
                A[rend] = tmp[rend];
}

static void __mergeSort(element_t A[], element_t tmp[],
                        size_t left, size_t right, comp_t element_comp)
{
        if (left < right) {
                size_t center = (left + right) / 2;
                __mergeSort(A, tmp, left, center, element_comp);
                __mergeSort(A, tmp, center+1, right, element_comp);
                merge(A, tmp, left, center+1, right, element_comp);
        }
}

int mergeSort(element_t A[], size_t n, comp_t element_comp)
{
        element_t *tmp = malloc(n * sizeof(element_t));
        if (tmp == NULL)
                return -1;
        __mergeSort(A, tmp, 0, n-1, element_comp);
        free(tmp);

        return 0;
}

void element_swap(element_t *lhs, element_t *rhs)
{
        element_t tmp = *lhs;
        *lhs = *rhs;
        *rhs = tmp;
}

/**
 * sort left, center, right in place
 */
element_t __median3(element_t A[], size_t left, size_t right, comp_t element_comp)
{
        size_t center = (left + right) / 2;

        if (element_comp(A[left], A[center]) > 0)
                element_swap(&A[left], &A[center]);
        if (element_comp(A[left], A[right]) > 0)
                element_swap(&A[left], &A[right]);
        if (element_comp(A[center], A[right]) > 0)
                element_swap(&A[center], &A[right]);

        // A[left] <= A[center] <= A[right]

        element_swap(&A[center], &A[right-1]); // hide pivot

        return A[right-1];                      // return pivot
}

void __quickSort(element_t A[], size_t left, size_t right, comp_t element_comp)
{
        if (left + 3 <= right) {
                element_t pivot = __median3(A, left, right, element_comp);
                size_t i = left;
                size_t j = right - 1;
                for ( ; ; ) {
                        while (element_comp(A[++i], pivot) < 0);
                        while (element_comp(A[--j], pivot) > 0);
                        if (i < j)
                                element_swap(&A[i], &A[j]);
                        else
                                break;
                }
                element_swap(&A[i], &A[right-1]);

                __quickSort(A, left, i-1, element_comp);
                __quickSort(A, i+1, right, element_comp);
        } else
                insertSort(A + left, right - left + 1, element_comp);
}

void quickSort(element_t A[], size_t n, comp_t element_comp)
{
        __quickSort(A, 0, n - 1, element_comp);
}
