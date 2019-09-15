#pragma once

#include <stddef.h>

struct Element {
    int dummy;
};

typedef struct Element *element_t;

typedef int (*comp_t)(const void*, const void*);

void insertSort(element_t elms[], size_t n, comp_t element_comp);

void shellSort(element_t elms[], size_t n, comp_t element_comp);

void printElement(element_t elm);

element_t element_ctor(int dummy);

int mergeSort(element_t A[], size_t n, comp_t element_comp);

void quickSort(element_t A[], size_t n, comp_t element_comp);
