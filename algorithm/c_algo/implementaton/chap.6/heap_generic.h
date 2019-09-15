#pragma once

#include <stddef.h>

struct Element;
typedef struct Element element_t;

struct Element {
    int dummy;
};

struct heap {
        size_t          capacity;
        size_t          size;
        element_t      *elems;
};

typedef int (comp_t)(const void*, const void*);

typedef struct heap *priority_queue_t;

priority_queue_t priority_queue_ctor(size_t capacity);

int priority_queue_insert(priority_queue_t heap, element_t elm, comp_t element_comp);

element_t priority_queue_delete_min(priority_queue_t heap, comp_t element_comp);

void priority_queue_sort_heap(priority_queue_t heap, comp_t element_comp);

void priority_queue_print(priority_queue_t heap);

void priority_queue_make_heap_order(priority_queue_t heap, comp_t element_comp);
