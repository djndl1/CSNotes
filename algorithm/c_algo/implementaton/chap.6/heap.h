#pragma once

#include <stddef.h>

struct Element;
typedef struct Element element_t;

struct heap {
        size_t          capacity;
        size_t          size;
        element_t      *elems;
};

typedef struct heap *priority_queue_t;

priority_queue_t priority_queue_ctor(size_t capacity);

int priority_queue_insert(priority_queue_t heap, element_t elm);
