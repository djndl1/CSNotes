#include "heap.h"

#include <stdlib.h>
#include <stdbool.h>

struct Element {
        int dummy;
};

static inline int element_comp(const void *l, const void *r)
{
        element_t* lhs = l;
        element_t* rhs = r;
        if (lhs->dummy > rhs->dummy)
                return 1;
        else if (lhs->dummy < rhs->dummy)
                return -1;
        else
                return 0;
}

priority_queue_t priority_queue_ctor(size_t capacity)
{
        if (capacity < 3)
                goto no_result;

        priority_queue_t heap = malloc(sizeof(struct heap));
        if (heap == NULL)
                goto no_result;
        heap->elems = calloc(capacity + 1, sizeof(element_t));
        if (heap->elems == NULL)
                goto alloc_fail;
        heap->size = 0;
        heap->capacity = capacity;

        return heap;
alloc_fail:
        free(heap);
no_result:
        return NULL;
}

bool priority_queue_is_full(priority_queue_t heap)
{
        return heap->size >= heap->capacity;
}

int priority_queue_insert(priority_queue_t heap, element_t elm)
{
        if (priority_queue_is_full(heap)) {
                return 1;
        }

        size_t i;
        for (i = ++heap->size;
             element_comp(&heap->elems[i/2], &elm) > 0 || i == 1;
             i /= 2)
                heap->elems[i] = heap->elems[i/2];
        heap->elems[i] = elm;

        return 0;
}

bool priority_queue_is_empty(priority_queue_t heap)
{
        return heap->size == 0;
}

element_t priority_queue_delete_min(priority_queue_t heap)
{
        if (priority_queue_is_empty(heap))
                return heap->elems[0];
        element_t min = heap->elems[1];
        element_t last = heap->elems[heap->size--];

        size_t i, child;
        for (i = 1; i * 2 <= heap->size; i = child) {
                child =  i * 2;
                if ( child != heap->size &&
                     element_comp(&heap->elems[child+1], &heap->elems[child]) < 0)
                        child++;

                if (element_comp(&last, &heap->elems[child]) > 0)
                        heap->elems[i] = heap->elems[child];
                else
                        break;
        }
        heap->elems[i] = last;
        return min;
        
}

void priority_queue_make_heap_order(priority_queue_t heap)
{
        if (priority_queue_is_empty(heap))
                return;
        element_t root = heap->elems[1];
        size_t i, child;
        for (i = 1; i * 2 <= heap->size; i = child) {
                child =  i * 2;
                if ( child != heap->size &&
                     element_comp(&heap->elems[child+1], &heap->elems[child]) < 0)
                        child++;

                if (element_comp(&root, &heap->elems[child]) > 0)
                        heap->elems[i] = heap->elems[child];
                else
                        break;
        }
        heap->elems[i] = root;
}
