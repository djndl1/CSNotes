#include "heap_generic.h"

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>


static inline int element_dummy_comp(const void *l, const void *r)
{
        const element_t* lhs = l;
        const element_t* rhs = r;
        if (lhs->dummy > rhs->dummy)
                return 1;
        else if (lhs->dummy < rhs->dummy)
                return -1;
        else
                return 0;
}

static inline void element_print(const void *self)
{
        const element_t* __self = self;
        printf("%d", __self->dummy);
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

bool priority_queue_is_empty(priority_queue_t heap)
{
        return heap->size == 0;
}

int priority_queue_insert(priority_queue_t heap, element_t elm, comp_t element_comp)
{
        if (priority_queue_is_full(heap)) {
                return 1;
        }

        if (priority_queue_is_empty(heap)) {
                heap->elems[++heap->size] = elm;
                return 0;
        }

        size_t i;
        for (i = ++heap->size;
             element_comp(&heap->elems[i/2], &elm) > 0;
             i /= 2) {
                if (i == 1)
                        break;
                heap->elems[i] = heap->elems[i/2];
        }
        heap->elems[i] = elm;

        return 0;
}

element_t priority_queue_delete_min(priority_queue_t heap, comp_t element_comp)
{
        if (priority_queue_is_empty(heap))
                return heap->elems[0];
        element_t min = heap->elems[1];
        element_t last = heap->elems[heap->size--];

        size_t i, child;
        for (i = 1; i*2 <= heap->size; i = child) {
                child =  i * 2;
                if ( child < heap->size &&
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

void priority_queue_adjust_root(priority_queue_t heap, size_t cur_root, comp_t element_comp)
{
        if (priority_queue_is_empty(heap))
                return;
        element_t root = heap->elems[cur_root];
        size_t i, child;
        for (i = cur_root; i * 2 <= heap->size; i = child) {
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

void priority_queue_make_heap_order(priority_queue_t heap, comp_t element_comp)
{
        for (size_t i = heap->size / 2; i > 0; i--)
                priority_queue_adjust_root(heap, i, element_comp);
}

void priority_queue_sort_heap(priority_queue_t heap, comp_t element_comp)
{
        size_t old_size = heap->size;
        for (size_t i = 0; i < old_size; i++) {
                element_t min = priority_queue_delete_min(heap, element_comp);
                //priority_queue_print(heap);
                heap->elems[heap->size+1] = min;
        }
        heap->size = old_size;
}

void priority_queue_print(priority_queue_t heap)
{
        for (size_t i = 1; i <= heap->size; i++) {
                element_print(&heap->elems[i]);
                putchar(' ');
        }
}
