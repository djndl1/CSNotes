#include "list.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../lib/xmalloc.h"
#include "../lib/errc.h"

struct List {
        node_t dummy_head;
        node_t tail;
        size_t size;
};

struct Node {
        void*    element;
        destructor_t element_destructor;

        node_t    next;
};


node_t node_new(size_t sz, destructor_t destr)
{
        node_t n = xmalloc(sizeof(struct Node));
        if (sz) {
                void *elm = xmalloc(sz);
                n->element = elm;
                n->element_destructor = destr;
        } else {
                n->element = NULL;
                n->element_destructor = NULL;
        }
        n->next = NULL;

        return n;
}

node_t node_copy_new(const void* elem, size_t sz, destructor_t destr)
{
        if (!elem)
                return NULL;
        node_t n = xmalloc(sizeof(struct Node));
        n->element = xmemdup(elem, sz);
        n->element_destructor = destr;
        n->next = NULL;

        return n;
}

void node_destroy(node_t self)
{
        if (!self)
                return;
        if (self->element && self->element_destructor)
                self->element_destructor(self->element);
        free(self->element);
        free(self);
}

void* node_retrieve(node_t self)
{
        return self ? self->element : NULL;
}

list_t list_new()
{
        list_t l = xmalloc(sizeof(struct List));
        node_t dummy = node_new(0, NULL);
        l->tail = l->dummy_head = dummy;
        l->size = 0;

        return l;
}

void list_destroy(list_t self)
{
        if (!self)
                return;

        node_t cur = self->dummy_head;
        while (cur) {
                node_t tmp = cur->next;
                NODE_DESTROY(cur);
                cur = tmp;
        }
        free(self);
}

bool list_is_empty(list_t self)
{
        if (!self)
                return true;
        return (self->tail == self->dummy_head);
}

node_t list_find(list_t self,
                 const void* elem, comparator_t comp_func)
{
        if (!self || list_is_empty(self))
                return NULL;

        for (node_t cur = self->dummy_head->next;
             cur != NULL;
             cur = cur->next) {
                if (comp_func(cur->element, elem) == 0)
                        return cur;
        }
        return NULL;
}

node_t list_index(list_t self, size_t ind)
{
        if (!self || list_is_empty(self) || ind >= self->size)
                return NULL;

        node_t cur = self->dummy_head->next;
        for (size_t i = 0; i < ind; i++) {
                cur = cur->next;
        }

        return cur;
}

int list_insert(list_t self,
                const void* element, size_t sz, destructor_t destr,
                size_t pos)
{
        if (!self || !element || !sz)
                return null_pointer_error;
        if (pos > self->size)
                return out_of_range;
        node_t new_node = node_copy_new(element, sz, destr);
        node_t prev_node = list_index(self, pos);
        new_node->next = prev_node->next;
        prev_node->next = new_node;

        if (pos == self->size)
                self->tail = new_node;
        self->size++;

        return no_error;
}

int list_append(list_t self,
                const void* element, size_t sz, destructor_t destr)
{
        if (!self || !element || !sz)
                return null_pointer_error;

        node_t new_node = node_copy_new(element, sz, destr);
        self->tail->next = new_node;

        self->tail = new_node;
        self->size++;

        return no_error;
}

int list_remove(list_t self, size_t pos)
{
        if (!self)
                return null_pointer_error;
        if (pos >= self->size)
                return out_of_range;

        if (pos == 0) {
                node_t first = self->dummy_head->next;
                self->dummy_head->next = first->next;

                if (first == self->tail)
                        self->tail = self->dummy_head;

                node_destroy(first);
                goto update;
        }

        node_t prev_node = list_index(self, pos-1);
        node_t del_node = prev_node->next;
        prev_node->next = del_node->next;
        if (del_node == self->tail)
                self->tail = prev_node;
        node_destroy(del_node);

update:
        self->size--;

        return no_error;
}

void list_for_each(list_t self, node_func func)
{
        if (!self || list_is_empty(self))
                return;

        for (node_t cur = self->dummy_head->next;
             cur != NULL;
             cur = cur->next)
                func(cur);
}

