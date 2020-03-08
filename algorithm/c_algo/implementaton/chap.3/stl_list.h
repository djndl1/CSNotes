#pragma once

#include "../lib/xmalloc.h"
#include "utils.h"

#include <string.h>

#define list_type_register(type)                                               \
  struct type##list_node;                                                      \
  typedef struct type##list_node type##_list_node_t;                           \
                                                                               \
  struct type##list_node {                                                     \
    type data;                                                                 \
    type##_list_node_t *prev;                                                  \
    type##_list_node_t *next;                                                  \
  };                                                                           \
                                                                               \
  type##_list_node_t *type##_list_node_new(                                    \
      type data, type##_list_node_t *prev, type##_list_node_t *next) {         \
    type##_list_node_t *n = xmalloc(sizeof(type##_list_node_t));               \
    n->data = data;                                                            \
    n->prev = prev;                                                            \
    n->next = next;                                                            \
                                                                               \
    return n;                                                                  \
  }                                                                            \
                                                                               \
  struct type##list;                                                           \
  typedef struct type##list type##_list_t;                                     \
                                                                               \
  struct type##list {                                                          \
    type##_list_node_t *head;                                                  \
    type##_list_node_t *tail;                                                  \
                                                                               \
    size_t sz;                                                                 \
                                                                               \
    size_t elem_size;                                                          \
    void (*elem_dtor)(type *);                                                 \
  };                                                                           \
                                                                               \
  type##_list_t type##_list_new(void (*elem_dtor)(type *)) {                   \
    type##_list_node_t *dummy = xmalloc(sizeof(type##_list_node_t));           \
    dummy->prev = dummy;                                                       \
    dummy->next = NULL;                                                        \
                                                                               \
    type##_list_t l = {.head = dummy,                                          \
                       .tail = dummy,                                          \
                       .sz = 0,                                                \
                       .elem_size = sizeof(type),                              \
                       .elem_dtor = elem_dtor};                                \
                                                                               \
    return l;                                                                  \
  }                                                                            \
                                                                               \
  void type##_list_delete(type##_list_t *self) {                               \
    if (self->elem_dtor) {                                                     \
      type##_list_node_t *cur = self->head->next;                              \
      while (cur != NULL) {                                                    \
        self->elem_dtor(&cur->data);                                           \
        type##_list_node_t *n = cur->next;                                     \
        free(cur);                                                             \
        cur = n;                                                               \
      }                                                                        \
    }                                                                          \
    free(self->head);                                                          \
    self->sz = self->elem_size = 0;                                            \
    self->elem_dtor = NULL;                                                    \
  }                                                                            \
                                                                               \
  void type##_list_insert(type##_list_t *self, type##_list_node_t *pos,        \
                          type data) {                                         \
    list_node_new(data, pos->prev, pos, n);                                    \
    pos->prev->next = n;                                                       \
    pos->prev = n;                                                             \
    self->sz++;                                                                \
  }                                                                            \
                                                                               \
  type##_list_node_t *type##_list_erase(type##_list_t *self,                   \
                                        type##_list_node_t *pos) {             \
    if (!pos || self->sz == 0)                                                 \
      return NULL;                                                             \
    type##_list_node_t *prior = pos->prev;                                     \
    type##_list_node_t *posterior = pos->next;                                 \
    prior->next = posterior;                                                   \
    if (posterior)                                                             \
      posterior->prev = prior;                                                 \
    if (self->elem_dtor)                                                       \
      self->elem_dtor(&(pos->data));                                           \
    pos->next = pos->prev = NULL;                                              \
    free(pos);                                                                 \
    self->sz--;                                                                \
                                                                               \
    return posterior;                                                          \
  }                                                                            \
                                                                               \
        void type##_list_reverse(type##_list_t *self)                         \
        {                                                                   \
            if (self->sz <= 1)                                              \
                return;                                                 \
            type##_list_node_t *front = self->head->next;               \
            type##_list_node_t *cur = front;                            \
            while (cur != NULL) {                                       \
                type##_list_node_t *posterior = cur->next;              \
                basic_swap(type##_list_node_t *, front->prev, front->next); \
                cur = posterior;                                        \
            }                                                           \
            front->next = NULL;                                         \
            self->tail->prev = self->head;                              \
            self->head->next = self->tail;                              \
            self->tail = front;                                         \
        }

#define list_node_init(type, data, prev, next, var)                     \
  type##_list_node_t var = {.data = data, .prev = prev, .next = next}

#define list_node_new(type, data, prev, next, var)  \
        type##_list_node_t *var = type##_list_node_new(data, prev, next)

#define list_node_ops(type, op) type##_list_node_##op

#define list_node_next(node) \
    ((node)->next)

#define list_node_prev(node) \
    ((node)->prev)

#define list_for_each_next(type, cur, list)                 \
        for (type##_list_node_t *cur = (list).head->next;  \
             cur != NULL;                                  \
             cur = cur->next)

#define list_for_each_prev(type, list)              \
    for (type##_list_node_t *cur = (list).tail;     \
         cur != list.head;)                         \
        cur = cur->prev)

#define list_begin(list) \
    ((list).head->next)

#define list_end(list) \
    ((list).tail)

#define list_front(list) ((list).head->next.data)

#define list_back(list) ((list).tail.data)

#define list_size(list) \
    ((list).sz)

#define list_empty(list) \
    ((list).sz == 0)

#define list_ops(type, op) \
    type##_list_##op

#define list_init(type, destr, var)              \
    type##_list_t var = list_ops(type, new)(destr)

#define list_delete(type, var) \
    list_ops(type, delete)(&var)

#define list_insert(type, list, data, node)      \
    list_ops(type, insert)(&list, node, data)

#define list_push_back(type, list, data)                                       \
    {                                                                   \
      list_node_new(type, data, list.tail, NULL, n);                    \
      list.tail->next = n;                                              \
      list.tail = n;                                                    \
      list.sz++;                                                        \
    }

#define list_push_front(type, list, data)                                      \
  {                                                                            \
    list_node_new(type, data, list.head, list.head->next, n);                  \
    if (list.head->next)                                                       \
      list.head->next->prev = n;                                               \
    list.head->next = n;                                                       \
  }

#define list_pop_back(type, list)                                              \
    (void) list_ops(type, erase)(&list, list.tail)

#define list_pop_front(type, list)              \
    (void) list_ops(type, erase)(&list, list.head->next)

#define list_erase(type, list, pos)             \
    list_ops(type, erase)(&list, pos)

#define list_reverse(type, list)                \
    list_ops(type, reverse)(&list)

#define list_swap(type, a, b)                                                  \
  {                                                                            \
    basic_swap(type##_list_node_t *, (a).head, (b).head);                      \
    basic_swap(type##_list_node_t *, (a).tail, (b).tail);                      \
    basic_swap(size_t, (a).sz, (b).sz);                                 \
  }
