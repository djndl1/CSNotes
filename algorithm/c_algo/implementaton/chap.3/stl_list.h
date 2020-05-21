#pragma once

#include "../lib/xmalloc.h"
#include "utils.h"

#include <string.h>
#include <stdbool.h>

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
    type##_list_node_t *head = xmalloc(sizeof(type##_list_node_t));            \
    type##_list_node_t *tail = xmalloc(sizeof(type##_list_node_t));            \
    head->prev = head;                                                         \
    head->next = tail;                                                         \
    tail->prev = head;                                                         \
    tail->next = tail;                                                         \
                                                                               \
    type##_list_t l = {.head = head,                                           \
                       .tail = tail,                                           \
                       .sz = 0,                                                \
                       .elem_size = sizeof(type),                              \
                       .elem_dtor = elem_dtor};                                \
                                                                               \
    return l;                                                                  \
  }                                                                            \
                                                                               \
  void type##_list_delete(type##_list_t *self) {                               \
    if (self->elem_dtor) {                                                     \
      list_for_each_next(type, it, *self) self->elem_dtor(&(it->data));        \
    }                                                                          \
    free(self->head);                                                          \
    free(self->tail);                                                          \
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
    posterior->prev = prior;                                            \
    if (self->elem_dtor)                                                       \
      self->elem_dtor(&(pos->data));                                           \
    pos->next = pos->prev = NULL;                                              \
    free(pos);                                                                 \
    self->sz--;                                                                \
                                                                               \
    return posterior;                                                          \
  }                                                                            \
                                                                               \
  void type##_list_reverse(type##_list_t *self) {                              \
    if (self->sz <= 1)                                                         \
      return;                                                                  \
    type##_list_node_t *cur = self->head;                                      \
    while (cur != self->tail) {                                                \
      type##_list_node_t *n = cur->next;                                       \
      basic_swap(type##_list_node_t *, cur->next, cur->prev);                  \
      cur = n;                                                                 \
    }                                                                          \
    basic_swap(type##_list_node_t *, cur->next, cur->prev);                    \
                                                                               \
    basic_swap(type##_list_node_t *, self->head, self->tail);                  \
  }                                                                            \
                                                                               \
  void type##_list_splice(type##_list_t *self, type##_list_node_t *pos,        \
                          type##_list_t *other,                         \
                          type##_list_node_t *first, type##_list_node_t *last) { \
    if (!other || !first || first == other.head || !pos || (last == first) ||  \
        other.sz == 0)                                                         \
      return;                                                                  \
                                                                               \
    size_t len = 0;                                                            \
    bool range_avail = true;                                                   \
    for (type##_list_node_t *it = first; it != last; it = it->next) {          \
      if (it == self->tail) {                                                  \
        range_avail = false;                                                   \
        len++;                                                                 \
        break;                                                                 \
      }                                                                        \
      len++;                                                                   \
    }                                                                          \
    if (!range_avail)                                                   \
            last = other->tail;                                         \
    type##_list_node_t *tmp_tail = last->prev;                          \
                                                                        \
    type##_list_node_t *cut_other = first->prev;                        \
    cut_other->next = last;                                             \
    last->prev = cut_other;                                             \
    other.sz -= len;                                                    \
                                                                               \
    type##_list_node_t *right_cut = pos->next;                          \
    pos->next = first;                                                  \
    first->prev = pos;                                                  \
    tmp_tail->next = right_cut;                                         \
    right_cut->prev = tmp_tail;                                         \
    self.sz += len;                                                     \
  }                                                                     \
                                                                        \
  void type##_list_unique(type##_list_t* self, type##_binary_predicate pred) \
  {                                                                     \
          type##_list_node_t* cur = self->head->next;                   \
          while (cur != self->tail) {                                   \
                  data cur_data = cur->data;                            \
                                                                        \
                  while (cur->next != self->tail && pred(cur_data, cur->next->data)) { \
                          list_erase(type, *self, cur->next);           \
                  }                                                     \
                  cur = cur->next;                                      \
          }                                                             \
  }

#define list_node_new(type, data, prev, next, var)  \
        type##_list_node_t *var = type##_list_node_new(data, prev, next)

#define list_node_ops(type, op) type##_list_node_##op

#define list_node_next(node) \
    ((node)->next)

#define list_node_prev(node) \
    ((node)->prev)

#define list_node_data(node)                    \
        ((node)->data)

/**
 * Internally, `cur` is the pointer to the list node. Since the data
 * is at the beginning of the node, the `cur` pointer also points to
 * the data stored in there. Use a cast or `list_node_data` to access
 * the data
 */
#define list_for_each_next(type, cur, list)                 \
        for (type##_list_node_t *cur = (list).head->next,;          \
             cur != (list).tail;                                    \
             cur = cur->next)

#define list_for_each_prev(type, cur, list)                \
    for (type##_list_node_t *cur = (list).tail->prev;     \
         cur != list.head;)                         \
        cur = cur->prev)

#define list_begin(list)                        \
    ((list).head->next)

#define list_end(list)                          \
    ((list).tail)

#define list_rbegin(list)                       \
        ((list).tail->prev)

#define list_rend(list)                                 \
        ((list).head)

#define list_front(list) ((list).head->next.data)

#define list_back(list) (((list).tail->prev).data)

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

#define list_push_back(type, list, data)        \
          list_ops(type, insert)(&list, (list).tail, data)

#define list_push_front(type, list, data)                                      \
        list_ops(type, insert)(&list, (list).tail->prev, data)

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

#define list_splice(type, list, pos, other, first, last)     \
        list_ops(type, splice)(&list, pos, &other, first, last)

