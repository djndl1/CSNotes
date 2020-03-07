#pragma once

#include "../lib/xmalloc.h"
#include "utils.h"

#include <string.h>

#define vector_for_each(idx, vec) for (size_t idx = 0; idx < (vec).sz; idx++)

#define vector_for_range(lower, upper, idx)                                    \
  for (size_t idx = lower; idx < upper; idx++)

#define vector_for_each_in(type, iter, vec)                                    \
  for (type *iter = (vec).data; iter != (vec).data + (vec).sz; iter++)

#define vector_type_register(type)                                             \
  struct type##vector;                                                         \
  typedef struct type##vector type##_vector_t;                                 \
                                                                               \
  struct type##vector {                                                        \
    type *data;                                                                \
                                                                               \
    size_t sz;                                                                 \
    size_t space;                                                              \
                                                                               \
    void (*elem_dtor)(type *);                                                 \
    size_t elem_size;                                                          \
  };                                                                           \
                                                                               \
  void type##_vector_reserve(type##_vector_t *self, size_t new_cap) {          \
    if (self->space >= new_cap)                                                \
      return;                                                                  \
                                                                               \
    self->data = xrealloc(self->data, sizeof(type) * new_cap);                 \
    self->space = new_cap;                                                     \
  }                                                                            \
                                                                               \
  void type##_vector_insert(type##_vector_t *self, size_t pos, type value) { \
      if (pos > self->sz)                                               \
          return;                                                       \
      if (self->space <= self->sz)                                      \
          vector_reserve(type, *self, 2 * self->space);                 \
      for (size_t i = self->sz; i > pos; i--)                           \
          self->data[i] = self->data[i - 1];                            \
      self->data[pos] = value;                                          \
      self->sz++;                                                       \
}                                                                       \
                                                                        \
  void type##_vector_erase(type##_vector_t *self, size_t pos)           \
  {                                                                     \
      if (self->sz == 0 || pos > self->sz)                              \
          return;                                                       \
      if (self->elem_dtor)                                              \
          self->elem_dtor(&self->data[pos]);                            \
      vector_for_range(pos, --self->sz, i)                                \
          self->data[i] = self->data[i+1];                              \
  }

#define vector_capacity(vec) ((vec).space)

#define vector_size(vec) ((vec).sz)

#define vector_data(vec) ((vec).data)

#define vector_at(vec, i) ((vec).data[i])

#define vector_begin(vec) ((vec).data)

#define vector_end(vec) ((vec).data + (vec).sz)

#define vector_front(vec) ((vec).data[0])

#define vector_back(vec) ((vec).data[(vec).sz-1])

#define vector_empty(vec) ((vec).sz == 0)

#define vector_clear(type, vec)                                         \
    {                                                                   \
        if ((vec).elem_dtor)                                            \
            vector_for_each_in(type, iter, vec) (vec).elem_dtor(iter);  \
        (vec).sz = 0;                                                   \
    }

#define vector_init(type, size, destr, var)                             \
  type##_vector_t var;                                            \
  (var).data = xcalloc(size, sizeof(type));                                    \
  (var).sz = size;                                                             \
  (var).space = size;                                                          \
  (var).elem_size = sizeof(type);                                              \
  (var).elem_dtor = destr

#define vector_delete(type, var)                 \
    {                                            \
        if ((var).elem_dtor)                     \
            vector_for_each_in(type, iter, var)  \
                (var).elem_dtor(iter);           \
        free((var).data);                        \
    }

#define vector_ops(type, op) \
    type##_vector_##op

#define vector_reserve(type, var, new_cap) \
    vector_ops(type, reserve)(&var, new_cap)

#define vector_insert(type, var, pos, value) \
    vector_ops(type, insert)(&var, pos, value)

#define vector_erase(type, var, pos)\
    vector_ops(type, erase)(&var, pos)

#define vector_push_back(type, var, val) \
    vector_insert(type, var, (var).sz, val)

#define vector_pop_back(type, var)\
    vector_erase(type, var, (var).sz-1)

#define vector_swap(type, a, b) \
    {                           \
        basic_swap(type*, (a).data, (b).data);  \
        basic_swap(size_t, (a).sz, (b).sz);     \
        basic_swap(size_t, (a).space, (b).space);   \
    }
