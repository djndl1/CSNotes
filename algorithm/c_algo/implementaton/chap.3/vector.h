#pragma once

#include "../lib/xmalloc.h"

#include "../lib/utils.h"

#include <string.h>

#define CTOR(type) type##_ctor_t
#define DTOR(type) type##_dtor_t

#define vec_type_register(type)                                                \
  typedef type *(*type##_ctor_t)(void *);                                      \
  typedef void (*type##_dtor_t)(type *);                                       \
  struct type##Vec;                                                            \
  typedef struct type##Vec type##_vec_t;                                       \
  struct type##Vec {                                                           \
    type *data;                                                                \
                                                                               \
    size_t elem_size;                                                          \
    CTOR(type) elem_ctor;                                                      \
    DTOR(type) elem_dtor;                                                      \
                                                                               \
    size_t sz;                                                                 \
    size_t space;                                                              \
                                                                               \
    void (*dtor)(type##_vec_t *);                                              \
    void (*push_back)(type##_vec_t*, type);                             \
    };                                                                  \
                                                                        \
  void type##_vector_dtor(type##_vec_t *self)                           \
  { \
      if (self->elem_dtor) \
          for_each(type, ptr, *self) \
              self->elem_dtor(ptr); \
      free(self->data); \
  } \
  \
  void type##_vector_push_back(type##_vec_t *self, type value) \
  { \
      if (self->space <= self->sz) { \
          type* d_tmp = xcalloc(2*self->sz, sizeof(type)); \
          memcpy(d_tmp, self->data, sizeof(type) * self->sz ); \
          d_tmp[self->sz++] = value;                             \
                                                                 \
          self->space *= 2; \
          self->dtor(self); \
          self->data = d_tmp; \
      } else                                         \
          self->data[self->sz++] = value; \
  }

#define vector_ops(type, ops) \
    (type##_vector_##ops)

#define vec_t(type, size, constr, destr, var)            \
    type##_vec_t  var; \
    (var).data = xcalloc(size, sizeof(type)); \
    (var).sz = size; \
    (var).space = size;                 \
    (var).elem_size = sizeof(type); \
    (var).elem_ctor = constr; \
    (var).elem_dtor = destr; \
    (var).dtor = type##_vector_dtor;                \
        (var).push_back = type##_vector_push_back

vec_type_register(int);
