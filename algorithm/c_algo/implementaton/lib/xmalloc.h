#pragma once

#include <stdint.h>
#include <stdlib.h>

#include <stddef.h>
#include <stdint.h>

#define xalloc_oversized(n, s)                                               \
  ((size_t)(PTRDIFF_MAX < SIZE_MAX ? PTRDIFF_MAX : SIZE_MAX - 1) / (s) < (n))

#ifdef __cplusplus
extern "C" {
#endif

extern _Noreturn void xalloc_die(void);

void *xmalloc(size_t s);

void *xzalloc(size_t s);

void *xcalloc(size_t n, size_t s);

void *xrealloc(void *p, size_t s);

void *x2realloc(void *p, size_t *pn);

void *xmemdup(void const *p, size_t s);

char *xstrdup(char const *str);


/* In the following macros, T must be an elementary or structure/union or
   typedef'ed type, or a pointer to such a type.  To apply one of the
   following macros to a function pointer or array type, you need to typedef
   it first and use the typedef name.  */

/* Allocate an object of type T dynamically, with error checking.  */
/* extern t *XMALLOC (typename t); */
#define XMALLOC(t) ((t *) xmalloc (sizeof (t)))

/* Allocate memory for N elements of type T, with error checking.  */
/* extern t *XNMALLOC (size_t n, typename t); */
#define XCALLOC(n, t) \
   ((t *) (sizeof (t) == 1 ? xmalloc (n) : xcalloc (n, sizeof (t))))

/* Allocate an object of type T dynamically, with error checking,
   and zero it.  */
/* extern t *XZALLOC (typename t); */
#define XZALLOC(t) ((t *) xzalloc (sizeof (t)))

/* Allocate memory for N elements of type T, with error checking,
   and zero it.  */
/* extern t *XCALLOC (size_t n, typename t); */
#define XZCALLOC(n, t) \
   ((t *) (sizeof (t) == 1 ? xzalloc (n) : xcalloc (n, sizeof (t))))


