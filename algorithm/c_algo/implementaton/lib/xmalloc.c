/* xmalloc.c -- malloc with out of memory checking

   Copyright (C) 1990-2000, 2002-2006, 2008-2019 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#include "xmalloc.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void xalloc_die(void) {
  perror("memory exhausted!");
  abort();
}

/* Allocate N bytes of memory dynamically, with error checking.  */

void *
xmalloc (size_t n)
{
  void *p = malloc(n);
  if (!p && n != 0)
    xalloc_die();
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.  */

void *
xrealloc (void *p, size_t n)
{
  if (!n && p)
    {
      /* The GNU and C99 realloc behaviors disagree here.  Act like
         GNU, even if the underlying realloc is C99.  */
      free (p);
      return NULL;
    }

  p = realloc (p, n);
  if (!p && n)
    xalloc_die ();
  return p;
}

/* Allocate S bytes of zeroed memory dynamically, with error checking.
   There's no need for xnzalloc (N, S), since it would be equivalent
   to xcalloc (N, S).  */

void *
xzalloc (size_t s)
{
  return memset (xmalloc (s), 0, s);
}

/* Allocate zeroed memory for N elements of S bytes, with error
   checking.  S must be nonzero.  */

void *
xcalloc (size_t n, size_t s)
{
  void *p;
  /* Test for overflow, since objects with size greater than
     PTRDIFF_MAX cause pointer subtraction to go awry.  Omit size-zero
     tests if HAVE_GNU_CALLOC, since GNU calloc never returns NULL if
     successful.  */
  if (xalloc_oversized (n, s)
      || (! (p = calloc (n, s)) && (n != 0)))
    xalloc_die ();
  return p;
}

/* Clone an object P of size S, with error checking.  There's no need
   for xnmemdup (P, N, S), since xmemdup (P, N * S) works without any
   need for an arithmetic overflow check.  */

void *
xmemdup (void const *p, size_t s)
{
  return memcpy (xmalloc (s), p, s);
}

/* Clone STRING.  */

char *
xstrdup (char const *string)
{
  return xmemdup (string, strlen (string) + 1);
}
