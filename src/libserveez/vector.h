/*
 * vector.h - simple vector list declarations
 *
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: vector.h,v 1.5 2001/12/13 18:00:01 ela Exp $
 *
 */

#ifndef __VECTOR_H__
#define __VECTOR_H__ 1

#include "defines.h"

/* Definition of an vector structure. */
typedef struct
{
  unsigned long length;     /* number of elements in this vector */
  unsigned long chunk_size; /* size of each element */
  void *chunks;             /* pointer to first element */
}
svz_vector_t;

/*
 * Iteration macro for the vector list @var{vector}. Each of its values
 * gets assigned to @var{value}. The iteration variable @var{i} runs from
 * 0 to the size-1 of the vector list.
 */
#define svz_vector_foreach(vector, value, i)                            \
  for ((i) = 0, (value) = vector ? svz_vector_get ((vector), 0) : NULL; \
       vector && (unsigned long) i < svz_vector_length (vector);        \
       (value) = svz_vector_get ((vector), ++(i)))

__BEGIN_DECLS

/*
 * A vector list is an array of memory chunks with a fixed size. It
 * holds copies of the values you added to the vector list. When deleting
 * or inserting an element the indexes of the following elements get
 * either decremented or incremented.
 */

SERVEEZ_API svz_vector_t *svz_vector_create (unsigned long);
SERVEEZ_API void svz_vector_destroy (svz_vector_t *);
SERVEEZ_API unsigned long int svz_vector_clear (svz_vector_t *);
SERVEEZ_API unsigned long svz_vector_add (svz_vector_t *, void *);
SERVEEZ_API void *svz_vector_get (svz_vector_t *, unsigned long);
SERVEEZ_API void *svz_vector_set (svz_vector_t *, unsigned long, void *);
SERVEEZ_API unsigned long svz_vector_del (svz_vector_t *, unsigned long);
SERVEEZ_API unsigned long svz_vector_ins (svz_vector_t *, unsigned long,
                                          void *);
SERVEEZ_API unsigned long svz_vector_idx (svz_vector_t *, void *);
SERVEEZ_API unsigned long svz_vector_contains (svz_vector_t *, void *);
SERVEEZ_API unsigned long svz_vector_length (svz_vector_t *);

__END_DECLS

#endif /* not __VECTOR_H__ */
