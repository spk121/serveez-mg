/*
 * vector.c - simple vector list implementation
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
 * $Id: vector.c,v 1.4 2001/07/02 11:46:34 ela Exp $
 *
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "util.h"
#include "vector.h"

/*
 * Create a new vector list without elements. Each element will have
 * the given size @var{size} in bytes.
 */
svz_vector_t *
svz_vector_create (unsigned long size)
{
  svz_vector_t *vec;

  if (size == 0)
    return NULL;
  vec = svz_malloc (sizeof (svz_vector_t));
  memset (vec, 0, sizeof (svz_vector_t));
  vec->chunk_size = size;
  return vec;
}

/*
 * Destroy a given vector list @var{vec}. This pointer is invalid afterwards.
 * The routine @code{svz_free()}s all elements.
 */
void
svz_vector_destroy (svz_vector_t *vec)
{
  if (vec->length && vec->chunks)
    svz_free (vec->chunks);
  svz_free (vec);
}

/*
 * Delete all elements of the given vector list @var{vec}. What you will 
 * have then is an empty vector list. Returns the previous length.
 */
unsigned long
svz_vector_clear (svz_vector_t *vec)
{
  unsigned long length = vec->length;

  if (length && vec->chunks)
    svz_free (vec->chunks);
  vec->chunks = NULL;
  vec->length = 0;
  return length;
}

/*
 * Add an element to the end of the given vector list @var{vec}. Return the
 * position the element got. @var{value} is a pointer to a chunk of the
 * vector lists chunk size.
 */
unsigned long
svz_vector_add (svz_vector_t *vec, void *value)
{
  vec->chunks = svz_realloc (vec->chunks, 
			     vec->chunk_size * (vec->length + 1));
  memcpy ((char *) vec->chunks + vec->chunk_size * vec->length, value, 
	  vec->chunk_size);
  vec->length++;

  return (vec->length - 1);
}

/*
 * Get an vector list element of the vector list @var{vec} at the given
 * position @var{index}. Return @code{NULL} if the index is out of range.
 */
void *
svz_vector_get (svz_vector_t *vec, unsigned long index)
{
  if (index < vec->length)
    return ((char *) vec->chunks + index * vec->chunk_size);
  return NULL;
}

/*
 * Overwrite the element at index @var{index} in the vector list @var{vec} 
 * with the given value @var{value}. Return @code{NULL} if the index is out 
 * of range or the pointer to the new element.
 */
void *
svz_vector_set (svz_vector_t *vec, unsigned long index, void *value)
{
  void *p;

  if (index >= vec->length)
    return NULL;
  p = (char *) vec->chunks + vec->chunk_size * index;
  memcpy ((char *) p, value, vec->chunk_size);
  return p;
}

/*
 * Delete the element of the vector @var{vec} at the position @var{index}. 
 * Return -1 if the given index is out of range otherwise the new length 
 * of the vector list.
 */
unsigned long
svz_vector_del (svz_vector_t *vec, unsigned long index)
{
  char *p;

  if (index >= vec->length)
    return (unsigned long) -1;

  /* delete all elements */
  if (vec->length == 1)
    {
      svz_free (vec->chunks);
      vec->chunks = NULL;
    }
  /* delete last element */
  else if (vec->length - 1 == index)
    {
      vec->chunks = svz_realloc (vec->chunks, vec->chunk_size * index);
    }
  /* delete element within the chunk */
  else
    {
      p = (char *) vec->chunks + vec->chunk_size * index;
      memmove ((char *) p, (char *) p + vec->chunk_size, 
	       (vec->length - index - 1) * vec->chunk_size);
      vec->chunks = svz_realloc (vec->chunks, 
				 vec->chunk_size * (vec->length - 1));
    }
  vec->length--;
  return vec->length;
}

/*
 * Insert the given element @var{value} into the vector list @var{vec} at 
 * the position @var{index}. Return the new length of the vector list or 
 * -1 if the index is out of range.
 */
unsigned long
svz_vector_ins (svz_vector_t *vec, unsigned long index, void *value)
{
  char *p;

  if (index > vec->length)
    return (unsigned long) -1;

  /* resize the chunk */
  vec->length++;
  vec->chunks = svz_realloc (vec->chunks, vec->chunk_size * vec->length);

  /* append at vector lists end */
  if (vec->length == index)
    {
      memcpy ((char *) vec->chunks + vec->chunk_size * index, value,
	      vec->chunk_size);
    }
  /* insert element into the chunk */
  else
    {
      p = (char *) vec->chunks + vec->chunk_size * index;
      memmove ((char *) p + vec->chunk_size, (char *) p,
	       (vec->length - index - 1) * vec->chunk_size);
      memcpy ((char *) p, value, vec->chunk_size);
    }
  return vec->length;
}

/*
 * Find the given value @var{value} in the vector list @var{vec}. Return -1 
 * if there is no such element or the index of the element.
 */
unsigned long
svz_vector_idx (svz_vector_t *vec, void *value)
{
  unsigned long index;
  char *p;

  if (value == NULL || vec->length == 0)
    return (unsigned long) -1;

  p = vec->chunks;
  for (index = 0; index < vec->length; index++, p += vec->chunk_size)
    {
      if (!memcmp (p, value, vec->chunk_size))
	return index;
    }
  return (unsigned long) -1;
}

/*
 * Return how often the vector list @var{vec} contains the element given
 * in @var{value}.
 */
unsigned long
svz_vector_contains (svz_vector_t *vec, void *value)
{
  char *p;
  unsigned long found = 0, index;

  if (value == NULL || vec->length == 0)
    return found;

  p = vec->chunks;
  for (index = 0; index < vec->length; index++, p += vec->chunk_size)
    {
      if (!memcmp (p, value, vec->chunk_size))
	found++;
    }
  return found;
}

/*
 * Return the current length of the vector list @var{vec}.
 */
unsigned long
svz_vector_length (svz_vector_t *vec)
{
  assert (vec);
  return vec->length;
}
