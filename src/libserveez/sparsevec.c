/*
 * sparsevec.c - sparse vector functions
 *
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: sparsevec.c,v 1.4 2001/09/11 15:05:48 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/sparsevec.h"

/* check if a given sparse vector index can be in this chunk */
#define svz_spvec_range_all(SPVEC, IDX) \
  (IDX >= SPVEC->offset && IDX < SPVEC->offset + SVZ_SPVEC_SIZE)

/* check if a given sparse vector index is in this chunk */
#define svz_spvec_range(SPVEC, IDX) \
  (IDX >= SPVEC->offset && IDX < SPVEC->offset + SPVEC->size)

/* define if development code should be included */
#define DEVEL 1

/*
 * Create and initialize a new chunk at a given index @var{offset}.
 * Return this chunk.
 */
static svz_spvec_chunk_t *
svz_spvec_create_chunk (unsigned long offset)
{
  svz_spvec_chunk_t *chunk;

  chunk = svz_malloc (sizeof (svz_spvec_chunk_t));
  memset (chunk, 0, sizeof (svz_spvec_chunk_t));
  chunk->offset = offset;
  return chunk;
}

/*
 * Put the given chunk @var{insert} into the sparse vector @var{spvec}.
 */
static void
svz_spvec_hook (svz_spvec_t *spvec, svz_spvec_chunk_t *insert)
{
  svz_spvec_chunk_t *chunk, *next;

  /* find the appropriate chunk */
  for (chunk = spvec->first; chunk; chunk = chunk->next)
    {
      if (insert->offset > chunk->offset)
	{
	  next = chunk->next;
	  /* really insert the chunk */
	  if (next && insert->offset <= next->offset)
	    {
	      insert->next = next;
	      insert->prev = chunk;
	      chunk->next = insert;
	      next->prev = insert;
	      return;
	    }
	  /* append at the end of chunks */
	  else if (!next)
	    {
	      chunk->next = insert;
	      insert->prev = chunk;
	      spvec->last = insert;
	      return;
	    }
	}
    }

  /* no chunk found, thus INSERT gets the first chunk */
  insert->next = spvec->first;
  if (spvec->first)
    spvec->first->prev = insert;
  spvec->first = insert;
  if (!spvec->last)
    spvec->last = insert;
}

/*
 * Cut the given chunk @var{delete} off the sparse vector @var{spvec} chain.
 */
static void
svz_spvec_unhook (svz_spvec_t *spvec, svz_spvec_chunk_t *delete)
{
  if (spvec->first == delete)
    {
      spvec->first = delete->next;
      if (spvec->first)
	spvec->first->prev = NULL;
      if (spvec->last == delete)
	{
	  spvec->last = NULL;
	  spvec->length = spvec->size = 0;
	}
      return;
    } 
  if (spvec->last == delete)
    {
      spvec->last = delete->prev;
      if (spvec->last)
	{
	  spvec->last->next = NULL;
	  spvec->length = spvec->last->offset + spvec->last->size;
	}
      else
	spvec->length = 0;
      return;
    }
  delete->prev->next = delete->next;
  delete->next->prev = delete->prev;
}

/*
 * Try to find a given sparse vector chunk @var{index} in the sparse vector 
 * chunks as fast as possible and return it.
 */
static svz_spvec_chunk_t *
svz_spvec_find_chunk (svz_spvec_t *spvec, unsigned long index)
{
  svz_spvec_chunk_t *chunk = NULL;

  /* index larger than list length ? */
  if (index >= spvec->length)
    {
      /* is index available in last chunk ? */
      if (spvec->last && svz_spvec_range_all (spvec->last, index))
	chunk = spvec->last;
    }
  /* start seeking in second half */
  else if (index > spvec->length >> 1)
    {
      for (chunk = spvec->last; chunk; chunk = chunk->prev)
	if (svz_spvec_range_all (chunk, index))
	  break;
    }
  /* start seeking at the start of the list (usual case) */ 
  else
    {
      /* index lesser than offset of first chunk ? */
      chunk = spvec->first;
      if (chunk && index < chunk->offset)
	return NULL;

      for (; chunk; chunk = chunk->next)
	if (svz_spvec_range_all (chunk, index))
	  {
	    if (chunk->next && svz_spvec_range_all (chunk->next, index))
	      continue;
	    break;
	  }
    }
  return chunk;
}

/*
 * Print text representation of the sparse vector @var{spvec}. This 
 * function is for testing and debugging purposes only and should not go 
 * into any distribution.
 */
void
svz_spvec_analyse (svz_spvec_t *spvec)
{
  unsigned long n;
  svz_spvec_chunk_t *chunk;

  for (n = 0, chunk = spvec->first; chunk; n++, chunk = chunk->next)
    {
      fprintf (stdout, 
	       "chunk %06lu at %p, ofs: %06lu, size: %02lu, fill: %08lX, "
	       "prev: %p, next %p\n",
	       n + 1, (void *) chunk, chunk->offset, chunk->size, chunk->fill, 
	       (void *) chunk->prev, (void *) chunk->next);
    }
  fprintf (stdout, "length: %lu, size: %lu, first: %p, last: %p\n", 
	   spvec->length, spvec->size, 
	   (void *) spvec->first, (void *) spvec->last);
}

/*
 * Validate the given sparse vector @var{spvec} and print invalid sparse 
 * vectors. Passing the @var{description} text you can figure out the stage 
 * an error occurred. Return zero if there occurred an error otherwise 
 * non-zero.
 */
static int
svz_spvec_validate (svz_spvec_t *spvec, char *description)
{
  svz_spvec_chunk_t *chunk, *next, *prev;
  unsigned long n = 0, bits;
  int ok = 1;

  /* any valid list ? */
  assert (spvec);

  /* go through all the sparse vector chunks */
  for (n = 0, chunk = spvec->first; chunk; n++, chunk = chunk->next)
    {
      next = chunk->next;
      prev = chunk->prev;

      /* check chain first */
      if ((!next && chunk != spvec->last) || (!prev && chunk != spvec->first))
	{
	  fprintf (stdout, "svz_spvec_validate: invalid last or first\n");
	  ok = 0;
	  break;
	}
      if ((next && next->prev != chunk) || (prev && prev->next != chunk))
	{
	  fprintf (stdout, "svz_spvec_validate: invalid next or prev\n");
	  ok = 0;
	  break;
	}

      /* check chunk size and offset */
      if (next && chunk->offset + chunk->size > next->offset)
	{
	  fprintf (stdout, "svz_spvec_validate: invalid size or offset\n");
	  ok = 0;
	  break;
	}

      /* check chunk chunk fill */
      bits = (1 << chunk->size) - 1;
      if (chunk->fill & ~bits || !(chunk->fill & ((bits + 1) >> 1)) ||
	  chunk->size == 0 || chunk->fill == 0)
	{
	  fprintf (stdout, "svz_spvec_validate: invalid chunk fill\n");
	  ok = 0;
	  break;
	}
    }

  /* check array length */
  chunk = spvec->last;
  if (chunk && chunk->offset + chunk->size != spvec->length)
    {
      fprintf (stdout, "svz_spvec_validate: invalid array length\n");
      ok = 0;
    }

  /* print out error description and sparse vector text representation 
     if necessary */
  if (!ok)
    {
      fprintf (stdout, "error in chunk %06lu (%s)\n", n + 1,
	       description ? description : "unspecified");
      svz_spvec_analyse (spvec);
    }
  return ok;
}

/*
 * Construct an empty sparse vector without initial capacity. Return the
 * newly created sparse vector.
 */
svz_spvec_t *
svz_spvec_create (void)
{
  svz_spvec_t *spvec;

  assert (SVZ_SPVEC_SIZE <= sizeof (unsigned long) * 8);
  spvec = svz_malloc (sizeof (svz_spvec_t));
  memset (spvec, 0, sizeof (svz_spvec_t));
  return spvec;
}

/*
 * Destroy the given sparse vector @var{spvec} completely. The argument 
 * cannot be used afterwards because it is invalid.
 */
void
svz_spvec_destroy (svz_spvec_t *spvec)
{
#if DEVEL
  svz_spvec_validate (spvec, "destroy");
#endif /* DEVEL */

  svz_spvec_clear (spvec);
  svz_free (spvec);
}

/*
 * Appends the specified element @var{value} at the end of the sparse 
 * vector @var{spvec}.
 */
void
svz_spvec_add (svz_spvec_t *spvec, void *value)
{
  svz_spvec_chunk_t *chunk, *last = spvec->last;

#if DEVEL
  svz_spvec_validate (spvec, "add");
#endif /* DEVEL */

  /* append an chunk if necessary */
  if (!last || last->size == SVZ_SPVEC_SIZE)
    {
      chunk = 
	svz_spvec_create_chunk (last ? last->offset + SVZ_SPVEC_SIZE : 0);
      if (last)
	{
	  last->next = chunk;
	  chunk->prev = spvec->last;
	}
      else
	spvec->first = chunk;
      spvec->last = last = chunk;
    }

  /* append the given value */
  last->value[last->size] = value;
  last->fill |= (1 << last->size);
  last->size++;
  
  /* adjust sparse vector properties */
  spvec->length++;
  spvec->size++;
}

/*
 * Removes all of the elements from the sparse vector @var{spvec}. The 
 * sparse vector will be as clean as created with @code{svz_spvec_create()}
 * then.
 */
void
svz_spvec_clear (svz_spvec_t *spvec)
{
  svz_spvec_chunk_t *next, *chunk = spvec->first;
  unsigned long length = spvec->length;

#if DEVEL
  svz_spvec_validate (spvec, "clear");
#endif /* DEVEL */

  /* return here if there is nothing to do */
  if (!chunk || !length)
    return;

  /* go through all chunks and release these */
  length -= chunk->offset;
  while (chunk)
    {
      next = chunk->next;
      length -= chunk->size;
      if (next)
	length -= (next->offset - chunk->offset - chunk->size);
      svz_free (chunk);
      chunk = next;
    }

  /* cleanup sparse vector */
  assert (length == 0);
  spvec->last = spvec->first = NULL;
  spvec->length = 0;
  spvec->size = 0;
}

/*
 * Returns non-zero if the sparse vector @var{spvec} contains the specified 
 * element @var{value}.
 */
unsigned long
svz_spvec_contains (svz_spvec_t *spvec, void *value)
{
  svz_spvec_chunk_t *chunk = spvec->first;
  unsigned long n, fill, found = 0;

#if DEVEL
  svz_spvec_validate (spvec, "contains");
#endif /* DEVEL */

  while (chunk)
    {
      for (fill = 1, n = 0; n < chunk->size; n++, fill <<= 1)
	{
	  if (chunk->fill & fill && chunk->value[n] == value)
	    found++;
	}
      chunk = chunk->next;
    }

  return found;
}

/*
 * Returns the element at the specified position @var{index} in the sparse 
 * vector @var{spvec} or @code{NULL} if there is no such element.
 */
void *
svz_spvec_get (svz_spvec_t *spvec, unsigned long index)
{
  svz_spvec_chunk_t *chunk;

#if DEVEL
  svz_spvec_validate (spvec, "get");
#endif /* DEVEL */

  /* return here if there is no such index at all */
  if (index >= spvec->length)
    return NULL;

  /* start searching at first or last chunk ? */
  if (index > spvec->length >> 1)
    {
      for (chunk = spvec->last; chunk; chunk = chunk->prev)
	if (svz_spvec_range (chunk, index))
	  break;
    }
  else
    {
      for (chunk = spvec->first; chunk; chunk = chunk->next)
	if (svz_spvec_range (chunk, index))
	  break;
    }

  /* evaluate peeked chunk */
  if (!chunk)
    return NULL;
  index -= chunk->offset;
  if (chunk->fill & (1 << index))
    return chunk->value[index];
  return NULL;
}

/*
 * Searches for the first occurrence of the given argument @var{value}. 
 * Return -1 if the value @var{value} could not be found in the sparse 
 * vector @var{spvec}.
 */
unsigned long
svz_spvec_index (svz_spvec_t *spvec, void *value)
{
  svz_spvec_chunk_t *chunk = spvec->first;
  unsigned long n, fill;

#if DEVEL
  svz_spvec_validate (spvec, "index");
#endif /* DEVEL */

  while (chunk)
    {
      for (fill = 1, n = 0; n < chunk->size; n++, fill <<= 1)
	{
	  if (chunk->fill & fill && chunk->value[n] == value)
	    return (n + chunk->offset);
	}
      chunk = chunk->next;
    }
  return (unsigned long) -1;
}

/*
 * Removes the element at the specified position @var{index} in the sparse 
 * vector @var{spvec} and returns its previous value.
 */
void *
svz_spvec_delete (svz_spvec_t *spvec, unsigned long index)
{
  svz_spvec_chunk_t *chunk, *next;
  void *value = NULL;
  unsigned long bit, idx, fill;

#if DEVEL
  char text[128];
  svz_spvec_validate (spvec, "delete");
#endif /* DEVEL */

  /* return if index is invalid */
  if (index >= spvec->length)
    return NULL;

  /* start at first or last chunk ? */
  if (index > spvec->length >> 1)
    {
      for (chunk = spvec->last; chunk; chunk = chunk->prev)
	if (svz_spvec_range (chunk, index))
	  break;
    }
  else
    {
      for (chunk = spvec->first; chunk; chunk = chunk->next)
	if (svz_spvec_range (chunk, index))
	  break;
    }

  /* evaluate peeked chunk */
  if (!chunk)
    return NULL;
  idx = index - chunk->offset;

  /* is there any value at the given index ? */
  if (!(chunk->fill & (1 << idx)))
    return NULL;

  /* delete this value */
  chunk->fill &= ~(1 << idx);
  spvec->size--;
  spvec->length--;

  /* adjust chunk size */
  if (!(chunk->fill & ~((1 << idx) - 1)))
    {
      for (bit = 1 << idx; bit && !(chunk->fill & bit); bit >>= 1) 
	chunk->size--;
    }
  else
    chunk->size--;

  /* adjust sparse vector length */
  if (chunk == spvec->last)
    spvec->length = chunk->offset + chunk->size;

  value = chunk->value[idx];

  /* release this chunk if possible */
  if (chunk->size == 0)
    {
      assert (chunk->fill == 0);

      /* break here if the list is empty */
      if (spvec->size == 0)
	{
	  svz_free (chunk);
	  spvec->last = spvec->first = chunk = NULL;
	  spvec->length = 0;
	  return value;
	}

      /* rearrange sparse vector */
      svz_spvec_unhook (spvec, chunk);
      next = chunk->next;
      svz_free (chunk);
      chunk = next;
    }

  /* shuffle value data if necessary */
  else if (idx < chunk->size)
    {
      /* delete a bit */
      bit = (1 << idx) - 1;
      fill = chunk->fill;
      chunk->fill = (fill & bit) | ((fill >> 1) & ~bit);
      assert (chunk->fill);
	      
      /* delete a value */
      memmove (&chunk->value[idx], &chunk->value[idx + 1],
	       (chunk->size - idx) * sizeof (void *));
    }

  /* reduce array index offset by one */
  while (chunk)
    {
      if (chunk->offset > index)
	chunk->offset--;
      chunk = chunk->next;
    }

#if DEVEL
  sprintf (text, "post-delete (%lu) = %p", index, value);
  svz_spvec_validate (spvec, "delete");
#endif /* DEVEL */

  /* return deleted value */
  return value;
}

/*
 * Removes all of the elements whose index is between @var{from} (inclusive) 
 * and @var{to} (exclusive) from the sparse vector @var{spvec}. Returns the 
 * amount of actually deleted elements.
 */
unsigned long
svz_spvec_delete_range (svz_spvec_t *spvec, 
			unsigned long from, unsigned long to)
{
  unsigned long idx, n = 0;

#if DEVEL
  svz_spvec_validate (spvec, "delete range");
#endif /* DEVEL */

  /* swap the `to' and `from' indexes if necessary */
  if (to < from)
    {
      idx = to;
      to = from + 1;
      from = idx + 1;
    }

  /* return here if there is nothing to do */
  if (to > spvec->length)
    to = spvec->length;
  if (from > spvec->length)
    from = spvec->length;
  if (to == from)
    return 0;

  /* special case: delete all list elements */
  if (from == 0 && to == spvec->length)
    {
      n = spvec->size;
      svz_spvec_clear (spvec);
      return n;
    }

  /* go through the index range and delete each list item */
  for (idx = from; idx < to;)
    {
      if (svz_spvec_delete (spvec, idx))
	{
	  to--;
	  n++;
	}
      else
	idx++;
    }
  return n;
}

/*
 * Replaces the element at the specified position @var{index} in the sparse 
 * vector @var{spvec} by the specified element @var{value} and return its 
 * previous value.
 */
void *
svz_spvec_set (svz_spvec_t *spvec, unsigned long index, void *value)
{
  svz_spvec_chunk_t *chunk, *next;
  void *replace = NULL;
  unsigned long idx;

#if DEVEL
  svz_spvec_validate (spvec, "set");
#endif /* DEVEL */

  /* start at first or last chunk ? */
  chunk = svz_spvec_find_chunk (spvec, index);

  /* found a valid chunk ? */
  if (chunk)
    {
      idx = index - chunk->offset;

      /* already set ? */
      if (chunk->fill & (1 << idx))
	{
	  replace = chunk->value[idx];
	  chunk->value[idx] = value;
	  return replace;
	}
      /* no, just place the value there */
      else if (chunk->next == NULL || idx < chunk->size)
	{
	  chunk->fill |= (1 << idx);
	  if (idx >= chunk->size)
	    chunk->size = idx + 1;
	  spvec->size++;
	  if (spvec->length < chunk->offset + chunk->size)
	    spvec->length = chunk->offset + chunk->size;
	  chunk->value[idx] = value;
	  return replace;
	}
    }

  /* no chunk found, create one at the given offset */
  next = svz_spvec_create_chunk (index);
  next->value[0] = value;
  next->fill |= 1;
  next->size = 1;
  svz_spvec_hook (spvec, next);

  /* adjust list properties */
  spvec->size++;
  if (spvec->length <= next->offset)
    spvec->length = index + 1;

  /* return replaced value */
  return replace;
}

/*
 * Delete the element at the given position @var{index} from the sparse 
 * vector @var{spvec} but leave all following elements untouched 
 * (unlike @code{svz_spvec_delete()}). Return its previous value if there 
 * is one otherwise return @code{NULL}.
 */
void *
svz_spvec_unset (svz_spvec_t *spvec, unsigned long index)
{
  svz_spvec_chunk_t *chunk;
  void *unset = NULL;
  unsigned long idx, bit;

#if DEVEL
  svz_spvec_validate (spvec, "unset");
#endif /* DEVEL */

  /* return if index is invalid */
  if (index >= spvec->length)
    return NULL;

  /* find an appropriate chunk */
  if ((chunk = svz_spvec_find_chunk (spvec, index)) == NULL)
    return NULL;

  idx = index - chunk->offset;

  /* is there a value set ? */
  if (!(chunk->fill & (1 << idx)))
    return NULL;

  /* save unset value for returning it */
  unset = chunk->value[idx];

  /* delete this value */
  spvec->size--;
  chunk->fill &= ~(1 << idx);
  if (idx + 1 == chunk->size)
    for (bit = 1 << idx; bit && !(chunk->fill & bit); bit >>= 1)
      {
	chunk->size--;
	if (chunk == spvec->last)
	  spvec->length--;
      }
  if (chunk->size == 0)
    {
      svz_spvec_unhook (spvec, chunk);
      svz_free (chunk);
    }

  /* return unset value */
  return unset;
}

/*
 * Returns the number of elements in the sparse vector @var{spvec}.
 */
unsigned long
svz_spvec_size (svz_spvec_t *spvec)
{
#if DEVEL
  svz_spvec_validate (spvec, "size");
#endif /* DEVEL */

  return spvec->size;
}

/*
 * Returns the index of the last element of the sparse vector @var{spvec} 
 * plus one.
 */
unsigned long
svz_spvec_length (svz_spvec_t *spvec)
{
#if DEVEL
  svz_spvec_validate (spvec, "length");
#endif /* DEVEL */

  return spvec->length;
}

/*
 * Inserts the specified element @var{value} at the given position @var{index}
 * in the sparse vector @var{spvec}.
 */
void
svz_spvec_insert (svz_spvec_t *spvec, unsigned long index, void *value)
{
  svz_spvec_chunk_t *chunk, *next;
  unsigned long idx, fill, bit;

#if DEVEL
  svz_spvec_validate (spvec, "insert");
#endif /* DEVEL */

  /* start at first or last chunk ? */
  chunk = svz_spvec_find_chunk (spvec, index);

  /* found a valid chunk ? */
  if (chunk)
    {
      idx = index - chunk->offset;

      /* can the value be inserted here ? */
      if (chunk->size < SVZ_SPVEC_SIZE)
	{
	  /* adjust chunk size */
	  chunk->size++;
	  if (idx >= chunk->size)
	    chunk->size = idx + 1;

	  if (idx < chunk->size)
	    {
	      /* insert a bit */
	      bit = (1 << idx) - 1;
	      fill = chunk->fill;
	      chunk->fill = (fill & bit) | ((fill << 1) & ~bit);

	      /* shuffle value data */
	      memmove (&chunk->value[idx + 1], &chunk->value[idx],
		       (chunk->size - 1 - idx) * sizeof (void *));
	    }
	      
	  /* insert the value */
	  chunk->fill |= (1 << idx);
	  chunk->value[idx] = value;
	  chunk = chunk->next;
	}

      /* no, chunk is full, need to split the chunk */
      else
	{
	  next = svz_spvec_create_chunk (index + 1);

	  /* keep less indexes in old chunk and copy greater to next */
	  memcpy (next->value, &chunk->value[idx], 
		  (SVZ_SPVEC_SIZE - idx) * sizeof (void *));
	  next->fill = (chunk->fill >> idx);
	  next->size = SVZ_SPVEC_SIZE - idx;

	  chunk->value[idx] = value;
	  chunk->fill &= (1 << (idx + 1)) - 1;
	  chunk->fill |= (1 << idx);
	  chunk->size = idx + 1;

	  svz_spvec_hook (spvec, next);
	  chunk = next->next;
	}
    }

  /* add another chunk */
  else
    {
      next = svz_spvec_create_chunk (index);
      next->fill = 1;
      next->size = 1;
      next->value[0] = value;
      svz_spvec_hook (spvec, next);
      chunk = next->next;
    }

  /* adjust sparse vector properties here */
  if (++spvec->length < index + 1)
    spvec->length = index + 1;
  spvec->size++;

  /* increase offset of all later chunks */
  while (chunk)
    {
      if (chunk->offset > index)
	chunk->offset++;
      chunk = chunk->next;
    }
}

/*
 * Rearranges the given sparse vector @var{spvec}. After that there are no 
 * more gaps within the sparse vector. The index - value relationship gets 
 * totally lost by this operation.
 */
void
svz_spvec_pack (svz_spvec_t *spvec)
{
  svz_spvec_chunk_t *chunk, *next, *prev;
  unsigned long need2pack, bits, n, size;
  void **value;

#if DEVEL
  svz_spvec_validate (spvec, "pack");
#endif /* DEVEL */

  if (!spvec->size)
    return;

  /* first check if it is necessary to pack the chunks */
  for (need2pack = 0, chunk = spvec->first; chunk; chunk = chunk->next)
    {
      next = chunk->next;
      if (next && chunk->size == SVZ_SPVEC_SIZE)
	{
	  if (chunk->fill != SVZ_SPVEC_MASK ||
	      chunk->offset + SVZ_SPVEC_SIZE != next->offset)
	    {
	      need2pack = 1;
	      break;
	    }
	}
      if (next && chunk->size < SVZ_SPVEC_SIZE)
	{
	  need2pack = 1;
	  break;
	}
      if (!next)
	{
	  bits = (1 << (spvec->length - chunk->offset)) - 1;
	  if ((chunk->fill & bits) != bits)
	    {
	      need2pack = 1;
	      break;
	    }
	}
    }

  /* return if packing is not necessary */
  if (!need2pack)
    return;

  /* rebuild sparse vector */
  value = svz_spvec_values (spvec);
  size = svz_spvec_size (spvec);
  svz_spvec_clear (spvec);
  prev = spvec->first;
  for (n = 0; n <= size - SVZ_SPVEC_SIZE; n += SVZ_SPVEC_SIZE)
    {
      chunk = svz_spvec_create_chunk (n);
      chunk->fill = SVZ_SPVEC_MASK;
      chunk->size = SVZ_SPVEC_SIZE;
      spvec->size += SVZ_SPVEC_SIZE;
      memcpy (chunk->value, &value[n], SVZ_SPVEC_SIZE * sizeof (void *));
      if (!prev)
	spvec->first = chunk;
      else
	prev->next = chunk;
      chunk->prev = prev;
      prev = chunk;
    }
  if (size % SVZ_SPVEC_SIZE)
    {
      size %= SVZ_SPVEC_SIZE;
      chunk = svz_spvec_create_chunk (n);
      chunk->fill = (1 << size) - 1;
      chunk->size = size;
      spvec->size += size;
      memcpy (chunk->value, &value[n], size * sizeof (void *));
      if (!prev)
	spvec->first = chunk;
      else
	prev->next = chunk;
      chunk->prev = prev;
    }
  spvec->last = chunk;
  spvec->length = spvec->size;
  svz_free (value);
}

/*
 * Delivers all values within the given sparse vector @var{spvec} in a 
 * single linear chunk. You have to @code{svz_free()} it after usage.
 */
void **
svz_spvec_values (svz_spvec_t *spvec)
{
  svz_spvec_chunk_t *chunk;
  void **value;
  unsigned long index, bit, n;

#if DEVEL
  svz_spvec_validate (spvec, "values");
#endif /* DEVEL */

  if (!spvec->size) 
    return NULL;

  value = svz_malloc (spvec->size * sizeof (void *));
  for (index = 0, chunk = spvec->first; chunk; chunk = chunk->next) 
    {
      for (bit = 1, n = 0; n < chunk->size; bit <<= 1, n++)
	{
	  if (chunk->fill & bit)
	    value[index++] = chunk->value[n];
	  assert (index <= spvec->size);
	}
    }
  return value;
}
