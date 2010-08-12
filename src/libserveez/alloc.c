/*
 * alloc.c - memory allocation module implementation
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2000 Raimund Jacob <raimi@lkcc.org>
 * Copyright (C) 1999 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * $Id: alloc.c,v 1.20 2003/06/14 14:57:59 ela Exp $
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

/* The @var{svz_malloc_func} variable is a function pointer for allocating
   dynamic memory. */
svz_malloc_func_t svz_malloc_func = malloc;
/* This function pointer is called whenever the core library needs to
   reallocate (resize) a memory block. */
svz_realloc_func_t svz_realloc_func = realloc;
/* In order to free a block of memory the core library calls this function
   pointer. */
svz_free_func_t svz_free_func = free;

/*
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 */
void *
svz_malloc (svz_t_size size)
{
  void *ptr;

  assert (size);
#if DEBUG_MEMORY_LEAKS
  if ((ptr = (void *) GC_MALLOC (size)) != NULL)
    return ptr;
#else
  if ((ptr = (void *) svz_malloc_func (size)) != NULL)
    return ptr;
#endif
  else
    {
      svz_log (LOG_FATAL, "malloc: virtual memory exhausted\n");
      exit (1);
    }
}

/*
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 * The memory is cleared (filled with zeros).
 */
void *
svz_calloc (svz_t_size size)
{
  void *ptr = svz_malloc (size);
  memset (ptr, 0, size);
  return ptr;
}

/*
 * Change the size of a @code{svz_malloc()}'ed block of memory. The @var{size}
 * argument is the new size of the block in bytes, The given variable
 * @var{ptr} must be a pointer previously returned by @code{svz_malloc()} or
 * @code{NULL} if you want to allocate a new block.
 */
void *
svz_realloc (void *ptr, svz_t_size size)
{
  assert (size);

  if (ptr)
    {
#if DEBUG_MEMORY_LEAKS
     if ((ptr = (void *) GC_REALLOC (ptr, size)) != NULL)
	{
	  return ptr;
	}
#else
      if ((ptr = (void *) svz_realloc_func (ptr, size)) != NULL)
	{
	  return ptr;
	}
#endif
      else
	{
	  svz_log (LOG_FATAL, "realloc: virtual memory exhausted\n");
	  exit (1);
	}
    }
  else
    {
      ptr = svz_malloc (size);
      return ptr;
    }
}

/*
 * Free a block of @code{svz_malloc()}'ed or @code{svz_realloc()}'ed memory
 * block. If @var{ptr} is a @code{NULL} pointer, no operation is performed.
 */
void
svz_free (void *ptr)
{
  if (ptr)
    {
#if DEBUG_MEMORY_LEAKS
      GC_FREE (ptr);
#else
      svz_free_func (ptr);
#endif
    }
}

/*
 * Duplicate the given string @var{src} if it is not @code{NULL} and has
 * got a valid length (greater than zero). Return the pointer to the
 * copied character string.
 */
char *
svz_strdup (char *src)
{
  char *dst;
  int len;

  if (src == NULL || (len = strlen (src)) == 0)
    return NULL;

  dst = svz_malloc (len + 1);
  memcpy (dst, src, len + 1);
  return dst;
}

/*
 * Allocate a block of memory with the size @var{size} permanently.
 * FIXME: Memory allocated this way should not tested for memory
 * leaks.
 */
void *
svz_pmalloc (svz_t_size size)
{
  void *ptr = svz_malloc_func (size);
  if (ptr == NULL)
    {
      svz_log (LOG_FATAL, "malloc: virtual memory exhausted\n");
      exit (1);
    }
  return ptr;
}

/*
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 * The memory block is cleared (filled with zeros) and considered permanently.
 */
void *
svz_pcalloc (svz_t_size size)
{
  void *ptr = svz_malloc (size);
  memset (ptr, 0, size);
  return ptr;
}

/*
 * Resize the memory block pointed to by @var{ptr} to @var{size} bytes. This
 * routine also allocates memory permanently.
 */
void *
svz_prealloc (void *ptr, svz_t_size size)
{
  void *dst = svz_realloc_func (ptr, size);
  if (dst == NULL)
    {
      svz_log (LOG_FATAL, "realloc: virtual memory exhausted\n");
      exit (1);
    }
  return dst;
}

/*
 * Duplicate the given character string @var{src} permanently.
 */
char *
svz_pstrdup (char *src)
{
  char *dst;

  assert (src);
  dst = svz_pmalloc (strlen (src) + 1);
  memcpy (dst, src, strlen (src) + 1);

  return dst;
}
