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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"

#if DEBUG_MEMORY_LEAKS
# include "libserveez/hash.h"
#endif /* DEBUG_MEMORY_LEAKS */

#if SVZ_ENABLE_DEBUG
/* The variable @var{svz_allocated_bytes} holds the overall number of bytes 
   allocated by the core library. */
unsigned int svz_allocated_bytes = 0;
/* This variable holds the number of memory blocks reserved by the core
   library. */
unsigned int svz_allocated_blocks = 0;
#endif /* SVZ_ENABLE_DEBUG */

/* The @var{svz_malloc_func} variable is a function pointer for allocating 
   dynamic memory. */
svz_malloc_func_t svz_malloc_func = malloc;
/* This function pointer is called whenever the core library needs to
   reallocate (resize) a memory block. */
svz_realloc_func_t svz_realloc_func = realloc;
/* In order to free a block of memory the core library calls this function
   pointer. */
svz_free_func_t svz_free_func = free;

#if DEBUG_MEMORY_LEAKS

/* heap hash table */
static svz_hash_t *heap = NULL;

/* return static heap hash code key length */
static unsigned
heap_hash_keylen (char *id)
{
  return SIZEOF_VOID_P;
}

/* compare two heap hash values */
static int 
heap_hash_equals (char *id1, char *id2)
{
  return memcmp (id1, id2, SIZEOF_VOID_P);
}

/* calculate heap hash code */
static unsigned long 
heap_hash_code (char *id)
{
  unsigned long code = SVZ_UINT32 (id);
  code >>= 3;
  return code;
}

/* structure for heap management */
typedef struct
{
  void *ptr;       /* memory pointer */
  size_t size; /* memory block's size */
  void *caller;    /* the caller */
}
heap_block_t;

/* add another heap block to the heap management */
static void
heap_add (heap_block_t *block)
{
  if (heap == NULL)
    {
      heap = svz_hash_create (4, NULL);
      heap->keylen = heap_hash_keylen;
      heap->code = heap_hash_code;
      heap->equals = heap_hash_equals;
    }
  svz_hash_put (heap, (char *) &block->ptr, block);
}

#ifdef _MSC_VER
# include <windows.h>
# include <imagehlp.h>
# define __builtin_return_address(no) ((void *) (stack.AddrReturn.Offset))
# define heap_caller()                                                     \
    STACKFRAME stack;                                                      \
    StackWalk (IMAGE_FILE_MACHINE_I386, GetCurrentProcess (),              \
	       GetCurrentThread (), &stack, NULL, NULL, NULL, NULL, NULL)
#else
# ifndef __GNUC__
#  define __builtin_return_address(no) 0
# endif
# define heap_caller()
#endif

#else /* !DEBUG_MEMORY_LEAKS */
# define heap_caller()
#endif /* !DEBUG_MEMORY_LEAKS */

/*
 * Allocate @var{size} bytes of memory and return a pointer to this block.
 */
void * 
svz_malloc (size_t size)
{
  void *ptr;
#if SVZ_ENABLE_DEBUG
  size_t *p;
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */
#endif /* SVZ_ENABLE_DEBUG */

  heap_caller ();
  assert (size);

#if SVZ_ENABLE_DEBUG
  if ((ptr = (void *) svz_malloc_func (size + 2 * 
				       sizeof (size_t))) != NULL)
    {
#if ENABLE_HEAP_COUNT
      /* save size at the beginning of the block */
      p = (size_t *) ptr;
      *p = size;
      p += 2;
      ptr = (void *) p;
#if DEBUG_MEMORY_LEAKS
      /* put heap pointer into special heap hash */
      block = svz_malloc_func (sizeof (heap_block_t));
      block->ptr = ptr;
      block->size = size;
      block->caller = __builtin_return_address (0);
      heap_add (block);
#endif /* DEBUG_MEMORY_LEAKS */
      svz_allocated_bytes += size;
#endif /* ENABLE_HEAP_COUNT */
      svz_allocated_blocks++;
      return ptr;
    }
#else /* not SVZ_ENABLE_DEBUG */
  if ((ptr = (void *) svz_malloc_func (size)) != NULL)
    {
      return ptr;
    }
#endif /* not SVZ_ENABLE_DEBUG */
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
svz_calloc (size_t size)
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
svz_realloc (void *ptr, size_t size)
{
#if SVZ_ENABLE_DEBUG
  size_t old_size, *p;
#endif /* SVZ_ENABLE_DEBUG */
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */

  heap_caller ();
  assert (size);

  if (ptr)
    {
#if SVZ_ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
#if DEBUG_MEMORY_LEAKS
      if ((block = svz_hash_delete (heap, (char *) &ptr)) == NULL ||
	  block->ptr != ptr)
	{
	  fprintf (stdout, "realloc: %p not found in heap (caller: %p)\n", 
		   ptr, __builtin_return_address (0));
	  assert (0);
	}
      svz_free_func (block);
#endif /* DEBUG_MEMORY_LEAKS */

      /* get previous blocksize */
      p = (size_t *) ptr;
      p -= 2;
      old_size = *p;
      ptr = (void *) p;
#endif /* ENABLE_HEAP_COUNT */

      if ((ptr = (void *) svz_realloc_func (ptr, size + 2 * 
					    sizeof (size_t))) != NULL)
	{
#if ENABLE_HEAP_COUNT
	  /* save block size */
	  p = (size_t *) ptr;
	  *p = size;
	  p += 2;
	  ptr = (void *) p;

#if DEBUG_MEMORY_LEAKS
	  block = svz_malloc_func (sizeof (heap_block_t));
	  block->ptr = ptr;
	  block->size = size;
	  block->caller = __builtin_return_address (0);
	  heap_add (block);
#endif /* DEBUG_MEMORY_LEAKS */

	  svz_allocated_bytes += size - old_size;
#endif /* ENABLE_HEAP_COUNT */

	  return ptr;
	}
#else /* not SVZ_ENABLE_DEBUG */
      if ((ptr = (void *) svz_realloc_func (ptr, size)) != NULL)
	{
	  return ptr;
	}
#endif /* not SVZ_ENABLE_DEBUG */
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
#if SVZ_ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
  size_t size, *p;
#if DEBUG_MEMORY_LEAKS
  heap_block_t *block;
#endif /* DEBUG_MEMORY_LEAKS */
#endif /* ENABLE_HEAP_COUNT */
#endif /* SVZ_ENABLE_DEBUG */

  heap_caller ();

  if (ptr)
    {
#if SVZ_ENABLE_DEBUG
#if ENABLE_HEAP_COUNT
#if DEBUG_MEMORY_LEAKS
      if ((block = svz_hash_delete (heap, (char *) &ptr)) == NULL ||
	  block->ptr != ptr)
	{
	  fprintf (stdout, "free: %p not found in heap (caller: %p)\n", 
		   ptr, __builtin_return_address (0));
	  assert (0);
	}
      svz_free_func (block);
#endif /* DEBUG_MEMORY_LEAKS */

      /* get blocksize */
      p = (size_t *) ptr;
      p -= 2;
      size = *p;
      ptr = (void *) p;
      assert (size);
      svz_allocated_bytes -= size;
#endif /* ENABLE_HEAP_COUNT */

      svz_allocated_blocks--;
#endif /* SVZ_ENABLE_DEBUG */
      svz_free_func (ptr);
    }
}

#if DEBUG_MEMORY_LEAKS
/*
 * Print a list of non-released memory blocks. This is for debugging only
 * and should never occur in final software releases. The function goes
 * through the heap hash and states each blocks address, size and caller.
 */
void
svz_heap (void)
{
  heap_block_t **block;
  unsigned long n;
  size_t *p;

  if ((block = (heap_block_t **) svz_hash_values (heap)) != NULL)
    {
      for (n = 0; n < (unsigned long) svz_hash_size (heap); n++)
	{
	  p = (size_t *) block[n]->ptr;
	  p -= 2;
	  fprintf (stdout, "heap: caller = %p, ptr = %p, size = %u\n",
		   block[n]->caller, block[n]->ptr, block[n]->size);
	  svz_hexdump (stdout, "unreleased heap", (int) block[n]->ptr,
		       block[n]->ptr, *p, 256);
	  svz_free_func (block[n]);
	}
      svz_hash_xfree (block);
    }
  else
    {
      fprintf (stdout, "heap: no unreleased heap blocks\n");
    }
  svz_hash_destroy (heap);
  heap = NULL;
}
#endif /* DEBUG_MEMORY_LEAKS */

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
 * Allocate a block of memory with the size @var{size} permanently. Memory
 * allocated this way does not get into account of the libraries memory
 * tracking.
 */
void *
svz_pmalloc (size_t size)
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
svz_pcalloc (size_t size)
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
svz_prealloc (void *ptr, size_t size)
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
