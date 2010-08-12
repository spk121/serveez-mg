/*
 * alloc.h - memory allocation module declarations
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
 * $Id: alloc.h,v 1.12 2003/06/14 14:57:59 ela Exp $
 *
 */

#ifndef __XALLOC_H__
#define __XALLOC_H__ 1

#include "libserveez/defines.h"

#include <sys/types.h>
#if DEBUG_MEMORY_LEAKS
# define GC_DEBUG
#endif /* DEBUG_MEMORY_LEAKS */
#include <gc.h>

/*
 * Free the memory block pointed to by @var{var} and set it to @code{NULL}
 * afterwards. The argument @var{var} is passed to @code{svz_free()}.
 */
#define svz_free_and_zero(var) \
  do { svz_free (var); (var) = NULL; } while (0)

#if DEBUG_MEMORY_LEAKS
# define CHECK_LEAKS() GC_collect()
#endif

__BEGIN_DECLS

/* Function type definitions. */
typedef void * (* svz_malloc_func_t) __PARAMS ((svz_t_size));
typedef void * (* svz_realloc_func_t) __PARAMS ((void *, svz_t_size));
typedef void (* svz_free_func_t) __PARAMS ((void *));

/* Global allocator functions. */
SERVEEZ_API svz_malloc_func_t svz_malloc_func;
SERVEEZ_API svz_realloc_func_t svz_realloc_func;
SERVEEZ_API svz_free_func_t svz_free_func;

/* Internal allocator functions. */
SERVEEZ_API void *svz_malloc __PARAMS ((svz_t_size));
SERVEEZ_API void *svz_calloc __PARAMS ((svz_t_size));
SERVEEZ_API void *svz_realloc __PARAMS ((void *, svz_t_size));
SERVEEZ_API void svz_free __PARAMS ((void *));
SERVEEZ_API char *svz_strdup __PARAMS ((char *));

/* Internal permanent allocator functions. */
SERVEEZ_API void *svz_pmalloc __PARAMS ((svz_t_size));
SERVEEZ_API void *svz_prealloc __PARAMS ((void *, svz_t_size));
SERVEEZ_API char *svz_pstrdup __PARAMS ((char *));

__END_DECLS

#endif /* not __XALLOC_H__ */
