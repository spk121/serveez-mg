/*
 * hash.h - hash function interface
 *
 * Copyright (C) 2000, 2001, 2002 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: hash.h,v 1.15 2002/07/30 22:39:08 ela Exp $
 *
 */

#ifndef __HASH_H__
#define __HASH_H__ 1

#include "defines.h"

/* useful defines */
#define HASH_SHRINK   4
#define HASH_EXPAND   8
#define HASH_MIN_SIZE 4

/*
 * This is the basic structure of a hash entry consisting of its
 * key, the actual value stored in the hash table and the hash code
 * of the key.
 */
typedef struct
{
  unsigned long code;
  char *key;
  void *value;
}
svz_hash_entry_t;

/*
 * The hash table consists of different hash buckets. This contains the
 * bucket's size and the entry array.
 */
typedef struct
{
  int size;
  svz_hash_entry_t *entry;
}
svz_hash_bucket_t;

/*
 * This structure keeps information of a specific hash table.
 */
typedef struct svz_hash
{
  int buckets;                     /* number of buckets in the table */
  int fill;                        /* number of filled buckets */
  int keys;                        /* number of stored keys */
  int (* equals) (char *, char *); /* key string equality callback */
  unsigned long (* code) (char *); /* hash code calculation callback */
  unsigned (* keylen) (char *);    /* how to get the hash key length */
  svz_free_func_t destroy;         /* element destruction callback */
  svz_hash_bucket_t *table;        /* hash table */
}
svz_hash_t;

__BEGIN_DECLS

/*
 * Basic hash table functions.
 */
SERVEEZ_API svz_hash_t *svz_hash_create (int, svz_free_func_t);
SERVEEZ_API void svz_hash_destroy (svz_hash_t *);
SERVEEZ_API void svz_hash_clear (svz_hash_t *);
SERVEEZ_API void *svz_hash_delete (svz_hash_t *, char *);
SERVEEZ_API void *svz_hash_put (svz_hash_t *, char *, void *);
SERVEEZ_API void *svz_hash_get (svz_hash_t *, char *);
SERVEEZ_API void **svz_hash_values (svz_hash_t *);
SERVEEZ_API char **svz_hash_keys (svz_hash_t *);
SERVEEZ_API int svz_hash_size (svz_hash_t *);
SERVEEZ_API int svz_hash_capacity (svz_hash_t *);
SERVEEZ_API char *svz_hash_contains (svz_hash_t *, void *);
SERVEEZ_API void svz_hash_rehash (svz_hash_t *, int);
SERVEEZ_API int svz_hash_exists (svz_hash_t *, char *);

__END_DECLS

/*
 * Helper macros for better usage
 */


/*
 * Iterator macro for walking over the values of a hash. Use like:
 * @example
 *   type_t **values; int i;
 *   svz_hash_foreach_value (hash, values, i) @{
 *     process_value (values[i]);
 *   @}
 * @end example
 * Be sure you pass real variables and no expressions to this macro !
 * Warning: Relatively slow implementation, builds up temporary array.
 * Do not @code{break} or @code{return} from inside the loop or your program
 * starts leaking memory ! Loop has to end normally.
 */
#define svz_hash_foreach_value(hash, iterarray, i)                           \
 for (                                                                       \
  ((i) = (((*((void***)(&(iterarray)))) = svz_hash_values (hash)) == NULL ?  \
                                  -1 : 0));                                  \
  ( (i) != -1 );                                                             \
  ( (++(i)) < svz_hash_size (hash)) ? 42 :                                   \
                    (svz_hash_xfree (iterarray), iterarray = NULL, (i) = -1) \
 )

/*
 * Iterator macro for walking over the keys of a hash. Use like:
 * @example
 *   char **allkeys; int i;
 *   svz_hash_foreach_key (hash, allkeys, i) @{
 *     printf ("%s => %p\\n", allkeys[i], svz_hash_get (hash, allkeys[i]));
 *   @}
 * @end example
 * Be sure you pass real variables and no expressions to this macro !
 * Warning: Relatively slow implementation, builds up temporary array.
 * Do not @code{break} or @code{return} from inside the loop or your program
 * starts leaking memory ! Loop has to end normally.
 */
#define svz_hash_foreach_key(hash, iterarray, i)                             \
 for (                                                                       \
  ((i) = (((*((char***)(&(iterarray)))) = svz_hash_keys (hash)) == NULL ?    \
                                  -1 : 0));                                  \
  ( (i) != -1 );                                                             \
  ( (++(i)) < svz_hash_size (hash)) ? 42 :                                   \
                    (svz_hash_xfree (iterarray), iterarray = NULL, (i) = -1) \
 )


#if DEBUG_MEMORY_LEAKS
# include <stdlib.h>
# define svz_hash_xfree(ptr) svz_free_func (ptr)
#else
# define svz_hash_xfree(ptr) svz_free (ptr)
#endif

#endif /* not __HASH_H__ */
