/*
 * hash.c - hash table functions
 *
 * Copyright (C) 2000, 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: hash.c,v 1.9 2003/06/14 14:57:59 ela Exp $
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
#include "libserveez/hash.h"

#if DEBUG_MEMORY_LEAKS
# define svz_free(ptr) svz_free_func (ptr)
# define svz_malloc(size) svz_malloc_func (size)
# define svz_realloc(ptr, size) svz_realloc_func (ptr, size)
#endif /* DEBUG_MEMORY_LEAKS */

/* some useful defines */
#define HASH_SHRINK_LIMIT(hash) (hash->buckets >> 2)
#define HASH_EXPAND_LIMIT(hash) ((hash->buckets >> 1) + (hash->buckets >> 2))
#define HASH_BUCKET(code, hash) (code & (hash->buckets - 1))

/*
 * Calculate the hash code for a given string @var{key}. This is the standard 
 * callback for any newly created hash table.
 */
static unsigned long
svz_hash_code (char *key)
{
  unsigned long code = 0;
  char *p = key;

  assert (key);
  while (*p)
    {
      code = (code << 1) ^ *p;
      p++;
    }
  return code;
}

/*
 * This is the default callback for any newly created hash for determining
 * two keys (@var{key1} and @var{key2}) being equal. Return zero if both 
 * strings are equal, otherwise non-zero.
 */
static int
svz_hash_key_equals (char *key1, char *key2)
{
  char *p1, *p2;

  assert (key1 && key2);

  if (key1 == key2)
    return 0;
  
  p1 = key1;
  p2 = key2;

  while (*p1 && *p2)
    {
      if (*p1 != *p2)
	return -1;
      p1++;
      p2++;
    }

  if (!*p1 && !*p2)
    return 0;
  return -1;
}

/*
 * This is the default routine for determining the actual hash table 
 * key length of the given key @var{key}.
 */
static unsigned
svz_hash_key_length (char *key)
{
  unsigned len = 0;

  assert (key);
  while (*key++)
    len++;
  len++;

  return len;
}

/*
 * This routine prints all the hash table @var{hash}. It is for debugging 
 * purposes only and should not go into distributions.
 */
static void
svz_hash_analyse (svz_hash_t *hash)
{
  svz_hash_bucket_t *bucket;
  int n, e, buckets, depth, entries;

  for (entries = 0, depth = 0, buckets = 0, n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      if (bucket->size > 0)
	buckets++;
      for (e = 0; e < bucket->size; e++)
	{
	  entries++;
#if 0
	  fprintf (stdout, "bucket %04d: entry %02d: code: %08lu "
		   "value: %p key: %-20s\n",
		   n + 1, e + 1, bucket->entry[e].code, 
		   bucket->entry[e].value, bucket->entry[e].key);
#endif /* 0 */
	  if (e > depth)
	    depth = e;
	}
    }
#if SVZ_ENABLE_DEBUG
  svz_log (LOG_DEBUG, 
	   "%d/%d buckets (%d), %d entries (%d), depth: %d\n",
	   buckets, hash->buckets, hash->fill, 
	   entries, hash->keys, depth + 1);
#endif /* SVZ_ENABLE_DEBUG */
}

/*
 * Create a new hash table with an initial capacity @var{size}. Return a 
 * non-zero pointer to the newly created hash. The size is calculated down
 * to a binary value. The @var{destroy} callback allows you to pass a
 * element destruction callback called within @code{svz_hash_clear()} and
 * @code{svz_hash_destroy()} for each value. If no such operation should be
 * performed the argument must be @code{NULL}.
 */
svz_hash_t *
svz_hash_create (int size, svz_free_func_t destroy)
{
  int n;
  svz_hash_t *hash;

  /* set initial hash table size to a binary value */
  for (n = size, size = 1; n != 1; n >>= 1) 
    size <<= 1;
  if (size < HASH_MIN_SIZE)
    size = HASH_MIN_SIZE;

  /* allocate space for the hash itself */
  hash = svz_malloc (sizeof (svz_hash_t));
  hash->buckets = size;
  hash->fill = 0;
  hash->keys = 0;
  hash->code = svz_hash_code;
  hash->equals = svz_hash_key_equals;
  hash->keylen = svz_hash_key_length;
  hash->destroy = destroy;

  /* allocate space for the hash table and initialize it */
  hash->table = svz_malloc (sizeof (svz_hash_bucket_t) * size);
  for (n = 0; n < size; n++)
    {
      hash->table[n].size = 0;
      hash->table[n].entry = NULL;
    }

  return hash;
}

/*
 * Destroy the existing hash table @var{hash}. Therefore we @code{svz_free()}
 * all keys within the hash, the hash table and the hash itself. The values 
 * in the hash keep untouched if the element destruction callback passed to
 * @code{svz_hash_create()} was @code{NULL}, otherwise it is called for each
 * value. If @var{hash} is @code{NULL} no operation is performed.
 */
void
svz_hash_destroy (svz_hash_t *hash)
{
  int n, e;
  svz_hash_bucket_t *bucket;

  if (hash == NULL)
    return;

  for (n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      if (bucket->size)
	{
	  for (e = 0; e < bucket->size; e++)
	    {
	      svz_free (bucket->entry[e].key);
	      if (hash->destroy)
		hash->destroy (bucket->entry[e].value);
	    }
	  svz_free (bucket->entry);
	}
    }
  svz_free (hash->table);
  svz_free (hash);
}

/*
 * Clear the hash table of a given hash @var{hash}. Afterwards it does not 
 * contains any key. In contradiction to @code{svz_hash_destroy()} this 
 * functions does not destroy the hash itself, but shrinks it to a minimal 
 * size.
 */
void
svz_hash_clear (svz_hash_t *hash)
{
  svz_hash_bucket_t *bucket;
  int n, e;

  /* go through all buckets of the table and delete its entries */
  for (n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      if (bucket->size)
	{
	  for (e = 0; e < bucket->size; e++)
	    {
	      svz_free (bucket->entry[e].key);
	      if (hash->destroy)
		hash->destroy (bucket->entry[e].value);
	    }
	  svz_free (bucket->entry);
	  bucket->entry = NULL;
	  bucket->size = 0;
	}
    }

  /* reinitialize the hash table */
  hash->buckets = HASH_MIN_SIZE;
  hash->fill = 0;
  hash->keys = 0;
  hash->table = svz_realloc (hash->table, 
			     sizeof (svz_hash_bucket_t) * hash->buckets);
}

/*
 * Rehash a given hash table @var{hash}. Double (@var{type} is 
 * @code{HASH_EXPAND}) its size and expand the hash codes or half (@var{type}
 * is @code{HASH_SHRINK}) its size and shrink the hash codes if these would 
 * be placed somewhere else.
 */
void
svz_hash_rehash (svz_hash_t *hash, int type)
{
  int n, e;
  svz_hash_bucket_t *bucket, *next_bucket;

#if 0
  svz_hash_analyse (hash);
#endif /* SVZ_ENABLE_DEBUG */

  if (type == HASH_EXPAND)
    {
      /*
       * Reallocate and initialize the hash table itself.
       */
      hash->buckets <<= 1;
      hash->table = svz_realloc (hash->table, 
				 sizeof (svz_hash_bucket_t) * hash->buckets);
      for (n = hash->buckets >> 1; n < hash->buckets; n++)
	{
	  hash->table[n].size = 0;
	  hash->table[n].entry = NULL;
	}

      /*
       * Go through all hash table entries and check if it is necessary
       * to relocate them.
       */
      for (n = 0; n < (hash->buckets >> 1); n++)
	{
	  bucket = &hash->table[n];
	  for (e = 0; e < bucket->size; e++)
	    {
	      if ((unsigned long) n != 
		  HASH_BUCKET (bucket->entry[e].code, hash))
		{
		  /* copy this entry to the far entry */
		  next_bucket = 
		    &hash->table[HASH_BUCKET (bucket->entry[e].code, hash)];
		  next_bucket->entry = svz_realloc (next_bucket->entry,
						    (next_bucket->size + 1) *
						    sizeof (svz_hash_entry_t));
		  next_bucket->entry[next_bucket->size] = bucket->entry[e];
		  next_bucket->size++;
		  if (next_bucket->size == 1)
		    hash->fill++;
	      
		  /* delete this entry */
		  bucket->size--;
		  if (bucket->size == 0)
		    {
		      svz_free (bucket->entry);
		      bucket->entry = NULL;
		      hash->fill--;
		    }
		  else
		    {
		      bucket->entry[e] = bucket->entry[bucket->size];
		      bucket->entry = svz_realloc (bucket->entry,
						   bucket->size *
						   sizeof (svz_hash_entry_t));
		    }
		  e--;
		}
	    }
	}
    }
  else if (type == HASH_SHRINK && hash->buckets > HASH_MIN_SIZE)
    {
      hash->buckets >>= 1;
      for (n = hash->buckets; n < hash->buckets << 1; n++)
	{
	  bucket = &hash->table[n];
	  if (bucket->size)
	    {
	      for (e = 0; e < bucket->size; e++)
		{
		  next_bucket = 
		    &hash->table[HASH_BUCKET (bucket->entry[e].code, hash)];
		  next_bucket->entry = svz_realloc (next_bucket->entry,
						    (next_bucket->size + 1) *
						    sizeof (svz_hash_entry_t));
		  next_bucket->entry[next_bucket->size] = bucket->entry[e];
		  next_bucket->size++;
		  if (next_bucket->size == 1)
		    hash->fill++;
		}
	      svz_free (bucket->entry);
	    }
	  hash->fill--;
	}
      hash->table = svz_realloc (hash->table, 
				 sizeof (svz_hash_bucket_t) * hash->buckets);
    }

#if 0
  svz_hash_analyse (hash);
#endif /* SVZ_ENABLE_DEBUG */
}

/*
 * This function adds a new element consisting of @var{key} and @var{value} 
 * to an existing hash @var{hash}. If the hash is 75% filled it gets rehashed 
 * (size will be doubled). When the key already exists then the value just 
 * gets replaced dropping and returning the old value. Note: This is 
 * sometimes the source of memory leaks.
 */
void *
svz_hash_put (svz_hash_t *hash, char *key, void *value)
{
  unsigned long code = 0;
  int e;
  void *old;
  svz_hash_entry_t *entry;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);

  /* Check if the key is already stored. If so replace the value. */
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  for (e = 0; e < bucket->size; e++)
    {
      if (bucket->entry[e].code == code &&
	  hash->equals (bucket->entry[e].key, key) == 0)
	{
	  old = bucket->entry[e].value;
	  bucket->entry[e].value = value;
	  return old;
	}
    }

  /* Reallocate this bucket. */
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  bucket->entry = svz_realloc (bucket->entry, 
			       sizeof (svz_hash_entry_t) * (bucket->size + 1));

  /* Fill this entry. */
  entry = &bucket->entry[bucket->size];
  entry->key = svz_malloc (hash->keylen (key));
  memcpy (entry->key, key, hash->keylen (key));
  entry->value = value;
  entry->code = code;
  bucket->size++;
  hash->keys++;

  /* 75% filled ? */
  if (bucket->size == 1)
    {
      hash->fill++;
      if (hash->fill > HASH_EXPAND_LIMIT (hash))
	{
	  svz_hash_rehash (hash, HASH_EXPAND);
	}
    }
  return NULL;
}

/*
 * Delete an existing hash entry accessed via a given key @var{key} form the
 * hash table @var{hash}. Return NULL if the key has not been found within 
 * the hash, otherwise the previous value.
 */
void *
svz_hash_delete (svz_hash_t *hash, char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;
  void *value;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  
  for (n = 0; n < bucket->size; n++)
    {
      if (bucket->entry[n].code == code && 
	  hash->equals (bucket->entry[n].key, key) == 0)
	{
	  value = bucket->entry[n].value;
	  bucket->size--;
	  svz_free (bucket->entry[n].key);
	  if (bucket->size)
	    {
	      bucket->entry[n] = bucket->entry[bucket->size];
	      bucket->entry = svz_realloc (bucket->entry, 
					   sizeof (svz_hash_entry_t) * 
					   bucket->size);
	    }
	  else
	    {
	      svz_free (bucket->entry);
	      bucket->entry = NULL;
	      hash->fill--;
	      if (hash->fill < HASH_SHRINK_LIMIT (hash))
		{
		  svz_hash_rehash (hash, HASH_SHRINK);
		}
	    }
	  hash->keys--;
	  return value;
	}
    }

  return NULL;
}

/*
 * Hash table lookup. Find a value for a given @var{key} in the hash table 
 * @var{hash}. Return NULL if the key has not been found within the hash 
 * table.
 */
void *
svz_hash_get (svz_hash_t *hash, char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  
  for (n = 0; n < bucket->size; n++)
    {
      if (bucket->entry[n].code == code && 
	  hash->equals (bucket->entry[n].key, key) == 0)
	{
	  return bucket->entry[n].value;
	}
    }

  return NULL;
}

/*
 * Returns a non-zero value if the given @code{key} is stored within
 * the hash table @code{hash}.  Otherwise the function returns zero.
 * This function is useful when you cannot tell whether the return
 * value of @code{svz_hash_get()} (@code{== NULL}) indicates a real
 * value in the hash or a non-existing hash key.
 */
int
svz_hash_exists (svz_hash_t *hash, char *key)
{
  int n;
  unsigned long code;
  svz_hash_bucket_t *bucket;

  code = hash->code (key);
  bucket = &hash->table[HASH_BUCKET (code, hash)];
  
  for (n = 0; n < bucket->size; n++)
    if (bucket->entry[n].code == code && 
	hash->equals (bucket->entry[n].key, key) == 0)
      return -1;
  return 0;
}

/*
 * This function delivers all values within a hash table @var{hash}. It 
 * returns NULL if there were no values in the hash. You MUST 
 * @code{svz_hash_xfree()} a non-NULL return value in order to prevent
 * memory leaks.
 */
void **
svz_hash_values (svz_hash_t *hash)
{
  void **values;
  svz_hash_bucket_t *bucket;
  int n, e, keys;

  if (hash == NULL || hash->keys == 0)
    return NULL;

  values = svz_malloc (sizeof (void *) * hash->keys);

  for (keys = 0, n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      for (e = 0; e < bucket->size; e++)
	{
	  values[keys++] = bucket->entry[e].value;
	  if (keys == hash->keys)
	    return values;
	}
    }
  return values;
}

/*
 * This function delivers all keys within a hash table @var{hash}. It 
 * returns NULL if there were no keys in the hash. You MUST 
 * @code{svz_hash_xfree()} a non-NULL return value.
 */
char **
svz_hash_keys (svz_hash_t *hash)
{
  char **values;
  svz_hash_bucket_t *bucket;
  int n, e, keys;

  if (hash == NULL || hash->keys == 0)
    return NULL;

  values = svz_malloc (sizeof (void *) * hash->keys);

  for (keys = 0, n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      for (e = 0; e < bucket->size; e++)
	{
	  values[keys++] = bucket->entry[e].key;
	  if (keys == hash->keys)
	    return values;
	}
    }
  return values;
}

/*
 * This routine delivers the number of keys in the hash table @var{hash}. If
 * the given @var{hash} is @code{NULL} it returns zero.
 */
int
svz_hash_size (svz_hash_t *hash)
{
  if (hash == NULL)
    return 0;
  return hash->keys;
}

/*
 * This function returns the current capacity of a given hash table @var{hash}.
 */
int
svz_hash_capacity (svz_hash_t *hash)
{
  return hash->buckets;
}

/*
 * This function can be used to determine if some key points to the @var{value}
 * argument in the hash table @var{hash}. Returns the appropriate key or NULL.
 */
char *
svz_hash_contains (svz_hash_t *hash, void *value)
{
  svz_hash_bucket_t *bucket;
  int n, e;

  for (n = 0; n < hash->buckets; n++)
    {
      bucket = &hash->table[n];
      for (e = 0; e < bucket->size; e++)
	{
	  if (bucket->entry[e].value == value)
	    return bucket->entry[e].key;
	}
    }
  return NULL;
}
