/*
 * test/hash-test.c - hash table tests
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
 * $Id: hash-test.c,v 1.15 2003/06/14 14:58:00 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/hash.h"
#include "test.h"

#define REPEAT 10000
#define test(error) \
  if (error) {      \
    test_failed (); \
    result++;       \
  } else {          \
    test_ok ();     \
  }                 \

/*
 * Main entry point for hash tests.
 */
int
main (int argc, char **argv)
{
  int result = 0;
  svz_hash_t *hash;
  long n, error, val;
  char *text;
  char **keys;
  void **values;

  test_print ("hash function test suite\n");

  /* hash creation */
  test_print ("             create: ");
  test ((hash = svz_hash_create (4, NULL)) == NULL);

  /* hash put and get */
  test_print (" put/get and exists: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      text = svz_strdup (test_string ());
      svz_hash_put (hash, text, (void *) 0xdeadbeef);
      if (((void *) 0xdeadbeef != svz_hash_get (hash, text)))
	error++;
      if (svz_hash_exists (hash, text) == 0)
	error++;
      svz_free (text);
    }
  test (error);

  /* hash containing a certain value */
  test_print ("           contains: ");
  error = 0;
  if (svz_hash_contains (hash, NULL))
    error++;
  if (svz_hash_contains (hash, (void *) 0xeabceabc))
    error++;
  svz_hash_put (hash, "1234567890", (void *) 0xeabceabc);
  if (strcmp ("1234567890", svz_hash_contains (hash, (void *) 0xeabceabc)))
    error++;
  test (error);

  /* hash key deletion */
  test_print ("             delete: ");
  error = 0;
  n = svz_hash_size (hash);
  if ((void *) 0xeabceabc != svz_hash_delete (hash, "1234567890"))
    error++;
  if (n - 1 != svz_hash_size (hash))
    error++;
  test (error);

  /* keys and values */
  test_print ("    keys and values: ");
  svz_hash_clear (hash);
  error = 0;
  text = svz_malloc (16);
  for (n = 0; n < REPEAT; n++)
    {
      sprintf (text, "%015lu", (unsigned long) n);
      svz_hash_put (hash, text, (void *) n);
      if (svz_hash_get (hash, text) != (void *) n)
	error++;
    }
  svz_free (text);
  if (n != svz_hash_size (hash))
    error++;
  values = svz_hash_values (hash);
  keys = svz_hash_keys (hash);
  if (keys && values)
    {
      for (n = 0; n < REPEAT; n++)
	{
	  if (atol (keys[n]) != (long) values[n])
	    error++;
	  if (svz_hash_get (hash, keys[n]) != values[n])
	    error++;
	  if (svz_hash_contains (hash, values[n]) != keys[n])
	    error++;
	}
      svz_hash_xfree (keys);
      svz_hash_xfree (values);
    }
  else
    error++;
  if (svz_hash_size (hash) != REPEAT)
    error++;
  test (error);

  /* rehashing */
  error = 0;
  test_print ("             rehash: ");
  while (hash->buckets > HASH_MIN_SIZE)
    svz_hash_rehash (hash, HASH_SHRINK);
  while (hash->buckets < svz_hash_size (hash) * 10)
    svz_hash_rehash (hash, HASH_EXPAND);
  while (hash->buckets > HASH_MIN_SIZE)
    svz_hash_rehash (hash, HASH_SHRINK);
  while (hash->buckets < svz_hash_size (hash) * 10)
    svz_hash_rehash (hash, HASH_EXPAND);
  text = svz_malloc (16);
  for (n = 0; n < REPEAT; n++)
    {
      sprintf (text, "%015lu", (unsigned long) n);
      if (svz_hash_get (hash, text) != (void *) n)
	error++;
      if (svz_hash_delete (hash, text) != (void *) n)
	error++;
    }
  if (svz_hash_size (hash))
    error++;
  svz_free (text);
  test (error);

  /* hash clear */
  test_print ("              clear: ");
  svz_hash_clear (hash);
  test (svz_hash_size (hash));

  /* hash destruction */
  test_print ("            destroy: ");
  svz_hash_destroy (hash);
  test_ok ();

  /* hash iteration */
  hash = svz_hash_create (4, NULL);

  svz_hash_put (hash, "1234567890", (void *) 1);
  svz_hash_put (hash, "1234567891", (void *) 2);
  svz_hash_put (hash, "1234567892", (void *) 3);

  error = 0;
  val = 0;
  test_print ("    value iteration: ");
  svz_hash_foreach_value (hash, values, n)
    {
      val += (long) values[n];
      error++;
    }
  test (error != 3 || val != 6);

  error = 0;
  val = 0;
  test_print ("      key iteration: ");
  svz_hash_foreach_key (hash, keys, n)
    {
      val += (long) svz_hash_get (hash, keys[n]);
      error++;
    }
  test (error != 3 || val != 6);
  svz_hash_destroy (hash);

  return result;
}
