/*
 * test/array-test.c - array tests
 *
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: array-test.c,v 1.9 2003/06/14 14:58:00 ela Exp $
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
#include "libserveez/util.h"
#include "libserveez/array.h"
#include "test.h"

/* general array test defines */
#define GAP    5
#define REPEAT 10000
#define test(error) \
  if (error) {      \
    test_failed (); \
    result++;       \
  } else {          \
    test_ok ();     \
  }                 \

/*
 * Main entry point for array tests.
 */
int
main (int argc, char **argv)
{
  int result = 0;
  svz_array_t *array, *dup;
  long n, error, i;
  void *value;

  test_init ();
  test_print ("array function test suite\n");

  /* array creation */
  error = 0;
  test_print ("    create: ");
  if ((array = svz_array_create (0, NULL)) == NULL)
    error++;
  if (svz_array_size (array) != 0)
    error++;
  test (error);

  /* add and get functions */
  test_print ("       add: ");
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, (void *) (n + 1));
  test (svz_array_size (array) != REPEAT);

  test_print ("       get: ");
  for (error = n = 0; n < REPEAT; n++)
    if (svz_array_get (array, n) != (void *) (n + 1))
      error++;
  if (svz_array_get (array, n) != NULL || svz_array_get (array, -1) != NULL)
    error++;
  test (error);

  /* array iteration */
  error = 0;
  test_print ("   iterate: ");
  svz_array_foreach (array, value, n)
    if (value != (void *) (n + 1))
      error++;
  if (n != REPEAT || (unsigned long) n != svz_array_size (array))
    error++;
  test (error);

  /* set function */
  test_print ("       set: ");
  for (error = n = 0; n < REPEAT; n++)
    if (svz_array_set (array, n, (void *) (REPEAT - n)) != (void *) (n + 1))
      error++;
  test (error);

  /* delete function */
  test_print ("    delete: ");
  for (error = n = 0; n < REPEAT; n++)
    if (svz_array_del (array, 0) != (void *) (REPEAT - n))
      error++;
  if (svz_array_size (array) != 0)
    error++;

  if (svz_array_del (array, -1) != NULL || svz_array_del (array, n) != NULL)
    error++;
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, (void *) n);
  for (n = REPEAT - 1; n >= 0; n--)
    if (svz_array_del (array, n) != (void *) n)
      error++;
  if (svz_array_size (array) != 0)
    error++;
  test (error);

  /* array clear function */
  test_print ("     clear: ");
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, (void *) n);
  svz_array_clear (array);
  test (svz_array_size (array) != 0);

  /* check the `contains' function */
  test_print ("  contains: ");
  error = 0;
  for (n = 0; n < REPEAT; n++)
    if (svz_array_contains (array, (void *) n))
      error++;
  for (n = 0; n < REPEAT; n++)
    {
      svz_array_add (array, (void *) n);
      if (svz_array_contains (array, (void *) n) != 1)
	error++;
    }
  for (n = 0; n < REPEAT; n++)
    {
      svz_array_set (array, n, (void *) 0);
      if (svz_array_contains (array, (void *) 0) != (unsigned long) n + 1)
	error++;
    }
  svz_array_clear (array);
  if (svz_array_contains (array, (void *) 0) != 0)
    error++;
  test (error);

  /* check the `index' function */
  test_print ("     index: ");
  error = 0;
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, (void *) 0);
  if (svz_array_idx (array, (void *) 0) != 0)
    error++;
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_array_idx (array, (void *) (n + 1)) != (unsigned long) -1)
	error++;
      svz_array_set (array, n, (void *) (n + 1));
      if (svz_array_idx (array, (void *) (n + 1)) != (unsigned long) n)
	error++;
    }
  test (error);

  /* check the `insert' function */
  test_print ("    insert: ");
  error = 0;
  svz_array_clear (array);
  for (n = 0; n < REPEAT; n++)
    if (svz_array_ins (array, 0, (void *) n) != 0)
      error++;
  if (svz_array_size (array) != REPEAT)
    error++;
  for (n = 0; n < REPEAT; n++)
    if (svz_array_get (array, n) != (void *) (REPEAT - n - 1))
      error++;
  svz_array_clear (array);
  for (n = 0; n < REPEAT; n++)
    if (svz_array_ins (array, n, (void *) n) != (unsigned long) n)
      error++;
  if (svz_array_size (array) != REPEAT)
    error++;
  for (n = 0; n < REPEAT; n++)
    if (svz_array_get (array, n) != (void *) n)
      error++;
  test (error);

  /* stress test */
  error = 0;
  test_print ("    stress: ");

  /* create reverse order */
  for (n = 0; n < REPEAT / 2; n++)
    {
      value = svz_array_get (array, n);
      if (svz_array_set (array, n, svz_array_get (array, REPEAT - n - 1)) !=
	  value)
	error++;
      if (svz_array_set (array, REPEAT - n - 1, value) !=
	  (void *) (REPEAT - n - 1))
	error++;
    }
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_array_idx (array, (void *) n) !=
	  (unsigned long) (REPEAT - n - 1))
	error++;
      if (svz_array_contains (array, (void *) n) != 1)
	error++;
    }
  test_print (error ? "?" : ".");

  /* insert and delete a bit (re-reverse) */
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_array_ins (array, n,
			 svz_array_del (array,
					svz_array_size (array) - 1)) !=
	  (unsigned long) n)
	error++;
      if (svz_array_idx (array, (void *) n) != (unsigned long) n)
	error++;
      if (svz_array_contains (array, (void *) n) != 1)
	error++;
    }
  if (svz_array_size (array) != REPEAT)
    error++;
  test_print (error ? "?" : ".");

  /* process parts of an array */
  for (n = 0; n < REPEAT; n += GAP)
    {
      for (i = 0; i < GAP; i++)
	{
	  if (svz_array_get (array, n + i) != (void *) (n + i))
	    error++;
	  if (svz_array_del (array, n + i) != (void *) (n + i))
	    error++;
	  if (svz_array_ins (array, n + i, (void *) 0xdeadbeef) !=
	      (unsigned long) n + i)
	    error++;
	}
      if (svz_array_size (array) != REPEAT)
	error++;
      if (svz_array_contains (array, (void *) 0xdeadbeef) !=
	  (unsigned long) n + i)
	error++;
      if (svz_array_idx (array, (void *) 0xdeadbeef) != (unsigned long) 0)
	error++;
    }
  test_print (error ? "?" : ".");

  test_print (" ");
  test (error);

  /* replication */
  test_print ("       dup: ");
  svz_array_clear (array);
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, (void *) n);
  error = 0;
  if ((dup = svz_array_dup (NULL)) != NULL)
    error++;
  if ((dup = svz_array_dup (array)) == NULL)
    error++;
  if (svz_array_size (array) != svz_array_size (dup))
    error++;
  svz_array_foreach (dup, value, i)
    if (value != (void *) i)
      error++;
  svz_array_destroy (dup);
  test (error);

  /* replication */
  test_print ("    strdup: ");
  error = 0;
  svz_array_clear (array);
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, NULL);
  if ((dup = svz_array_strdup (NULL)) != NULL)
    error++;
  if ((dup = svz_array_strdup (array)) == NULL)
    error++;
  if (svz_array_size (dup) != svz_array_size (array))
    error++;
  svz_array_foreach (dup, value, i)
    if (value != NULL)
      error++;
  svz_array_destroy (dup);
  svz_array_clear (array);
  for (n = 0; n < REPEAT; n++)
    svz_array_add (array, svz_strdup (svz_itoa (n)));
  if ((dup = svz_array_strdup (array)) == NULL)
    error++;
  if (svz_array_size (dup) != svz_array_size (array))
    error++;
  svz_array_foreach (dup, value, i)
    {
      if (strcmp (value, svz_itoa (i)))
	error++;
      svz_free (svz_array_get (array, i));
    }
  svz_array_destroy (dup);
  test (error);

  /* destroy function */
  test_print ("   destroy: ");
  svz_array_destroy (array);
  test_ok ();

#if DEBUG_MEMORY_LEAKS
  /* is heap ok ? */
  CHECK_LEAKS ();
#endif /* DEBUG_MEMORY_LEAKS */

  return result;
}
