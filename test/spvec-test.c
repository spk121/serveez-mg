/*
 * test/spvec-test.c - sparse vector function tests
 *
 * Copyright (C) 2000, 2001, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: spvec-test.c,v 1.4 2003/06/14 14:58:00 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>

#include "libserveez/alloc.h"
#include "libserveez/sparsevec.h"
#include "test.h"

/* general sparse vector test defines */
#define REPEAT 10000
#define SIZE   1000
#define GAP    5
#define test(error) \
  if (error) {      \
    test_failed (); \
    result++;       \
  } else {          \
    test_ok ();     \
  }                 \

/*
 * Main entry point for sparse vector tests.
 */
int
main (int argc, char **argv)
{
  int result = 0;
  svz_spvec_t *list;
  unsigned long n, error, i;
  void **values;

  test_init ();
  test_print ("sparse vector function test suite\n");

  /* sparse vector creation */
  test_print ("         create: ");
  test ((list = svz_spvec_create ()) == NULL);

  /* add and get */
  test_print ("    add and get: ");
  for (n = error = 0; n < REPEAT; n++)
    {
      svz_spvec_add (list, (void *) 0xdeadbeef);
      if (svz_spvec_get (list, n) != (void *) 0xdeadbeef)
	error++;
    }
  if (svz_spvec_size (list) != REPEAT || svz_spvec_length (list) != REPEAT)
    error++;
  test (error);

  /* contains */
  error = 0;
  test_print ("       contains: ");
  if (svz_spvec_contains (list, NULL))
    error++;
  if (svz_spvec_contains (list, (void *) 0xdeadbeef) != REPEAT)
    error++;
  svz_spvec_add (list, (void *) 0xeabceabc);
  if (svz_spvec_contains (list, (void *) 0xeabceabc) != 1)
    error++;
  test (error);

  /* searching */
  error = 0;
  test_print ("          index: ");
  if (svz_spvec_index (list, (void *) 0xdeadbeef) != 0)
    error++;
  if (svz_spvec_index (list, (void *) 0xeabceabc) != REPEAT)
    error++;
  if (svz_spvec_index (list, NULL) != (unsigned long) -1)
    error++;
  test (error);

  /* deletion */
  error = 0;
  test_print ("         delete: ");
  if (svz_spvec_delete (list, REPEAT) != (void *) 0xeabceabc)
    error++;
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_spvec_delete (list, 0) != (void *) 0xdeadbeef)
	error++;
    }
  if (svz_spvec_length (list) || svz_spvec_size (list))
    error++;
  test (error);

  /* insertion */
  error = 0;
  test_print ("         insert: ");
  for (n = 0; n < REPEAT; n++)
    {
      svz_spvec_insert (list, 0, (void *) 0xeabceabc);
      if (svz_spvec_get (list, 0) != (void *) 0xeabceabc)
	error++;
      if (svz_spvec_delete (list, 0) != (void *) 0xeabceabc)
	error++;

      svz_spvec_insert (list, n, (void *) 0xdeadbeef);
      if (svz_spvec_get (list, n) != (void *) 0xdeadbeef)
	error++;
    }
  if (svz_spvec_length (list) != REPEAT || svz_spvec_size (list) != REPEAT)
    error++;
  test (error);

  /* set (replace) and unset (clear) */
  svz_spvec_clear (list);
  error = 0;
  test_print ("  set and unset: ");
  for (n = 0; n < REPEAT / GAP; n++)
    {
      if (svz_spvec_set (list, n * GAP, (void *) (n * GAP)) != NULL)
	error++;
      if (svz_spvec_unset (list, n * GAP) != (void *) (n * GAP))
	error++;

      for (i = GAP - 1; i > 0; i--)
	{
	  if (svz_spvec_set (list, n * GAP + i, (void *) ((n * GAP) + i)) !=
	      NULL)
	    error++;
	  if (svz_spvec_unset (list, n * GAP + i) != (void *) ((n * GAP) + i))
	    error++;
	}
    }

  if (svz_spvec_length (list) != 0 || svz_spvec_size (list) != 0)
    error++;
  for (n = 0; n < REPEAT; n++)
    svz_spvec_add (list, (void *) n);
  for (n = REPEAT; n != 0; n--)
    {
      if (svz_spvec_unset (list, n - 1) != (void *) (n - 1))
	error++;
      if (svz_spvec_size (list) != (unsigned) n - 1)
	error++;
    }

  test (error);

  /* set (replace) and get */
  svz_spvec_clear (list);
  error = 0;
  test_print ("    set and get: ");
  for (n = 0; n < REPEAT / GAP; n++)
    {
      if (svz_spvec_set (list, n * GAP, (void *) (n * GAP)) != NULL)
	error++;
      if (svz_spvec_get (list, n * GAP) != (void *) (n * GAP))
	error++;

      for (i = GAP - 1; i > 0; i--)
	{
	  if (svz_spvec_set (list, n * GAP + i, (void *) ((n * GAP) + i)) !=
	      NULL)
	    error++;
	  if (svz_spvec_get (list, n * GAP + i) != (void *) ((n * GAP) + i))
	    error++;
	}
    }

  if (svz_spvec_length (list) != REPEAT || svz_spvec_size (list) != REPEAT)
    error++;
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_spvec_get (list, n) != (void *) n)
	error++;
    }
  test (error);

  /* values */
  error = 0;
  test_print ("         values: ");
  if ((values = svz_spvec_values (list)) != NULL)
    {
      for (n = 0; n < REPEAT; n++)
	{
	  if (values[n] != (void *) n)
	    error++;
	}
      svz_free (values);
    }
  else
    error++;
  test (error);

  /* pack */
  error = 0;
  svz_spvec_clear (list);
  test_print ("           pack: ");
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_spvec_set (list, n * GAP, (void *) n) != NULL)
	error++;
    }
  if (svz_spvec_length (list) != REPEAT * GAP - GAP + 1 ||
      svz_spvec_size (list) != REPEAT)
    error++;
  svz_spvec_pack (list);
  if (svz_spvec_length (list) != REPEAT || svz_spvec_size (list) != REPEAT)
    error++;
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_spvec_get (list, n) != (void *) n)
	error++;
    }
  test (error);

  /* range deletion */
  error = 0;
  test_print ("   delete range: ");
  svz_spvec_set (list, 0, (void *) 0xdeadbeef);
  for (n = 0; (unsigned) n < svz_spvec_length (list) - GAP; n++)
    {
      if (svz_spvec_delete_range (list, n, n + GAP) != GAP)
	error++;
      n += GAP;
    }
  n++;
  if (svz_spvec_size (list) != (unsigned) n ||
      svz_spvec_length (list) != (unsigned) n)
    error++;
  for (i = GAP, n = 0; (unsigned) n < svz_spvec_length (list) - 1; n++, i++)
    {
      if (svz_spvec_get (list, n) != (void *) i)
	error++;
      if (((n + 1) % (GAP + 1)) == 0)
	i += GAP;
    }
  n = svz_spvec_length (list);
  if (svz_spvec_delete_range (list, 0, n) != (unsigned) n)
    error++;
  if (svz_spvec_size (list) != 0 || svz_spvec_length (list) != 0)
    error++;
  test (error);

  /* stress test */
  error = 0;
  svz_spvec_clear (list);
  test_print ("         stress: ");

  /* put any values to array until a certain size (no order) */
  while (svz_spvec_size (list) != SIZE)
    {
      n = test_value (SIZE);
      if (svz_spvec_get (list, n) != (void *) (n + SIZE))
	{
	  svz_spvec_set (list, n, (void *) (n + SIZE));
	}
    }

  /* check for final size and length */
  if (svz_spvec_size (list) != SIZE || svz_spvec_length (list) != SIZE)
    error++;
  test_print (error ? "?" : ".");

  /* check contains(), index() and get() */
  for (n = 0; n < SIZE; n++)
    {
      if (svz_spvec_contains (list, (void *) (n + SIZE)) != 1)
	error++;
      if (svz_spvec_index (list, (void *) (n + SIZE)) != n)
	error++;
      if (svz_spvec_get (list, n) != (void *) (n + SIZE))
	error++;
    }
  test_print (error ? "?" : ".");

  /* delete all values */
  for (n = 0; n < SIZE; n++)
    {
      if (svz_spvec_delete (list, 0) != (void *) (n + SIZE))
	error++;
    }

  /* check "post" size */
  if (svz_spvec_size (list) || svz_spvec_length (list))
    error++;
  test_print (error ? "?" : ".");

  /* build array insert()ing values */
  while (svz_spvec_size (list) != REPEAT)
    {
      n = test_value (REPEAT);
      svz_spvec_insert (list, n, (void *) 0xdeadbeef);
    }

  /* check size and length of array */
  if (svz_spvec_size (list) > svz_spvec_length (list))
    error++;

  /* check all values */
  if (svz_spvec_contains (list, (void *) 0xdeadbeef) != REPEAT)
    error++;
  test_print (error ? "?" : ".");

  /* save values, pack() list and check "post" get() values */
  if ((values = svz_spvec_values (list)) != NULL)
    {
      svz_spvec_pack (list);
      if (svz_spvec_size (list) != REPEAT ||
	  svz_spvec_length (list) != REPEAT)
	error++;
      for (n = 0; n < REPEAT; n++)
	{
	  if (svz_spvec_get (list, n) != values[n] ||
	      values[n] != (void *) 0xdeadbeef)
	    error++;
	}
      svz_free (values);
    }
  else
    error++;
  test_print (error ? "?" : ".");

  /* delete each value, found by index() and check it via contains() */
  n = REPEAT;
  while (svz_spvec_size (list))
    {
      if (svz_spvec_delete (list, svz_spvec_index (list, (void *) 0xdeadbeef))
	  != (void *) 0xdeadbeef)
	error++;
      if (svz_spvec_contains (list, (void *) 0xdeadbeef) != (unsigned) --n)
	error++;
    }

  /* check "post" size */
  if (svz_spvec_size (list) || svz_spvec_length (list))
    error++;
  test_print (error ? "?" : ".");

  for (i = SIZE; i < SIZE + 10; i++)
    {
      /* build sparse vector */
      while (svz_spvec_size (list) != (unsigned) i)
	{
	  n = test_value (10 * i) + 1;
	  svz_spvec_insert (list, n, (void *) n);
	  if (svz_spvec_get (list, n) != (void *) n)
	    error++;
	}

      /* delete all values by chance */
      while (svz_spvec_size (list))
	{
	  svz_spvec_delete_range (list, test_value (i),
				  test_value (10 * i * 5 + 1));
	}
      test_print (error ? "?" : ".");
    }

  /* check functions set() and unset() */
  for (i = 0; i < REPEAT; i++)
    {
      n = test_value (i * 20 + test_value (20));
      if (svz_spvec_set (list, n, (void *) n) != NULL)
	error++;
      if ((void *) n != svz_spvec_unset (list, n))
	error++;
    }
  test_print (error ? "?" : ".");

  test_print (" ");
  test (error);

  /* sparse vector destruction */
  test_print ("        destroy: ");
  svz_spvec_destroy (list);
  test_ok ();

#if DEBUG_MEMORY_LEAKS
  /* is heap ok ? */
  CHECK_LEAKS ();
#endif /* DEBUG_MEMORY_LEAKS */

  return result;
}
