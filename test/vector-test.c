/*
 * test/vector-test.c - vector tests
 *
 * Copyright (C) 2001, 2003 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/vector.h"
#include "test.h"

/* general vector test defines */
#define REPEAT 10000
#define test(error) \
  if (error) {      \
    test_failed (); \
    result++;       \
  } else {          \
    test_ok ();     \
  }                 \

/*
 * Main entry point for vector tests.
 */
int
main (int argc __attribute__ ((unused)), 
      char **argv __attribute__ ((unused)))
{
  int result = 0;
  svz_vector_t *vector;
  long n, error, i, v;
  long *value;

  test_init ();
  test_print ("vector function test suite\n");

  /* vector creation */
  error = 0;
  test_print ("    create: ");
  if ((vector = svz_vector_create (sizeof (long))) == NULL)
    error++;
  if (svz_vector_length (vector) != 0)
    error++;
  test (error);

  /* add function */
  error = 0;
  test_print ("       add: ");
  for (n = 0; n < REPEAT; n++)
    {
      if (svz_vector_add (vector, &n) != (unsigned long) n)
        error++;
      if (svz_vector_length (vector) != (unsigned long) n + 1)
        error++;
    }
  test (error);

  /* get function */
  test_print ("       get: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      value = svz_vector_get (vector, n);
      if (*value != n)
        error++;
    }
  if (svz_vector_get (vector, n) != NULL ||
      svz_vector_get (vector, -1) != NULL)
    error++;
  test (error);

  /* set function */
  test_print ("       set: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      i = REPEAT - n;
      value = svz_vector_set (vector, n, &i);
      if (*value != REPEAT - n)
        error++;
      value = svz_vector_get (vector, n);
      if (*value != REPEAT - n)
        error++;
    }
  test (error);

  /* delete function */
  test_print ("    delete: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      if (svz_vector_del (vector, 0) != (unsigned long) REPEAT - n - 1)
        error++;
      if (svz_vector_length (vector) != (unsigned long) REPEAT - n - 1)
        error++;
    }
  test (error);

  /* insert function */
  test_print ("    insert: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      if (svz_vector_ins (vector, 0, &n) != (unsigned long) n + 1)
        error++;
      if (svz_vector_length (vector) != (unsigned long) n + 1)
        error++;
    }
  for (n = 0; n < REPEAT; n++)
    {
      svz_vector_del (vector, n);
      if (svz_vector_ins (vector, n, &n) != REPEAT)
        error++;
      if (svz_vector_length (vector) != REPEAT)
        error++;
    }
  test (error);

  /* index function */
  test_print ("     index: ");
  for (error = n = 0; n < REPEAT; n++)
    {
      if (svz_vector_idx (vector, &n) != (unsigned long) n)
        error++;
      i = 0xdeadbeef;
      svz_vector_set (vector, n, &i);
      if (svz_vector_idx (vector, &i) != 0)
        error++;
    }
  test (error);

  /* contains function */
  test_print ("  contains: ");
  i = 0xdeadbeef;
  for (error = n = 0; n < REPEAT; n++)
    {
      if (svz_vector_contains (vector, &i) != (unsigned long) REPEAT - n)
        error++;
      svz_vector_set (vector, n, &n);
      if (svz_vector_contains (vector, &n) != 1)
        error++;
    }
  test (error);

  /* clear function */
  test_print ("     clear: ");
  error = 0;
  if (svz_vector_clear (vector) != REPEAT)
    error++;
  test (error);

  /* stress test */
  test_print ("    stress: ");
  svz_vector_destroy (vector);
  for (error = i = 0; i < 10; i++)
    {
      v = test_value (1024) + 1;
      value = svz_malloc (v);
      vector = svz_vector_create (v);
      for (n = 0; n < REPEAT / 5; n++)
        {
          memset (value, (int) (n & 0xff), v);
          if (svz_vector_ins (vector, n, value) != (unsigned long) n + 1)
            error++;
          if (svz_vector_idx (vector, value) != (unsigned long) (n & 0xff))
            error++;
          if (svz_vector_length (vector) != (unsigned long) n + 1)
            error++;
          if (memcmp (svz_vector_get (vector, n), value, v))
            error++;
          if (svz_vector_contains (vector, value) < 1)
            error++;
        }
      svz_vector_destroy (vector);
      svz_free (value);
      test_print (error ? "?" : ".");
    }
  if ((vector = svz_vector_create (sizeof (long))) == NULL)
    error++;
  if (svz_vector_length (vector) != 0)
    error++;
  if (svz_vector_del (vector, 1) != (unsigned long) -1)
    error++;
  if (svz_vector_get (vector, (unsigned long) -1) != NULL)
    error++;
  test_print (" ");
  test (error);

  /* destroy function */
  test_print ("   destroy: ");
  svz_vector_destroy (vector);
  test_ok ();

#if SVZ_ENABLE_DEBUG
  /* is heap ok?  */
  test_print ("      heap: ");
  test (svz_allocated_bytes || svz_allocated_blocks);
#endif /* SVZ_ENABLE_DEBUG */

  return result;
}
