/*
 * array.c - array functions
 *
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
 * Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
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
 * $Id: array.c,v 1.16 2003/05/31 12:12:09 ela Exp $
 *
 */

#include <config.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libserveez/alloc.h"
#include "libserveez/util.h"
#include "libserveez/array.h"

/*
 * Create a new array with the initial capacity @var{capacity} and return
 * a pointer to it. If @var{capacity} is zero it defaults to some value. The
 * @var{destroy} argument allows you to release dynamic allocated memory when
 * calling @code{svz_array_clear()} and @code{svz_array_destroy()}. If the
 * array contains data allocated by @code{svz_malloc()} you need to set
 * @var{destroy} to @code{svz_free()}. For structured data you can pass a 
 * user defined routine which recurses into the structure. If the array 
 * contains data which should not be released you must set @var{destroy} 
 * to @code{NULL}.
 */
svz_array_t *
svz_array_create (unsigned long capacity, svz_free_func_t destroy)
{
  svz_array_t *array;

  if (!capacity)
    capacity = 4;
  array = svz_malloc (sizeof (svz_array_t));
  memset (array, 0, sizeof (svz_array_t));
  array->data = svz_malloc (sizeof (void *) * capacity);
  array->capacity = capacity;
  array->destroy = destroy;
  return array;
}

/*
 * Delete all values within the array @var{array} and set its size to zero.
 * The array @var{array} itself keeps valid. Do not perform any operation
 * if @var{array} is @code{NULL}. If you passed a @var{destroy} function to
 * @code{svz_array_create()} the routine calls this function passing each
 * element of @var{array} to it.
 */
void
svz_array_clear (svz_array_t *array)
{
  if (array == NULL || array->data == NULL)
    return;

  if (array->destroy != NULL)
    {
      unsigned long n;
      for (n = 0; n < array->size; n++)
	array->destroy (array->data[n]);
    }
  svz_free (array->data);
  array->data = NULL;
  array->size = 0;
  array->capacity = 0;
}

/*
 * Completely destroy the array @var{array}. The @var{array} handle is
 * invalid afterwards. The routine runs the @var{destroy} callback for each
 * element of the array.
 */
void
svz_array_destroy (svz_array_t *array)
{
  if (array)
    {
      svz_array_clear (array);
      svz_free (array);
    }
}

/*
 * Check if the given @var{size} argument supersedes the capacity of the
 * array @var{array} and reallocate the array if necessary.
 */
static void
svz_array_ensure_capacity (svz_array_t *array, unsigned long size)
{
  if (size > array->capacity)
    {
      array->capacity = array->capacity * 3 / 2 + 1;
      array->data = svz_realloc (array->data, sizeof (void *) * 
				 array->capacity);
    }
}

/*
 * Return the array element at the position @var{index} of the array 
 * @var{array} if the index is within the array range. Return @code{NULL}
 * if not.
 */
void *
svz_array_get (svz_array_t *array, unsigned long index)
{
  if (array == NULL || index >= array->size)
    return NULL;
  return array->data[index];
}

/*
 * Replace the array element at the position @var{index} of the array
 * @var{array} with the value @var{value} and return the previous value
 * at this index. Returns @code{NULL} and does not perform any operation
 * if @var{array} is @code{NULL} or the @var{index} is out of the array
 * range.
 */
void *
svz_array_set (svz_array_t *array, unsigned long index, void *value)
{
  void *prev;

  if (array == NULL || index >= array->size)
    return NULL;
  prev = array->data[index];
  array->data[index] = value;
  return prev;
}

/*
 * Append the value @var{value} at the end of the array @var{array}. Does
 * not perform any operation if @var{array} is @code{NULL}.
 */
void
svz_array_add (svz_array_t *array, void *value)
{
  if (array)
    {
      svz_array_ensure_capacity (array, array->size + 1);
      array->data[array->size++] = value;
    }
}

/*
 * Remove the array element at the position @var{index} of the array
 * @var{array}. Return its previous value or @code{NULL} if the index
 * is out of the arrays range.
 */
void *
svz_array_del (svz_array_t *array, unsigned long index)
{
  void *value;

  if (array == NULL || index >= array->size)
    return NULL;
  value = array->data[index];
  if (index != array->size - 1)
    memmove (&array->data[index], &array->data[index + 1], 
	     (array->size - index - 1) * sizeof (void *));
  array->size--;
  return value;
}

/*
 * Return the given arrays @var{array} current capacity.
 */
unsigned long
svz_array_capacity (svz_array_t *array)
{
  if (array == NULL)
    return 0;
  return array->capacity;
}

/*
 * Return the given arrays @var{array} current size.
 */
unsigned long
svz_array_size (svz_array_t *array)
{
  if (array == NULL)
    return 0;
  return array->size;
}

/*
 * Returns how often the given value @var{value} is stored in the array
 * @var{array}. Return zero if there is no such value.
 */
unsigned long
svz_array_contains (svz_array_t *array, void *value)
{
  unsigned long n, found;

  if (array == NULL)
    return 0;
  for (found = n = 0; n < array->size; n++)
    if (array->data[n] == value)
      found++;
  return found;
}

/*
 * This function returns the index of the first occurrence of the value 
 * @var{value} in the array @var{array}. It returns (-1) if there is no
 * such value stored within the array.
 */
unsigned long
svz_array_idx (svz_array_t *array, void *value)
{
  unsigned long n;

  if (array == NULL)
    return (unsigned long) -1;
  for (n = 0; n < array->size; n++)
    if (array->data[n] == value)
      return n;
  return (unsigned long) -1;
}

/*
 * This routine inserts the given value @var{value} at the position 
 * @var{index}. The indices of all following values in the array @var{array}
 * and the size of the array get automatically incremented. Return the
 * values index or (-1) if the index is out of array bounds.
 */
unsigned long
svz_array_ins (svz_array_t *array, unsigned long index, void *value)
{
  if (array == NULL || index > array->size)
    return (unsigned long) -1;
  svz_array_ensure_capacity (array, array->size + 1);
  if (index < array->size)
    memmove (&array->data[index + 1], &array->data[index], 
	     (array->size - index) * sizeof (void *));
  array->data[index] = value;
  array->size++;
  return index;
}

/*
 * This function replicates the given array @var{array}. It returns
 * @code{NULL} if there is nothing to do and an identical copy if the
 * array otherwise.
 */
svz_array_t *
svz_array_dup (svz_array_t *array)
{
  svz_array_t *dup;

  if (array == NULL)
    return NULL;
  dup = svz_array_create (array->size, array->destroy);
  dup->size = array->size;
  if (array->size)
    memcpy (dup->data, array->data, array->size * sizeof (void *));
  return dup;
}

/*
 * This function works something like @code{svz_array_dup()} but considers
 * the values within the array @var{array} to be zero-terminated character 
 * strings and duplicates these via @code{svz_strdup()}.
 */
svz_array_t *
svz_array_strdup (svz_array_t *array)
{
  svz_array_t *dup;
  unsigned long n;

  if (array == NULL)
    return NULL;
  dup = svz_array_create (array->size, svz_free);
  dup->size = array->size;
  for (n = 0; n < array->size; n++)
    dup->data[n] = svz_strdup (array->data[n]);
  return dup;
}

/*
 * Create a @code{NULL} terminated C array containing the values of the 
 * given @var{array}. If the given @var{array} is @code{NULL} then an empty 
 * C array is returned. It is your responsibility to @code{svz_free()} the 
 * returned pointer.
 */
void **
svz_array_values (svz_array_t *array)
{
  unsigned long length = array ? array->size : 0;
  void **carray = (void **) svz_malloc (sizeof (void *) * (length + 1));

  if (array != NULL)
    memcpy (carray, array->data, sizeof (void *) * length);
  carray[length] = NULL;
  return carray;
}

/*
 * This function destroys the given array @var{array} if it holds no
 * elements and returns @code{NULL} in this case.  Otherwise the
 * function returns the given array.
 */
svz_array_t *
svz_array_destroy_zero (svz_array_t *array)
{
  if (array && array->size == 0)
    {
      svz_array_destroy (array);
      return NULL;
    }
  return array;
}
