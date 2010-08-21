/*
 * guile-bin.c - binary data exchange layer for Guile servers
 *
 * Copyright (C) 2001, 2002, 2003 Stefan Jahn <stefan@lkcc.org>
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
 * $Id: guile-bin.c,v 1.30 2003/08/26 04:59:33 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_GUILE_SERVER

#include <stdio.h>
#include <string.h>

#if GUILE_SOURCE
# include <libguile/gh.h>
#else
# include <guile/gh.h>
#endif

#include "libserveez.h"
#include "guile-api.h"
#include "guile-bin.h"

/*
 * Structure definition of the data the binary smob refers to.
 */
typedef struct guile_bin
{
  unsigned char *data; /* data pointer */
  int size;            /* size of the above data */ 
  int garbage;         /* if set the data pointer got allocated by
			  the smob functions */
}
guile_bin_t;

/* The smob tag. */
static scm_t_bits guile_bin_tag = 0;

/* Useful defines for accessing the binary smob. */
#define GET_BIN_SMOB(binary) \
  ((guile_bin_t *) ((unsigned long) SCM_SMOB_DATA (binary)))
#define CHECK_BIN_SMOB(binary) \
  (SCM_NIMP (binary) && SCM_TYP16 (binary) == guile_bin_tag)
#define CHECK_BIN_SMOB_ARG(binary, arg, var)                       \
  if (!CHECK_BIN_SMOB (binary))                                    \
    scm_wrong_type_arg_msg (FUNC_NAME, arg, binary, "svz-binary"); \
  var = GET_BIN_SMOB (binary)
#define MAKE_BIN_SMOB()                                    \
  ((guile_bin_t *) ((void *)                               \
    scm_gc_malloc (sizeof (guile_bin_t), "svz-binary")))

/* Smob test function: Returns @code{#t} if the given cell @var{binary} is 
   an instance of the binary smob type. */
#define FUNC_NAME "binary?"
static SCM
guile_bin_p (SCM binary)
{
  return CHECK_BIN_SMOB (binary) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

/* Smob print function: Displays a text representation of the given
   cell @var{binary} to the output port @var{port}. */
static int
guile_bin_print (SCM binary, SCM port, scm_print_state *state)
{
  guile_bin_t *bin = GET_BIN_SMOB (binary);
  static char txt[256];

  sprintf (txt, "#<svz-binary %p, size: %d>", bin->data, bin->size);
  scm_puts (txt, port);
  return 1;
}

/* Smob free function: Releases any allocated resources used the given
   cell @var{binary}. No need to mark any referring scheme cell. Returns
   the number of bytes actually free()'d. */
static size_t
guile_bin_free (SCM binary)
{
  guile_bin_t *bin = GET_BIN_SMOB (binary);
  size_t size = sizeof (guile_bin_t);

  /* Free the data pointer if it has been allocated by ourselves and
     is not just a reference. */
  if (bin->garbage)
    {
      size += bin->size;
      scm_gc_free ((void *) bin->data, bin->size, "svz-binary-data");
    }
  scm_gc_free ((void *) bin, sizeof (guile_bin_t), "svz-binary");
  return size;
}

/* Smob equal function: Return #t if the both cells @var{a} and @var{b}
   are definitely or virtually equal. Otherwise return #f. */
static SCM
guile_bin_equal (SCM a, SCM b)
{
  guile_bin_t *bin1 = GET_BIN_SMOB (a);
  guile_bin_t *bin2 = GET_BIN_SMOB (b);

  if (bin1 == bin2)
    return SCM_BOOL_T;
  else if (bin1->size == bin2->size)
    {
      if (bin1->data == bin2->data)
	return SCM_BOOL_T;
      else if (memcmp (bin1->data, bin2->data, bin1->size) == 0)
	return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}

/* Converts the given string cell @var{string} into a binary smob. The data
   pointer of the binary smob is marked as garbage which must be free()'d 
   in the sweep phase of the garbage collector. */
#define FUNC_NAME "string->binary"
SCM
guile_string_to_bin (SCM string)
{
  guile_bin_t *bin;

  SCM_ASSERT_TYPE (SCM_STRINGP (string), string, 
		   SCM_ARG1, FUNC_NAME, "string");

  bin = MAKE_BIN_SMOB ();
  bin->size = SCM_NUM2INT (SCM_ARG1, scm_string_length (string));
  if (bin->size > 0)
    {
      bin->data = (unsigned char *) 
	scm_gc_malloc (bin->size, "svz-binary-data");
      memcpy (bin->data, SCM_STRING_CHARS (string), bin->size);
      bin->garbage = 1;
    }
  else
    {
      bin->data = NULL;
      bin->garbage = 0;
    }

  SCM_RETURN_NEWSMOB (guile_bin_tag, bin);
}
#undef FUNC_NAME

/* Converts the given binary smob @var{binary} into a string. Returns the
   string cell itself. */
#define FUNC_NAME "binary->string"
SCM
guile_bin_to_string (SCM binary)
{
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  return scm_mem2string ((char *) bin->data, bin->size);
}
#undef FUNC_NAME

/* This routine searches through the binary smob @var{binary} for the cell
   @var{needle}. The latter argument can be either an exact number, character,
   string or another binary smob. It returns @code{#f} if the needle could 
   not be found and a positive number indicates the position of the first 
   occurrence of @var{needle} in the binary smob @var{binary}. */
#define FUNC_NAME "binary-search"
SCM
guile_bin_search (SCM binary, SCM needle)
{
  SCM ret = SCM_BOOL_F;
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT (SCM_STRINGP (needle) || SCM_CHARP (needle) || 
	      SCM_EXACTP (needle) || CHECK_BIN_SMOB (needle),
	      needle, SCM_ARG2, FUNC_NAME);

  /* Search for a pattern. */
  if (SCM_STRINGP (needle) || CHECK_BIN_SMOB (needle))
    {
      guile_bin_t *search = NULL;
      int len;
      unsigned char *p, *end, *start;

      if (CHECK_BIN_SMOB (needle))
	search = GET_BIN_SMOB (needle);
      len = search ? search->size : SCM_NUM2INT (SCM_ARG2, 
						 scm_string_length (needle));
      p = search ? search->data : SCM_STRING_UCHARS (needle);
      start = bin->data;
      end = start + bin->size - len;

      /* Return #f if searching in empty data sets. */
      if (len == 0 || p == NULL || start == NULL)
	return ret;

      /* Iterate the data. */
      while (start <= end)
	{
	  if (*start == *p && memcmp (start, p, len) == 0)
	    {
	      ret = scm_int2num (start - bin->data);
	      break;
	    }
	  start++;
	} 
    }

  /* Search for a single byte. */
  else if (SCM_CHARP (needle) || SCM_EXACTP (needle))
    {
      unsigned char c;
      unsigned char *p, *end;

      c = (unsigned char)
	(SCM_CHARP (needle) ? SCM_CHAR (needle) : 
	 (unsigned char) SCM_NUM2INT (SCM_ARG2, needle));
      p = bin->data;
      end = p + bin->size;

      while (p < end)
	{
	  if (*p == c)
	    {
	      ret = scm_int2num (p - bin->data);
	      break;
	    }
	  p++;
	}
    }
  return ret;
}
#undef FUNC_NAME

/* Performs an in place reversal of the given binary smob @var{binary}
   and returns it. */
#define FUNC_NAME "binary-reverse!"
SCM
guile_bin_reverse_x (SCM binary)
{
  guile_bin_t *bin;
  int first, last;
  unsigned char b;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);

  for (first = 0, last = bin->size - 1; first < last; first++, last--)
    {
      b = bin->data[first];
      bin->data[first] = bin->data[last];
      bin->data[last] = b;
    }
  return binary;
}
#undef FUNC_NAME

/* Returns a new binary smob with the reverse byte order of the given
   binary smob @var{binary}. */
#define FUNC_NAME "binary-reverse"
SCM
guile_bin_reverse (SCM binary)
{
  guile_bin_t *bin, *reverse;
  int first, last;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  reverse = MAKE_BIN_SMOB ();
  reverse->size = bin->size;

  /* Return empty smob if necessary. */
  if (reverse->size == 0)
    {
      reverse->garbage = 0;
      reverse->data = NULL;
      SCM_RETURN_NEWSMOB (guile_bin_tag, reverse);
    }

  /* Reserve some memory for the new smob. */
  reverse->data = (unsigned char *) 
    scm_gc_malloc (reverse->size, "svz-binary-data");
  reverse->garbage = 1;

  /* Apply reverse byte order to the new smob. */
  for (first = 0, last = reverse->size - 1; first < reverse->size; )
    reverse->data[first++] = bin->data[last--];

  SCM_RETURN_NEWSMOB (guile_bin_tag, reverse);
}
#undef FUNC_NAME

/* Set the byte at position @var{index} of the binary smob @var{binary} to
   the value given in @var{value} which can be either a character or an
   exact number. */
#define FUNC_NAME "binary-set!"
SCM
guile_bin_set_x (SCM binary, SCM index, SCM value)
{
  guile_bin_t *bin;
  int idx;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT_TYPE (SCM_EXACTP (index), index, SCM_ARG2, FUNC_NAME, "exact");
  SCM_ASSERT_TYPE (SCM_EXACTP (value) || SCM_CHARP (value), 
		   value, SCM_ARG3, FUNC_NAME, "char or exact");

  /* Check the range of the index argument. */
  idx = SCM_NUM2INT (SCM_ARG2, index);
  if (idx < 0 || idx >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, index);

  bin->data[idx] = (unsigned char)
    (SCM_CHARP (value) ? SCM_CHAR (value) : 
     (unsigned char) SCM_NUM2INT (SCM_ARG3, value));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Obtain the byte at position @var{index} of the binary smob 
   @var{binary}. */
#define FUNC_NAME "binary-ref"
SCM
guile_bin_ref (SCM binary, SCM index)
{
  guile_bin_t *bin;
  int idx;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT_TYPE (SCM_EXACTP (index), index, SCM_ARG2, FUNC_NAME, "exact");

  /* Check the range of the index argument. */
  idx = SCM_NUM2INT (SCM_ARG2, index);
  if (idx < 0 || idx >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, index);

  return SCM_MAKE_CHAR (bin->data[idx]);
}
#undef FUNC_NAME

/* Return the size in bytes of the binary smob @var{binary}. */
#define FUNC_NAME "binary-length"
SCM
guile_bin_length (SCM binary)
{
  guile_bin_t *bin;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  return scm_int2num (bin->size);
}
#undef FUNC_NAME

/* Append either the binary smob or string @var{append} onto the binary
   smob @var{binary}. If @var{binary} has been a simple data pointer
   reference it is then a standalone binary smob as returned by
   @code{string->binary}. */
#define FUNC_NAME "binary-concat!"
SCM
guile_bin_concat_x (SCM binary, SCM append)
{
  guile_bin_t *bin, *concat = NULL;
  int len, equal;
  unsigned char *p;
  
  /* Check arguments first. */
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT (SCM_STRINGP (append) || CHECK_BIN_SMOB (append),
	      append, SCM_ARG2, FUNC_NAME);

  if (CHECK_BIN_SMOB (append))
    concat = GET_BIN_SMOB (append);
  len = concat ? 
    concat->size : SCM_NUM2INT (SCM_ARG2, scm_string_length (append));
  p = concat ? concat->data : SCM_STRING_UCHARS (append);
  equal = (p == bin->data) ? 1 : 0;

  /* Return here if there is nothing to concatenate. */
  if (len <= 0)
    return binary;

  if (bin->garbage)
    {
      bin->data = (unsigned char *) 
	scm_gc_realloc ((void *) bin->data, bin->size, bin->size + len, 
			"svz-binary-data");
    }
  else
    {
      unsigned char *odata = bin->data;
      bin->data = (unsigned char *) 
	scm_gc_malloc (bin->size + len, "svz-binary-data");
      memcpy (bin->data, odata, bin->size);
    }

  /* Reapply concatenation pointer if identical binaries have been passed. */
  p = equal ? bin->data : p;

  memcpy (bin->data + bin->size, p, len);
  bin->size += len;
  bin->garbage = 1;
  return binary;
}
#undef FUNC_NAME

/* Create a subset binary smob from the given binary smob @var{binary}. The
   range of this subset is specified by @var{start} and @var{end} both
   inclusive (thus the resulting size is = @var{end} - @var{start} + 1). 
   With a single exception: If @var{end} is not given or specified with -1 
   the routine returns all data until the end of @var{binary}. */
#define FUNC_NAME "binary-subset"
SCM
guile_bin_subset (SCM binary, SCM start, SCM end)
{
  guile_bin_t *bin, *ret;
  int from, to;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  SCM_ASSERT_TYPE (SCM_EXACTP (start), start, SCM_ARG2, FUNC_NAME, "exact");
  SCM_ASSERT_TYPE (SCM_EXACTP (end) || SCM_UNBNDP (end), 
		   end, SCM_ARG3, FUNC_NAME, "exact");

  from = SCM_NUM2INT (SCM_ARG2, start);
  to = SCM_UNBNDP (end) ? -1 : SCM_NUM2INT (SCM_ARG3, end);
  if (to == -1)
    to = bin->size - 1;

  /* Check the ranges of both indices. */
  if (from < 0 || from >= bin->size)
    SCM_OUT_OF_RANGE (SCM_ARG2, start);
  if (to < 0 || to >= bin->size || to < from)
    SCM_OUT_OF_RANGE (SCM_ARG3, end);

  ret = MAKE_BIN_SMOB ();
  ret->size = to - from + 1;
  ret->data = bin->data + from;
  ret->garbage = 0;

  SCM_RETURN_NEWSMOB (guile_bin_tag, ret);
}
#undef FUNC_NAME

/* Convert the given binary smob @var{binary} into a scheme list. The list
   is empty if the size of @var{binary} is zero. */
#define FUNC_NAME "binary->list"
SCM
guile_bin_to_list (SCM binary)
{
  guile_bin_t *bin;
  unsigned char *p;
  SCM list;

  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);
  for (list = SCM_EOL, p = bin->data + bin->size; p-- > bin->data; )
    list = scm_cons (scm_ulong2num (*p), list);
  return list;
}
#undef FUNC_NAME

/* Convert the scheme list @var{list} into a binary smob. Each of the 
   elements of @var{list} is checked for validity.  The elements can be
   either exact numbers in a byte's range or characters. */
#define FUNC_NAME "list->binary"
SCM
guile_list_to_bin (SCM list)
{
  guile_bin_t *bin;
  unsigned char *p;
  int value;
  SCM val;

  SCM_ASSERT_TYPE (SCM_LISTP (list), list, SCM_ARG1, FUNC_NAME, "list");
  bin = MAKE_BIN_SMOB ();
  bin->size = SCM_NUM2ULONG (SCM_ARG1, scm_length (list));

  if (bin->size > 0)
    {
      p = bin->data = (unsigned char *) 
	scm_gc_malloc (bin->size, "svz-binary-data");
      bin->garbage = 1;
    }
  else
    {
      bin->garbage = 0;
      bin->data = NULL;
      SCM_RETURN_NEWSMOB (guile_bin_tag, bin);
    }
	
  /* Iterate over the list and build up binary smob. */
  while (SCM_PAIRP (list))
    {
      val = SCM_CAR (list);
      if (!SCM_EXACTP (val) && !SCM_CHARP (val))
	{
	  scm_gc_free ((void *) bin->data, bin->size, "svz-binary-data");
	  scm_gc_free ((void *) bin, sizeof (guile_bin_t), "svz-binary");
	  scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARGn, val, "char or exact");
	}
      value = SCM_CHARP (val) ? 
	((int) SCM_CHAR (val)) : SCM_NUM2INT (SCM_ARGn, val);
      if (value < -128 || value > 255)
	{
	  scm_gc_free ((void *) bin->data, bin->size, "svz-binary-data");
	  scm_gc_free ((void *) bin, sizeof (guile_bin_t), "svz-binary");
	  SCM_OUT_OF_RANGE (SCM_ARGn, val);
	}
      *p++ = (unsigned char) value;
      list = SCM_CDR (list);
    }

  SCM_RETURN_NEWSMOB (guile_bin_tag, bin);
}
#undef FUNC_NAME

/* Checks if the given scheme cell @var{binary} is a binary smob or not.
   Returns zero if not, otherwise non-zero. */
int
guile_bin_check (SCM binary)
{
  return CHECK_BIN_SMOB (binary) ? 1 : 0;
}

/* This routine converts any given data pointer @var{data} with a
   certain @var{size} into a binary smob. This contains then a simple
   reference to the given data pointer. */
SCM
guile_data_to_bin (void *data, int size)
{
  guile_bin_t *bin;
     
  bin = MAKE_BIN_SMOB ();
  bin->size = size;
  bin->data = data;
  bin->garbage = 0;
  SCM_RETURN_NEWSMOB (guile_bin_tag, bin);
}

/* Converts the data pointer @var{data} with a size of @var{size} bytes
   into a binary smob which is marked as garbage.  This means the data 
   pointer must be allocated by @code{scm_gc_malloc()} or 
   @code{scm_gc_realloc()}. */
SCM
guile_garbage_to_bin (void *data, int size)
{
  guile_bin_t *bin;
     
  bin = MAKE_BIN_SMOB ();
  bin->size = size;
  bin->data = data;
  bin->garbage = 1;
  SCM_RETURN_NEWSMOB (guile_bin_tag, bin);
}

/* This function converts the given binary smob @var{binary} back to a
   data pointer. If the @var{size} argument is not NULL the current size
   of the binary smob will be stored there. */
void *
guile_bin_to_data (SCM binary, int *size)
{
  guile_bin_t *bin = GET_BIN_SMOB (binary);
  if (size)
    *size = bin->size;
  return bin->data;
}

/* The following macro expands to a function definition which accesses a
   binary smob's data for reading depending on the given @var{ctype}. */
#define MAKE_BIN_REF(ctype)                                                  \
static SCM GUILE_CONCAT3 (guile_bin_,ctype,_ref) (SCM binary, SCM index) {   \
  guile_bin_t *bin; int idx; long val = 0; void *data;                       \
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);                                \
  SCM_ASSERT_TYPE (SCM_EXACTP (index), index, SCM_ARG2, FUNC_NAME, "exact"); \
  idx = SCM_NUM2INT (SCM_ARG2, index);                                       \
  if (idx < 0 || idx >= (int) (bin->size / sizeof (ctype)))                  \
    SCM_OUT_OF_RANGE (SCM_ARG2, index);                                      \
  data = SVZ_NUM2PTR (SVZ_PTR2NUM (bin->data) + idx * sizeof (ctype));       \
  memcpy (&val, data, sizeof (ctype));                                       \
  return scm_long2num (val);                                                 \
}

/* Checks whether the scheme value @var{value} can be stored within a
   @var{ctype}. The macro stores valid values in @var{val} and throws an
   exception if it is out of range. */
#define CTYPE_CHECK_RANGE(ctype, value, pos, val) do {                     \
    if (SCM_POSITIVEP (value)) {                                           \
      unsigned long uval = SCM_NUM2ULONG (pos, value);                     \
      unsigned ctype cval = (unsigned ctype) uval;                         \
      if (uval != (unsigned long) cval) SCM_OUT_OF_RANGE (pos, value);     \
      val = (unsigned long) uval;                                          \
    } else {                                                               \
      long ival = SCM_NUM2LONG (pos, value);                  \
      ctype cval = (ctype) ival;                 \
      if (ival != (long) cval) SCM_OUT_OF_RANGE (pos, value); \
      val = (unsigned long) ival;                                          \
    }                                                                      \
  } while (0)

/* The following macro expands to a function definition which accesses a
   binary smob's data for writing depending on the given @var{ctype}. */
#define MAKE_BIN_SET(ctype)                                                  \
static SCM GUILE_CONCAT3 (guile_bin_,ctype,_set) (SCM binary, SCM index,     \
						  SCM value) {               \
  guile_bin_t *bin; int idx; unsigned long val; SCM old;                     \
  unsigned long oldval = 0; void *data;                                      \
  CHECK_BIN_SMOB_ARG (binary, SCM_ARG1, bin);                                \
  SCM_ASSERT_TYPE (SCM_EXACTP (index), index, SCM_ARG2, FUNC_NAME, "exact"); \
  idx = SCM_NUM2INT (SCM_ARG2, index);                                       \
  if (idx < 0 || idx >= (int) (bin->size / sizeof (ctype)))                  \
    SCM_OUT_OF_RANGE (SCM_ARG2, index);                                      \
  SCM_ASSERT_TYPE (SCM_EXACTP (value), value, SCM_ARG3, FUNC_NAME, "exact"); \
  CTYPE_CHECK_RANGE (ctype, value, SCM_ARG3, val);                           \
  data = SVZ_NUM2PTR (SVZ_PTR2NUM (bin->data) + idx * sizeof (ctype));       \
  memcpy (&oldval, data, sizeof (ctype));                                    \
  old = scm_long2num (oldval); memcpy (data, &val, sizeof (ctype));          \
  return old;                                                                \
}

/* Returns the @code{long} value of the binary smob @var{binary} at the 
   array index @var{index}. */
#define FUNC_NAME "binary-long-ref"
MAKE_BIN_REF (long)
#undef FUNC_NAME

/* Sets the @code{long} value of the binary smob @var{binary} at the array
   index @var{index} to the given value @var{value}. The procedure returns
   the previous (overridden) value. */
#define FUNC_NAME "binary-long-set!"
MAKE_BIN_SET (long)
#undef FUNC_NAME

/* Returns the @code{int} value of the binary smob @var{binary} at the 
   array index @var{index}. */
#define FUNC_NAME "binary-int-ref"
MAKE_BIN_REF (int)
#undef FUNC_NAME

/* Sets the @code{int} value of the binary smob @var{binary} at the array
   index @var{index} to the given value @var{value}. The procedure returns
   the previous (overridden) value. */
#define FUNC_NAME "binary-int-set!"
MAKE_BIN_SET (int)
#undef FUNC_NAME

/* Returns the @code{short} value of the binary smob @var{binary} at the 
   array index @var{index}. */
#define FUNC_NAME "binary-short-ref"
MAKE_BIN_REF (short)
#undef FUNC_NAME

/* Sets the @code{short} value of the binary smob @var{binary} at the array
   index @var{index} to the given value @var{value}. The procedure returns
   the previous (overridden) value. */
#define FUNC_NAME "binary-short-set!"
MAKE_BIN_SET (short)
#undef FUNC_NAME

/* Returns the @code{char} value of the binary smob @var{binary} at the 
   array index @var{index}. */
#define FUNC_NAME "binary-char-ref"
MAKE_BIN_REF (char)
#undef FUNC_NAME

/* Sets the @code{char} value of the binary smob @var{binary} at the array
   index @var{index} to the given value @var{value}. The procedure returns
   the previous (overridden) value. */
#define FUNC_NAME "binary-char-set!"
MAKE_BIN_SET (char)
#undef FUNC_NAME

/* Initialize the binary smob with all its features. Call this function
   once at application startup and before running any scheme file 
   evaluation. */
void
guile_bin_init (void)
{
#if HAVE_OLD_SMOBS
  /* Guile 1.3 backward compatibility code. */
  static scm_smobfuns guile_bin_funs = {
    NULL, guile_bin_free, guile_bin_print, guile_bin_equal };
  guile_bin_tag = scm_newsmob (&guile_bin_funs);
#else
  /* Create new smob data type. */
  guile_bin_tag = scm_make_smob_type ("svz-binary", 0);
  scm_set_smob_print (guile_bin_tag, guile_bin_print);
  scm_set_smob_free (guile_bin_tag, guile_bin_free);
  scm_set_smob_equalp (guile_bin_tag, guile_bin_equal);
#endif /* not HAVE_OLD_SMOBS */

  scm_c_define_gsubr ("binary?", 1, 0, 0, guile_bin_p);
  scm_c_define_gsubr ("string->binary", 1, 0, 0, guile_string_to_bin);
  scm_c_define_gsubr ("binary->string", 1, 0, 0, guile_bin_to_string);
  scm_c_define_gsubr ("list->binary", 1, 0, 0, guile_list_to_bin);
  scm_c_define_gsubr ("binary->list", 1, 0, 0, guile_bin_to_list);
  scm_c_define_gsubr ("binary-search", 2, 0, 0, guile_bin_search);
  scm_c_define_gsubr ("binary-set!", 3, 0, 0, guile_bin_set_x);
  scm_c_define_gsubr ("binary-ref", 2, 0, 0, guile_bin_ref);
  scm_c_define_gsubr ("binary-length", 1, 0, 0, guile_bin_length);
  scm_c_define_gsubr ("binary-concat!", 2, 0, 0, guile_bin_concat_x);
  scm_c_define_gsubr ("binary-subset", 2, 1, 0, guile_bin_subset);
  scm_c_define_gsubr ("binary-reverse", 1, 0, 0, guile_bin_reverse);
  scm_c_define_gsubr ("binary-reverse!", 1, 0, 0, guile_bin_reverse_x);

  scm_c_define_gsubr ("binary-long-ref", 2, 0, 0, guile_bin_long_ref);
  scm_c_define_gsubr ("binary-int-ref", 2, 0, 0, guile_bin_int_ref);
  scm_c_define_gsubr ("binary-short-ref", 2, 0, 0, guile_bin_short_ref);
  scm_c_define_gsubr ("binary-char-ref", 2, 0, 0, guile_bin_char_ref);
  scm_c_define_gsubr ("binary-long-set!", 3, 0, 0, guile_bin_long_set);
  scm_c_define_gsubr ("binary-int-set!", 3, 0, 0, guile_bin_int_set);
  scm_c_define_gsubr ("binary-short-set!", 3, 0, 0, guile_bin_short_set);
  scm_c_define_gsubr ("binary-char-set!", 3, 0, 0, guile_bin_char_set);
}

#else /* not ENABLE_GUILE_SERVER */

static int have_guile_bin = 0;

#endif/* ENABLE_GUILE_SERVER */
