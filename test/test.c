/*
 * test/test.c - test suite utility functions
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
 * $Id: test.c,v 1.6 2002/07/23 16:39:56 ela Exp $
 *
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* Initialize test suite. */
void
test_init (void)
{
  srand (time (NULL));
}

/* Return a random string. */
char *
test_string (void)
{
  static char text[0x101];
  int length = (rand () & 0xff) + 1;

  text[length] = '\0';
  while (length--)
    {
      text[length] = (rand () % (128 - ' ')) + ' ';
    }
  return text;
}

/* Return a random number between 0 and NR - 1. */
unsigned long
test_value (unsigned long nr)
{
  return nr ? (rand () % nr) : 0;
}

/* Print any text. */
void
test_print (char *text)
{
  fprintf (stderr, text);
  fflush (stderr);
}

/* Print an Ok. */
void
test_ok (void)
{
  fprintf (stderr, "ok\n");
  fflush (stderr);
}

/* Print a Failed. */
void
test_failed (void)
{
  fprintf (stderr, "failed\n");
  fflush (stderr);
}
