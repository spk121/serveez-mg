/* Copyright (C) 1992-1999,2001,2003,2004,2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>


#ifndef TCSASOFT
#define TCSASOFT 0
#endif
#define PWD_BUFFER_SIZE 256

char *
getpasswd (const char *prompt)
{
  FILE *in, *out;
  struct termios s, t;
  int tty_changed;
  static char buf[PWD_BUFFER_SIZE];
  int nread;


  /* Turn echoing off if it is on now.  */

  if (tcgetattr (fileno (stdin), &t) == 0)
    {
      /* Save the old one. */
      s = t;
      /* Tricky, tricky. */
      t.c_lflag &= ~(ECHO | ISIG);
      tty_changed = (tcsetattr (fileno (in), TCSAFLUSH | TCSASOFT, &t) == 0);
    }
  else
    tty_changed = 0;

  /* Write the prompt.  */
  fputs (prompt, stderr);
  fflush (stderr);

  /* Read the password.  */
  fgets (buf, PWD_BUFFER_SIZE - 1, in);
  if (buf != NULL)
    {
      nread = strlen (buf);
      if (nread < 0)
	buf[0] = '\0';
      else if (buf[nread - 1] == '\n')
	{
	  /* Remove the newline.  */
	  buf[nread - 1] = '\0';
	  if (tty_changed)
	    /* Write the newline that was not echoed.  */
	    putc ('\n', out);
	}
    }

  /* Restore the original setting.  */
  if (tty_changed)
    {
      tcsetattr (fileno (stdin), TCSAFLUSH | TCSASOFT, &s);
    }

  return buf;
}
